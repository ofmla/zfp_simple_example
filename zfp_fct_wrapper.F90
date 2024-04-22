module zfp_fct_wrapper
  use, intrinsic :: iso_c_binding, only : c_ptr, c_long, c_double, c_loc, c_sizeof
  use, intrinsic :: iso_fortran_env
  use zfp

  implicit none
  private

#ifdef REAL32
  integer, parameter, public :: zfp_fct_rk = real32   !! real kind used by this module [4 bytes]
#elif REAL64
  integer, parameter, public :: zfp_fct_rk = real64   !! real kind used by this module [8 bytes]
#else
  integer, parameter, public :: zfp_fct_rk = real64   !! real kind used by this module [8 bytes]
#endif

  integer, parameter, public :: wp = zfp_fct_rk 
  public :: compress, decompress

  contains

  subroutine compress(indata, buffer, tolerance, precision, rate, parallel)
    implicit none
    real(wp), intent(in), target :: indata(..) ! assumed rank argument
    character, intent(out), allocatable :: buffer(:)
    integer, optional :: precision
    real(c_double), optional :: tolerance, rate
    logical, optional :: parallel

    type(zFORp_field) :: fld
    type(zFORp_bitstream) :: bs, stream
    type(zFORp_stream) :: zfp
    type(c_ptr) :: in_ptr
    integer :: dtype, set_prec, is_success
    integer(c_long) :: zfpsize, bufsize
    real(c_double):: set_rate, set_tol
    integer, allocatable :: shap(:)
    logical :: parl = .true.

    if (.not. (present(tolerance) .or. present(precision) .or. present(rate))) then
        stop 'No compression parameter'
    endif
    if(present(tolerance) .and. present(precision)) stop 'only 1 parameter is allowed'
    if(present(tolerance) .and. present(rate)) stop 'only 1 parameter is allowed'
    if(present(rate) .and. present(precision)) stop 'only 1 parameter is allowed'
    if(present(parallel) .and. (parallel .eqv. .false.)) parl =.false.

    in_ptr = c_loc(indata); shap = shape(indata)
    if (c_sizeof(1._wp) .eq. 8) then
      dtype = zFORp_type_double
    else
      dtype = zFORp_type_float
    endif

    select rank(indata)
    rank(1) ; fld = zFORp_field_1d(in_ptr, dtype , shap(1))
    rank(2) ; fld = zFORp_field_2d(in_ptr, dtype , shap(1), shap(2))
    rank(3) ; fld = zFORp_field_3d(in_ptr, dtype , shap(1), shap(2), shap(3))
    rank(4) ; fld = zFORp_field_4d(in_ptr, dtype , shap(1), shap(2), shap(3), shap(4))
    rank default; stop "arg is of rank other than 1 thru' 4"
    end select

    zfp = zFORp_stream_open(bs)
    if (parl) is_success =zFORp_stream_set_execution(zfp, zFORp_exec_omp)

    if (present(tolerance)) then
     set_tol = zFORp_stream_set_accuracy(zfp, tolerance)
    else if (present(precision)) then
     set_prec = zFORp_stream_set_precision(zfp, precision)
    else if (present(rate)) then 
     set_rate = zFORp_stream_set_rate(zfp, rate, dtype, rank(indata), 0)
    endif

    bufsize = zFORp_stream_maximum_size(zfp, fld)

    block
     character, allocatable, target :: tmp(:)
     allocate(tmp(bufsize))
     stream = zFORp_bitstream_stream_open(c_loc(tmp), bufsize)
     call zFORp_stream_set_bit_stream(zfp, stream)
     call zFORp_stream_rewind(zfp)
     zfpsize = zFORp_compress(zfp, fld)
     buffer = tmp(:zfpsize)
     deallocate(tmp)
     call zFORp_bitstream_stream_close(stream)
    end block

    call zFORp_field_free(fld)
    call zFORp_stream_close(zfp)

  end subroutine compress

  subroutine decompress(buffer, outdata, tolerance, precision, rate, parallel)
    implicit none
    real(wp), intent(inout), target :: outdata(..) ! assumed rank argument
    character, intent(in), target :: buffer(:)
    integer, optional :: precision
    real(c_double), optional :: tolerance, rate
    logical, optional :: parallel

    type(zFORp_field) :: fld
    type(zFORp_bitstream) :: bs, stream
    type(zFORp_stream) :: zfp
    type(c_ptr) :: out_ptr
    integer :: dtype, set_prec, is_success
    integer(c_long) :: zfpsize, bufsize, bitstream_offset_bytes
    real(c_double):: set_rate, set_tol
    integer, allocatable :: shap(:)
    logical :: parl = .true.

    if (.not. (present(tolerance) .or. present(precision) .or. present(rate))) then
        stop 'No compression parameter'
    endif
    if(present(tolerance) .and. present(precision)) stop 'only 1 parameter is allowed'
    if(present(tolerance) .and. present(rate)) stop 'only 1 parameter is allowed'
    if(present(rate) .and. present(precision)) stop 'only 1 parameter is allowed'
    if(present(parallel) .and. (parallel .eqv. .false.)) parl =.false.

    out_ptr = c_loc(outdata); shap = shape(outdata)
    if (c_sizeof(1._wp) .eq. 8) then
      dtype = zFORp_type_double
    else
      dtype = zFORp_type_float
    endif

    select rank(outdata)
    rank(1) ; fld = zFORp_field_1d(out_ptr, dtype , shap(1))
    rank(2) ; fld = zFORp_field_2d(out_ptr, dtype , shap(1), shap(2))
    rank(3) ; fld = zFORp_field_3d(out_ptr, dtype , shap(1), shap(2), shap(3))
    rank(4) ; fld = zFORp_field_4d(out_ptr, dtype , shap(1), shap(2), shap(3), shap(4))
    rank default; stop "arg is of rank other than 1 thru' 4"
    end select

    zfp = zFORp_stream_open(bs)
    if (parl) is_success =zFORp_stream_set_execution(zfp, zFORp_exec_omp)
    if (is_success .ne. 0) stop "Parallel decompression not supported on this platform"

    if (present(tolerance)) then
     set_tol = zFORp_stream_set_accuracy(zfp, tolerance)
    else if (present(precision)) then
     set_prec = zFORp_stream_set_precision(zfp, precision)
    else if (present(rate)) then 
     set_rate = zFORp_stream_set_rate(zfp, rate, dtype, rank(outdata), 0)
    endif

    bufsize = size(buffer)
    stream = zFORp_bitstream_stream_open(c_loc(buffer),bufsize)
    call zFORp_stream_set_bit_stream(zfp, stream)
    call zFORp_stream_rewind(zfp)

    bitstream_offset_bytes = zFORp_decompress(zfp, fld)

    call zFORp_field_free(fld)
    call zFORp_stream_close(zfp)
    call zFORp_bitstream_stream_close(stream)
  end subroutine decompress
end module zfp_fct_wrapper
