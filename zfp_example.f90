program zfp_example
  use, intrinsic :: iso_c_binding
  use, intrinsic :: iso_fortran_env, only: wp => real64
  use zfp
  implicit none
  integer, parameter :: iunit=99
  integer, parameter :: nx = 100
  integer, parameter :: ny = 100
  integer, parameter :: nz = 100
  real(wp), parameter :: tol=1.d-3
  character(:), allocatable :: arg1
  integer :: length, status, ios

  if (command_argument_count() .ne. 1) stop "error! num. of arguments should be 1: '-c' or '-d'"
  call get_command_argument(1, length = length, status = status)
  if (status .eq. 0) then
    allocate (character(length) :: arg1)
    call get_command_argument(1, arg1, status = status)
  end if

  ! use -d to decompress rather than compress data
  if (arg1 .eq. '-c') then
    blk1: block
      real(wp), dimension (:,:,:), allocatable :: in_array
      character, allocatable :: buffer_out(:)
      real(wp) :: x, y, z
      integer :: i, j, k
      ! allocate 100x100x100 array of doubles
      allocate (in_array(nz, nx, ny))
      ! initialize array to be compressed
      do k = 1, ny
        do j = 1, nx
          do i = 1, nz
            z = 2.0_wp * real(i-1) / real(nz)
            x = 2.0_wp * real(j-1) / real(nx)
            y = 2.0_wp * real(k-1) / real(ny)
            in_array(i, j, k) = exp(-(x * x + y * y + z * z))
          end do
        end do
      end do
      ! compress 
      call compress(in_array, buffer_out, tol)
      open(unit=iunit, file='compress.zfp', &
           form='unformatted', access='stream', status='replace', &
           action='write', iostat=ios)
      if (ios.ne. 0) stop "Error opening output file"
      write(iunit) buffer_out(:)
      close(iunit)
    end block blk1
  else if (arg1 .eq. '-d') then
    blk2: block
      real(wp), dimension (:,:,:), allocatable :: out_array
      character, allocatable :: buffer_in(:)
      integer :: filesize
      ! decompress
      allocate (out_array(nz, nx, ny))
      open(unit=iunit, file='compress.zfp', form='unformatted', &
           access='stream', status='old', action='read', iostat=ios)
      if (ios .ne. 0) then  
        stop "No file compress.zfp :("
      else
        inquire(file='compress.zfp', size=filesize)  
        if (filesize .gt. 0) then 
          allocate( buffer_in(filesize) )  
          read(iunit, pos=1, iostat=ios) buffer_in  
          if (ios .ne. 0) deallocate(buffer_in)  
          close(iunit)  
        end if  
      end if
      if (allocated(buffer_in)) then
        call decompress(buffer_in, out_array, tol)
        open(unit=iunit, file='decompress.zfp', form='unformatted', &
             access='stream', status='replace', action='write', iostat=ios)
        if (ios .ne. 0) stop "Error opening 'decompress.zfp' file"
        write(iunit) out_array(:,:,:)
        close(iunit)
      endif
    end block blk2
  endif

contains

  subroutine compress(indata, buffer, tolerance, precision, rate, parallel)
    implicit none
    real(wp), intent(in), target :: indata(..) ! assumed rank argument
    integer, optional :: precision
    real(c_double), optional :: tolerance, rate
    logical, optional :: parallel
    character, intent(out), allocatable :: buffer(:)

    type(zFORp_field) :: field
    type(zFORp_bitstream) :: bs, stream
    type(zFORp_stream) :: zfp
    type(c_ptr) :: in_ptr
    integer :: dtype, set_prec, is_success
    integer(c_long) :: zfpsize, bufsize
    real(c_double):: set_rate, set_tol
    integer, allocatable :: ashape(:)
    logical :: parallel_=.true.

    if(.not. (present(tolerance) .or. present(precision) .or. present(rate))) stop 'No compression parameter'
    if((present(tolerance) .and. present(precision))) stop 'only one parameter is allowed'
    if((present(tolerance) .and. present(rate))) stop 'only one parameter is allowed'
    if((present(rate) .and. present(precision))) stop 'only one parameter is allowed'
    if(present(parallel) .and. (parallel .eq. .false.)) parallel_ =.false.

    in_ptr = c_loc(indata); ashape = shape(indata)
    if (c_sizeof(1._wp) .eq. 8) then
      dtype = zFORp_type_double
    else
      dtype = zFORp_type_float
    endif

    select rank(indata)
    rank(1) ; field = zFORp_field_1d(in_ptr, dtype , ashape(1))
    rank(2) ; field = zFORp_field_2d(in_ptr, dtype , ashape(1), ashape(2))
    rank(3) ; field = zFORp_field_3d(in_ptr, dtype , ashape(1), ashape(2), ashape(3))
    rank(4) ; field = zFORp_field_4d(in_ptr, dtype , ashape(1), ashape(2), ashape(3), ashape(4))
    rank default; stop "arg is of rank other than 1 thru' 4"
    end select

    zfp = zFORp_stream_open(bs)
    if (parallel_) is_success =zFORp_stream_set_execution(zfp, zFORp_exec_omp)

    if (present(tolerance)) then
     set_tol = zFORp_stream_set_accuracy(zfp, tolerance)
    else if (present(precision)) then
     set_prec = zFORp_stream_set_precision(zfp, precision)
    else if (present(rate)) then 
     set_rate = zFORp_stream_set_rate(zfp, rate, dtype, rank(indata), 0)
    endif

    bufsize = zFORp_stream_maximum_size(zfp, field)

    block
     character, allocatable, target :: tmp(:)
     allocate(tmp(bufsize))
     stream = zFORp_bitstream_stream_open(c_loc(tmp), bufsize)
     call zFORp_stream_set_bit_stream(zfp, stream)
     call zFORp_stream_rewind(zfp)
     zfpsize = zFORp_compress(zfp, field)
     buffer = tmp(:zfpsize)
     deallocate(tmp)
     call zFORp_bitstream_stream_close(stream)
    end block

    call zFORp_field_free(field)
    call zFORp_stream_close(zfp)

  end subroutine compress

  subroutine decompress(buffer, outdata, tolerance, precision, rate, parallel)
    implicit none
    real(wp), intent(inout), target :: outdata(..) ! assumed rank argument
    integer, optional :: precision
    real(c_double), optional :: tolerance, rate
    logical, optional :: parallel
    character, dimension(:), intent(in), target :: buffer

    type(zFORp_field) :: field
    type(zFORp_bitstream) :: bs, stream
    type(zFORp_stream) :: zfp
    type(c_ptr) :: out_ptr
    integer :: dtype, set_prec, is_success
    integer(c_long) :: zfpsize, bufsize, bitstream_offset_bytes
    real(c_double):: set_rate, set_tol
    integer, allocatable :: ashape(:)
    logical :: parallel_=.true.


    if(.not. (present(tolerance) .or. present(precision) .or. present(rate))) stop 'No compression parameter'
    if((present(tolerance) .and. present(precision))) stop 'only one parameter is allowed'
    if((present(tolerance) .and. present(rate))) stop 'only one parameter is allowed'
    if((present(rate) .and. present(precision))) stop 'only one parameter is allowed'
    if(present(parallel) .and. (parallel .eq. .false.)) parallel_ =.false.

    out_ptr = c_loc(outdata); ashape = shape(outdata)
    if (c_sizeof(1._wp) .eq. 8) then
      dtype = zFORp_type_double
    else
      dtype = zFORp_type_float
    endif

    select rank(outdata)
    rank(1) ; field = zFORp_field_1d(out_ptr, dtype , ashape(1))
    rank(2) ; field = zFORp_field_2d(out_ptr, dtype , ashape(1), ashape(2))
    rank(3) ; field = zFORp_field_3d(out_ptr, dtype , ashape(1), ashape(2), ashape(3))
    rank(4) ; field = zFORp_field_4d(out_ptr, dtype , ashape(1), ashape(2), ashape(3), ashape(4))
    rank default; stop "arg is of rank other than 1 thru' 4"
    end select

    zfp = zFORp_stream_open(bs)
    if (parallel_) is_success =zFORp_stream_set_execution(zfp, zFORp_exec_omp)
    if (is_success .ne. 0) stop "Parallel decompression not supported on this platform"

    bufsize = size(buffer)
    stream = zFORp_bitstream_stream_open(c_loc(buffer),bufsize)
    call zFORp_stream_set_bit_stream(zfp, stream)
    call zFORp_stream_rewind(zfp)

    bitstream_offset_bytes = zFORp_decompress(zfp, field)

    call zFORp_field_free(field)
    call zFORp_stream_close(zfp)
    call zFORp_bitstream_stream_close(stream)
  end subroutine decompress

end program zfp_example
