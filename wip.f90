program zfp_example
  use, intrinsic :: iso_c_binding
  use, intrinsic :: iso_fortran_env, only: wp => real32
  use zfp
  implicit none
  integer, parameter :: nx = 100
  integer, parameter :: ny = 100
  integer, parameter :: nz = 100
  real(wp), dimension(nz, nx, ny) :: array
  real(wp) :: x, y, z
  logical :: decompress
  integer :: i, j, k

  ! use -d to decompress rather than compress data
  decompress = .true. !command_argument_count() == 1 .and. command_argument(1) == "-d"

  ! allocate 100x100x100 array of doubles
  if (.not. decompress) then
    ! initialize array to be compressed
    do k = 1, ny
      do j = 1, nx
        do i = 1, nz
          z = 2.0d0 * real(i-1) / real(nz)
          x = 2.0d0 * real(j-1) / real(nx)
          y = 2.0d0 * real(k-1) / real(ny)
          array(i, j, k) = exp(-(x * x + y * y + z * z))
        end do
      end do
    end do
  end if

  ! compress or decompress array
  call compress(array, nx, ny, nz, 1.0d-3, decompress)

contains

  subroutine compress(m, n, l, indata, tolerance, precision, rate, parallel)
    integer, intent(in) :: m, n, l
    real(wp), intent(inout), target :: indata(m,n,l)
    real(wp), optional :: tolerance, precision, rate
    logical, optional :: parallel
    character, intent(out), allocatable, target :: buffer(:)

    type(zFORp_field) :: field
    type(zFORp_bitstream) :: bs, stream
    type(zFORp_stream) :: zfp
    type(c_ptr) :: uncompressed_ptr
    integer :: data_type

    if (.not. (present(tolerance) .or. present(precision) .or. present(rate))) stop ''
    if((present(tolerance) .and. present(precision))) stop ''
    if((present(tolerance) .and. present(rate))) stop ''
    if((present(rate) .and. present(precision))) stop ''

    field = zFORp_field_3d(uncompressed_ptr, scalar_type , nx, ny, nz)
    zfp = zFORp_stream_open(bs)
    data_type = zFORp_type_double

    if (present(tolerance)) then
      acc = zFORp_stream_set_accuracy(zfp, tolerance)
    !else if (present(precision)) then
    !        zFORp_stream_set_precision(zfp, precision)
    !else if (present(rate)) then 
    !        zFORp_stream_set_rate(zfp, rate, data_type, 3, 0)
    endif

    bufsize = zFORp_stream_maximum_size(zfp, field)
    allocate(buffer(bufsize))

    stream = zFORp_bitstream_stream_open(c_loc(buffer), bufsize)
    call zFORp_stream_set_bit_stream(zfp, stream)
    call zFORp_stream_rewind(zfp)
    zfpsize = zFORp_compress(zfp, field)

    call zFORp_field_free(field)
    call zFORp_stream_close(zfp)
    call zFORp_bitstream_stream_close(stream)
  end subroutine compress

  subroutine compress(array, nx, ny, nz, tolerance, decompress)
    real(8), intent(inout), dimension(:,:,:), target :: array
    integer, intent(in) :: nx, ny, nz
    real(8), intent(in) :: tolerance
    logical, intent(in) :: decompress
    ! ...
    type(c_ptr) :: uncompressed_ptr
    type(zFORp_field) :: field
    integer :: scalar_type, ios
    real(8) :: acc
    integer (kind=8) :: bufsize, bitstream_offset_bytes, csize, dsize, file_size
    type(zFORp_bitstream) :: stream
    type(zFORp_stream) :: zfp
    character, dimension(:), allocatable, target :: buffer
    integer, parameter :: iunit = 99, sizeofreal = 8
    type(zFORp_bitstream) :: bs
    integer, dimension(4), target :: size_arr

    size_arr(:) = 0
    scalar_type = zFORp_type_double
    uncompressed_ptr = c_loc(array)
    field = zFORp_field_3d(uncompressed_ptr, scalar_type , nx, ny, nz)

    zfp = zFORp_stream_open(bs)

    acc = zFORp_stream_set_accuracy(zfp, tolerance)
    bufsize = zFORp_stream_maximum_size(zfp, field)
    allocate(buffer(bufsize))
    print*, 'accuracy: ', acc, 'bufsize: ', bufsize

    stream = zFORp_bitstream_stream_open(c_loc(buffer), bufsize)
    call zFORp_stream_set_bit_stream(zfp, stream)
    call zFORp_stream_rewind(zfp)

    if (decompress) then
        print*, 'decompress: ', decompress, 'bufsize: ', bufsize
        ! read compressed stream and decompress and output array
        open(unit=iunit, file='compress.zfp', form='unformatted', &
             access='stream', status='old', action='read', iostat=ios)
        if (ios /= 0) stop "Error opening input file"
        !inquire(unit=iunit,recl=file_size)
        !print*,file_size
        read(iunit) buffer(1:158744)
        close(iunit)
        bitstream_offset_bytes = zFORp_decompress(zfp, field)
        dsize = zFORp_field_size(field, size_arr)
        print*, size_arr(:)
        open(unit=iunit, file='decompress.zfp', form='unformatted', &
             access='stream', status='replace', action='write', iostat=ios)
        if (ios /= 0) stop "Error opening output file"
        print*, dsize, nx*nz*ny*8
        write(iunit) array(:,:,:)
        close(iunit)    
    else
        csize = zFORp_compress(zfp, field)
        print*,'csize: ', csize
        open(unit=iunit, file='compress.zfp', form='unformatted', &
             access='stream', status='replace', action='write', iostat=ios)
        if (ios /= 0) stop "Error opening output file"
        write(iunit) buffer(1:csize)
        close(iunit)
    end if

    call zFORp_field_free(field)
    call zFORp_stream_close(zfp)
    call zFORp_bitstream_stream_close(stream)
    deallocate(buffer)

  end subroutine compress

end program zfp_example
