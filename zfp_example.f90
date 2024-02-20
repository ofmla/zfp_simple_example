program zfp_example
  use, intrinsic :: iso_c_binding, only : c_double
  use zfp_fct_wrapper
  implicit none
  integer, parameter :: iunit=99
  integer, parameter :: nx = 100
  integer, parameter :: ny = 100
  integer, parameter :: nz = 100
  real(c_double), parameter :: tol=1.d-3
  character(:), allocatable :: arg1
  integer :: length, status, ios

  if (command_argument_count() .ne. 1) then
    stop "Error! Number of arguments should be 1:" // &
         " '-c' for compression or '-d' for decompression."
  endif
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
      open(unit=iunit, file='compressed.zfp', &
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
      open(unit=iunit, file='compressed.zfp', form='unformatted', &
           access='stream', status='old', action='read', iostat=ios)
      if (ios .ne. 0) then  
        stop "No file compress.zfp :("
      else
        inquire(file='compressed.zfp', size=filesize)  
        if (filesize .gt. 0) then 
          allocate( buffer_in(filesize) )  
          read(iunit, pos=1, iostat=ios) buffer_in  
          if (ios .ne. 0) deallocate(buffer_in)  
          close(iunit)  
        end if  
      end if
      if (allocated(buffer_in)) then
        call decompress(buffer_in, out_array, tol)
        open(unit=iunit, file='decompressed.zfp', form='unformatted', &
             access='stream', status='replace', action='write', iostat=ios)
        if (ios .ne. 0) stop "Error opening 'decompress.zfp' file"
        write(iunit) out_array(:,:,:)
        close(iunit)
      endif
    end block blk2
  endif

end program zfp_example
