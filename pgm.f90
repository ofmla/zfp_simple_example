program pgm
    use, intrinsic :: iso_c_binding, only : c_signed_char, c_int32_t, c_loc, c_double, c_long, c_int32_t
    use zfp
    implicit none
    integer :: ios, pos, i
    integer, parameter :: iunit = 99
    character(len=200) :: line
    integer :: ilen, fieldsep, headcount, nz, nx, maxgray
    integer(c_signed_char), allocatable :: image(:)
    type(zFORp_field) :: field
    type(zFORp_bitstream) :: bs, stream
    type(zFORp_stream) :: zfps
    real(c_double):: set_rate, rate
    integer(c_long) :: bytes, zfpsize
    character, allocatable, target :: buffer(:), tmp(:)
    integer(c_int32_t), target :: ublock(512*512), decom_ublock(512*512)
    
    ! http://www.eecs.umich.edu/courses/eecs651/w03/sources/images/lena512x512.pgm
    
    ! read pgm header 
    open(unit=iunit, file='lena512x512.pgm', access='stream', &
         form='formatted', iostat=ios) 
    if(ios .ne. 0) stop "Error opening file lena512x512.pgm"

    headcount=0
    do
     read(iunit,'(a)', iostat=ios) line
     if (ios .ne. 0) stop 'error opening image'
     if (line(1:1) == '#') cycle
     headcount= headcount+1
     if (headcount .eq. 1)then
      if(line(1:2) .ne. "P5") stop 'error opening image'
     else if (headcount .eq. 2) then
      ilen=len_trim(line)
      fieldsep = index(line, " ")
      read(line(:fieldsep),'(i10)') nz       ! rows
      read(line(fieldsep+1:ilen),'(i10)') nx ! columns
     else
      read (line,'(i10)') maxgray
      if (maxgray .gt. 255) stop 'error opening image'
     endif
     if (headcount .eq. 3) exit
    end do
    
    inquire(iunit, pos= pos)
    close(iunit)
   
    if (mod(nz,4) .ne. 0 .or. mod(nx,4) .ne. 0) then
     stop 'image dimensions must be multiples of four'
    endif
    
    open(unit=iunit, file='lena512x512.pgm', access='stream', &
         form='unformatted', iostat=ios)
    if (ios .ne. 0) stop "Error opening file lena512x512.pgm"
    
    ! read image data
    allocate(image(nz*nx))
    read(iunit, pos=pos, iostat=ios) image(:)
    close(iunit)
    print*, image(1:12)

    ! create input array
    do i=1,nz*nx
     if (image(i) .lt. 0) then
      ublock(i) = (int(image(i), kind=kind(c_int32_t)) + 128) * (2**23)
     !else
     ! ublock(i) = (int(image(i), kind=kind(c_int32_t))) * (2**23)
     endif
    enddo
    print*, ublock(1:12)
    field = zFORp_field_2d(c_loc(ublock), zFORp_type_int32, nz, nx)
    
    ! initialize compressed stream
    zfps = zFORp_stream_open(bs)
    rate = 5.d0
    set_rate = zFORp_stream_set_rate(zfps, rate, zFORp_type_int32, 2, 0)
    bytes = zFORp_stream_maximum_size(zfps, field)
    allocate(buffer(bytes))
    stream = zFORp_bitstream_stream_open(c_loc(buffer), bytes)
    call zFORp_stream_set_bit_stream(zfps, stream)
    !call zFORp_field_free(field)
    
    call zFORp_stream_rewind(zfps)
    zfpsize = zFORp_compress(zfps, field)
    print*, zfpsize, 'compressed bytes'
    
    ! decompress
    call zFORp_stream_rewind(zfps)
    call zFORp_field_set_pointer(field, c_loc(decom_ublock))
    zfpsize = zFORp_decompress(zfps, field)
    
    do i=1,nz*nx
     decom_ublock(i) = max(-128, min(ublock(i) / ishft(1, 23), 127)) + 128
    enddo
    print*, decom_ublock(1:12)
    open(unit=iunit, file='output.pgm', form='unformatted', access='stream', &
         status='replace', action='write', iostat=ios)
    if (ios .ne. 0) stop "Error opening file output.pgm"
    write(iunit) 'P5', char(10)
    write(line, '(i0, a, i0)')  nz,' ',nx
    write(iunit) trim(line), char(10)
    write(line, '(i0)') 255
    write(iunit) trim(line), char(10)
    write(iunit) char(decom_ublock(:))
    close(iunit)
    
    call zFORp_bitstream_stream_close(stream)
    call zFORp_field_free(field)
    call zFORp_stream_close(zfps)
    
    contains
    
    subroutine zfp_promote_uint8_to_int32(oblock, iblock, dims)
     integer, intent(out) :: oblock(:)
     integer, intent(in) :: iblock(:)
     integer, intent(in) :: dims
     integer :: icount, i
  
     icount = 2** (2 * dims)
     do i = 1, icount
      oblock(i) = (iblock(i) - 128) * (2**23)
     end do
    end subroutine zfp_promote_uint8_to_int32

 
end program pgm
