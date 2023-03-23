program main
    implicit none
    call example1()

contains

    subroutine read_3d(no,u,nx,ny,nz)
        integer :: no, nx, ny, nz, k
        real(kind=8), dimension(nx, ny, nz) :: u
        real(kind=8), dimension(nx,ny) :: u2d
        do k=1,nz
            read(no) u2d
            u(:,:,k)=u2d
        enddo
    end

    subroutine example1()
        ! use tecplot module
        use tecplot

        ! define a tecplot object
        type(tecplot_time_file) :: plt_file
        integer,allocatable :: locations(:)
        integer,allocatable :: type_list(:)
        integer,allocatable :: shared_list(:)

        !===================== USER DEFINED ======================================
        integer,parameter :: num_of_variables = 5
        !===================== USER DEFINED ======================================

        integer :: nx,ny,nz,k
        integer :: nxx,nyy,nzz
        integer :: xx1,yy1,zz1
        integer :: xx2,yy2,zz2
        character(len=50) :: filename1,filename2,filename3,filename4
        real(kind=4),allocatable :: your_datas(:,:,:,:)
        !===================== USER DEFINED ======================================
        real(kind=8),allocatable :: xx3d(:,:,:), yy3d(:,:,:), zz3d(:,:,:), Q(:,:,:),d(:,:,:),u(:,:,:)
        !===================== USER DEFINED ======================================

        real(kind=4) :: physics_time = 1.0

        ! set dimensions
        !===================== USER DEFINED ======================================

        open(33,file="./read_Q.in")
        do k=1,2
            read(33,*)
        enddo
        read(33,*) nx,ny,nz
        read(33,*)
        read(33,*) xx1,yy1,zz1
        read(33,*)
        read(33,*) xx2,yy2,zz2
        read(33,*)
        read(33,*) filename1
	  read(33,*)
        read(33,*) filename2
	  read(33,*)
        read(33,*) filename3
        read(33,*)
        read(33,*) filename4
        close(33)

        nxx = xx2 - xx1  + 1
        nyy = yy2 - yy1  + 1
        nzz = zz2 - zz1  + 1

        print*, "nx , ny , nz ",nx, ny, nz
        print*, "xx1, yy1, zz1",xx1, yy1, zz1
        print*, "xx2, yy2, zz2",xx2, yy2, zz2
        print*, "nxx, nyy, nzz",nxx, nyy, nzz

        !===================== USER DEFINED ======================================

        allocate(your_datas(nxx,nyy,nzz,num_of_variables))
        allocate(locations(num_of_variables))
        allocate(type_list(num_of_variables))
        allocate(shared_list(num_of_variables))
        allocate(xx3d(nx,ny,nz), yy3d(nx,ny,nz), zz3d(nx,ny,nz),Q(nx,ny,nz),d(nx,ny,nz),u(nx,ny,nz))

        ! read data file
        print*, "Read grid file ......"
        open(56,file=filename2,form='unformatted')
        call read_3d(56,xx3d,nx,ny,nz)
        call read_3d(56,yy3d,nx,ny,nz)
        call read_3d(56,zz3d,nx,ny,nz)
        close(56)
        print*, 'read grid file ok'

        !===================== USER DEFINED ======================================
        print*, 'read data file ...'
        open(70,file=filename3,form='unformatted')
        print*, 'read Q ...'
        call read_3d(70,Q,nx,ny,nz)
        close(70)
        print*, 'read Q data ok'

        open(70,file=filename4,form='unformatted')
        print*, 'read flow data ...'
        read(70)
        call read_3d(70,d,nx,ny,nz)
        call read_3d(70,u,nx,ny,nz)
        close(70)
        print*, 'read flow data ok'
        !===================== USER DEFINED ======================================



        ! locations = 0 means data in node, 1 means data in cell(not supported yet)
        locations = 0
        ! shared_list(i)=-1 means the i-th data is not shared in this zone. If shared_list(i)=m,
        ! it means the i-th data is shared with zone m in this file
        shared_list = -1
        ! type_list(i) = 1 means the i-th data is of type float. (Other data type not supported yet.)
        type_list = 1

        ! call init subroutine first
        ! nx, ny, nz means the dimension of the data
        ! 'x,y,z,u,v,w' is a string contains names of variables, must be divided by ','
        call plt_file%init(filename1,nxx,nyy,nzz,'Tecplot File Title','x,y,z,Q,u')


        ! for each zone, call the two subroutines
        ! physics_time can be any value, it will only be used when there are more than 1 zone in a file.
        call plt_file%write_zone_header('zone name', physics_time, 0, locations)

        ! your_datas(:,:,:,1:3) =  x,y,z coordinates(Variable assignment is omitted in this example)
        ! your_datas(:,:,:,4:6) =  u,v,w datas (Variable assignment is omitted in this example)
        ! ALL datas are stored in sequence like (((x(ix,iy,iz),ix=1,nx),iy=1,ny),iz=1,nz)
        ! set coordinate
        your_datas(:,:,:,1) = xx3d(xx1:xx2, yy1:yy2, zz1:zz2)
        your_datas(:,:,:,2) = yy3d(xx1:xx2, yy1:yy2, zz1:zz2)
        your_datas(:,:,:,3) = zz3d(xx1:xx2, yy1:yy2, zz1:zz2)

        ! set value
        !===================== USER DEFINED ======================================
        your_datas(:,:,:,4) = Q(xx1:xx2, yy1:yy2, zz1:zz2)
        your_datas(:,:,:,5) = u(xx1:xx2, yy1:yy2, zz1:zz2)
        !===================== USER DEFINED ======================================


        call plt_file%write_zone_data(type_list, shared_list, your_datas)

        ! before exit, you must call complete subroutine
        call plt_file%complete

    end subroutine example1

end program main
