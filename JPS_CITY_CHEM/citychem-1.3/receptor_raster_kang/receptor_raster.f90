      Program receptor_raster

      implicit none

      Character(50) simid, name_mp !! name of simulation case, stations (name_mp)
      character(10) name_var  
      integer nx, ny, nmp, idnum_rec  !! number of subgrid cells, monitor stations, and receptor # (=total number of stations+1)
      double precision x_mp,y_mp,y_rec,x_rec  !! (x,y,z) of receptor points and monitor stations
      double precision z_mp, z_rec
      double precision dx,dy   !! grid size (m) of receptor subgrid (usually 100m)
      double precision xsw_main,ysw_main  !! sw corner of main grid
      integer rcmax !! max distance of a source to the receptor point, set as -9900 (missing)
      integer i,j
      
      write(*,*) "Generate receptor raster file" 
      
      open(unit=10,file="./receptor_input.txt")  !! open input file to define receptor raster geometry
      read(10,10) name_var, simid
      write(*,10) name_var, simid
      read(10,20) name_var, nmp
      write(*,20) name_var, nmp
      read(10,20) name_var, nx
      write(*,20) name_var, nx
      read(10,20) name_var, ny
      write(*,20) name_var, ny
      read(10,30) name_var, dx    
      write(*,30) name_var, dx  
      read(10,30) name_var, dy
      write(*,30) name_var, dy
      read(10,30) name_var, z_rec  !! height of receptor (same for all receptors)
      write(*,30) name_var, z_rec
      read(10,30) name_var, xsw_main  !! south-west corner of main grid (UTM, @m)
      write(*,30) name_var, xsw_main
      read(10,30) name_var, ysw_main  !! south-west corner of main grid (UTM, @m)
      write(*,30) name_var, ysw_main
      read(10,20) name_var, rcmax  !! max distance of a source to the receptor point, set as -9900 (missing)
      write(*,20) name_var, rcmax
 

      open(unit=20,file="../INPUT/receptor_stations_raster.txt")

      write(20,11) simid
      write(20,12) dx,dy
      write(20,13) 
      
      ! KFL: The following codes are not working, set nstation = 0
      nmp = 0
      if (nmp .ge. 1) then    !!! reading the monitor station information 
      do i=1,nmp
         read (10,60) name_var, x_mp,y_mp,z_mp,name_mp
         write(20,61) simid,x_mp,y_mp,z_mp,rcmax,name_mp
      enddo
      endif
      
      idnum_rec=nmp 
 
      if (nx .ge. 1 .and. ny .ge. 1) then
      do j=1,ny
!         write(*,*) "calculate receptor location for", j 
         if(j .eq. 1) y_rec=ysw_main+0.5*dy
         if(j .gt. 1) y_rec=y_rec+dy
         do i=1,nx
            if(i .eq. 1) x_rec=xsw_main+0.5*dx
            if(i .gt. 1) x_rec=x_rec+dx
            idnum_rec=idnum_rec+1

            write (20,62) simid, x_rec,y_rec,z_rec,rcmax,idnum_rec
         enddo
      enddo
      endif

      close (10)
      close (20)
         


   10 format(a10,a50)
   11 format(a50)  
   12 format('* Dummy Receptor points for ',f8.1,' x ',f8.1,'m2 raster grid')  
   13 format('*                                   GUID         X         Y       Z   NUMPERSONS NAME')
   20 format(a10,i8)  
   30 format(a10,f15.1)  !!! for reading location values  
   60 format(a10,2f10.1,f8.1,a50) !! reading station information
   61 format(a40,2f10.1,f8.1,i8,2X, a50) !! reading station information
   62 format(a40,2f10.1,f8.1,2i8) !! reading station information

      end program
