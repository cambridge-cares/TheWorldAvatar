! <module_readnc.for - A component of the City-scale
!                 Chemistry Transport Model EPISODE-CityChem>
!*****************************************************************************!
!*
!*        CITY-scale CHEMistry Transport Extension
!*
!*        Copyright (C) 2018  Matthias Steffen Karl
!*
!*        Contact Information: 
!*            Institute of Coastal Research
!*            Helmholtz-Zentrum Geesthacht
!*            Max-Planck-Str. 1
!*            21502 Geesthacht
!*            Germany
!*            email:  matthias.karl@hzg.de
!*
!*      EPISODE-CityChem, developed at Helmholtz-Zentrum Geesthacht (HZG) is designed
!*      for treating complex atmospheric chemistry in urban areas (Karl, 2018). The model
!*      is an extension of the EPISODE dispersion model to enable chemistry/transport
!*      simulations of reactive pollutants on city scale. EPISODE is an Eulerian dispersion
!*      model developed at the Norwegian Institute for Air Research (NILU) appropriate for
!*      air quality studies at the local scale (Slørdal et al. 2003 &2008). The model is an
!*      open source code subject to the Reciprocal Public License ("RPL") Version 1.5,
!*      https://opensource.org/licenses/RPL-1.5.
!*
!*        Reference:
!*      Karl, M. (2018):  Development of the city-scale chemistry transport model 
!*      CityChem-EPISODE and its application to the city of Hamburg, 
!*      Geosci. Model Dev. Discuss.,
!*      https://doi.org/10.5194/gmd-2018-8, 2018.
!*
!*
!*  Unless explicitly acquired and licensed from Licensor under another license,
!*  the contents of this file are subject to the Reciprocal Public License ("RPL")
!*  Version 1.5, https://opensource.org/licenses/RPL-1.5 or subsequent versions as
!*  allowed by the RPL, and You may not copy or use this file in either source code
!*  or executable form, except in compliance with the terms and conditions of the RPL. 
!*
!*  All software distributed under the RPL is provided strictly on an "AS IS" basis, 
!*  WITHOUT WARRANTY OF ANY KIND, EITHER EXPRESS OR IMPLIED, AND LICENSOR HEREBY 
!*  DISCLAIMS ALL SUCH WARRANTIES, INCLUDING WITHOUT LIMITATION, ANY WARRANTIES OF
!*  MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE, QUIET ENJOYMENT, OR NON-INFRINGEMENT.
!*  See the RPL for specific language governing rights and limitations under the RPL.
!*
!*****************************************************************************!
!
!***********************************************************************
!***
!***      UECT
!***      Urban Emission Conversion Tool
!***
!***********************************************************************

      module module_readnc

!*****************************************************************************!
!*     Module mod_readnc contains i/o utilities for opening and
!*     reading of netCDF files in accord with CF-convention
!*****************************************************************************!

          include 'netcdf.inc'

      contains

       subroutine checkread(status,varname,fileName)

! *** The nf90 are functions which return 0 if no error occur.
! *** checkread is only a subroutine which check wether the function returns zero

          use netcdf
          implicit none
          integer, intent ( in) :: status
          character (len=*),intent(in)::varname
          character (len=*),intent(in)::fileName 

          if(status /= nf90_noerr) then 
             print *, trim(nf90_strerror(status))
             print *, "variable ",varname
             print *, "fileName ",fileName
             print *, "error in NetCDF_ml"
             call STOPIT('mod_readnc: NetCDF_ml: check status')
          end if

      end subroutine checkread


      subroutine handle_ncerr( ret, mes )
!---------------------------------------------------------------------
!	... netcdf error handling routine
!---------------------------------------------------------------------

          implicit none
!---------------------------------------------------------------------
!	... dummy arguments
!---------------------------------------------------------------------
          integer, intent(in) :: ret
          character(len=*), intent(in) :: mes

          if( ret /= nf_noerr ) then
           write(*,*) nf_strerror( ret )
           stop 'netcdf error'
          endif

      end subroutine handle_ncerr


!***************************************
! GetCDF is for READING netCDF files
!***************************************
subroutine ReadNCfile(varname,fileName,var,I_MAX,J_MAX,Z_MAX,nstart,nfetch)

! *** open and reads CDF file (written from CityChem)

! ***    varname: field to be fetched (input)
! ***    fileName:fileName (input)!***    The function nf90_def_var_deflate sets the deflate parameters for a variable in a
! ***    netCDF-4 file. It does not work on netCDF-3 files.
! ***    var: array to store field (output)
! ***    I_MAX,J_MAX,K_MAX, dimensions of "var"(input)
! ***    nstart: first field read
! ***    nfetch: number of fields read 


          use netcdf
          implicit none

          character (len=*),intent(in)   :: fileName 
          character (len=*),intent(in)   :: varname
          integer, intent(in)            :: nstart,I_MAX,J_MAX,Z_MAX

          integer, intent(inout)         ::  nfetch
          double precision, dimension(*),intent(inout) :: var   ! values(i,j,k,n) 


          integer :: GIMAX,GJMAX,KMAX,nrecords,xfelt_ident(20),period
          integer :: varID,statusn,alloc_err
          integer :: n,Nrec,ijn,ijkn
          integer :: ncFileID,iDimID,jDimID,kDimID,timeDimID
          integer :: iVarID,jVarID,kVarID,i,j,k
          integer :: var_date(9000),ndate(4)
          double precision,allocatable,dimension(:,:,:,:)  :: values
          real    :: depsum,add_offset,scale_factor
          character(len=100) ::attribute,attribute2
          character(len=256) ::fn 
          integer :: xtype,ndims,dimids(100)

!! !  Nrec=size(var,3)
           Nrec=nfetch

         !  print *,'  reading ',trim(fileName)
           fn=fileName

! *** open an existing netcdf dataset
           call checkread(nf90_open(path=trim(fileName),mode=nf90_nowrite,ncid=ncFileID),varname,trim(fn))


! *** test if the variable is defined and get varID:
           statusn = nf90_inq_varid(ncid = ncFileID, name = varname, varID = VarID)

           if(statusn == nf90_noerr) then     
           !   print *, 'variable exists: ',trim(varname)
           else
              print *, 'variable does not exist in Temperature file: ',varname,nf90_strerror(statusn)
              call STOPIT('mod_readnc: NetCDF_ml: check status')
          endif

!debug
!          print *,'ncread',ncFileID,varname,VarID,trim(fn)

! *** find dimensions id
          call checkread(nf90_Inquire_Variable(ncID=ncFileID,VarID=VarID,xtype=xtype,ndims=ndims,dimids=dimids),varname,trim(fn))

!debug
!          print *,'ncread ndims',ndims

          idimID = dimids(1)
          jdimID = dimids(2)
          if (ndims>3)  kdimID    = dimids(3)
          if (ndims>2)  timedimID = dimids(ndims)

! *** get dimensions length
         call checkread(nf90_inquire_dimension(ncid=ncFileID, dimID=idimID,  len=GIMAX),varname,trim(fn))
         call checkread(nf90_inquire_dimension(ncid=ncFileID, dimID=jdimID,  len=GJMAX),varname,trim(fn))
         KMAX=1
         if (ndims>3) then
           call checkread(nf90_inquire_dimension(ncid=ncFileID, dimID=kdimID,  len=KMAX),varname,trim(fn))
         endif
         if (ndims>2) then
           call checkread(nf90_inquire_dimension(ncid=ncFileID, dimID=timedimID,  len=nrecords),varname,trim(fn))

           if (nstart+nfetch-1>nrecords) then
              write(*,*)'WARNING: did not find all data'
              nfetch=nrecords-nstart+1
              if(nfetch <= 0) then
                  call STOPIT('mod_readnc: Could not get data from IC file')
              endif
           endif

           if (I_MAX/=GIMAX.or.J_MAX/=GJMAX.or.Z_MAX/=KMAX) then
              write(*,*)'Warning: buffer size and NetCDF sizes differ'
              write(*,*)I_MAX,GIMAX,J_MAX,GJMAX,Z_MAX,KMAX
           endif

         endif
!debug
         !print *, 'ncread sizes ',GIMAX,GJMAX,KMAX,nrecords


         if (ndims==2) then
           nfetch=1
           allocate(values(GIMAX,GJMAX,nfetch,1), stat=alloc_err)
           if ( alloc_err /= 0 ) then
             print *, 'alloc failed in ReadCDF_ml: ',alloc_err,ndims
             call STOPIT('mod_readnc: Could not allocate 2-D array (ndims=2)')
           endif  

         elseif (ndims==3) then

          !allocate a 2D array 
           allocate(values(GIMAX,GJMAX,nfetch,1), stat=alloc_err)
           if ( alloc_err /= 0 ) then
             print *, 'alloc failed in ReadCDF_ml: ',alloc_err,ndims
             call STOPIT('mod_readnc: Could not allocate 2-D array (ndims=3)')
           endif

         elseif (ndims==4) then

           !allocate a 3D array 
           allocate(values(GIMAX,GJMAX,KMAX,nfetch), stat=alloc_err)
           if ( alloc_err /= 0 ) then
             print *, 'alloc failed in ReadCDF_ml: ',alloc_err,ndims
             call STOPIT('mod_readnc: Could not allocate 3-D array (ndims=4)')
           endif

         else
           print *, 'unexpected number of dimensions: ',ndims
           call STOPIT('mod_readnc: Number of dimensions (ndims) in IC file is unexpected') 
         endif


! *** get variable attributes
         if (ndims>2) then
           call checkread(nf90_get_att(ncFileID, VarID, "add_offset", add_offset),varname,trim(fn))
           call checkread(nf90_get_att(ncFileID, VarID, "scale_factor", scale_factor),varname,trim(fn))
         endif

! *** get time variable
!!  !  call check(nf90_inq_varid(ncid = ncFileID, name = "time", varID = timeDimID))
!!  !  call check(nf90_get_var(ncFileID, timeDimID, var_date,start=(/ nstart /),count=(/ nfetch /)))

! *** get concentration variable
    !     write(*,*)'Reading record',nstart,' to ',nstart+nfetch-1
         if (ndims==2) then

           call checkread(nf90_get_var(ncFileID,VarID,values,start=(/1,1,nstart/),count=(/ GIMAX,GJMAX,nfetch /)),varname,trim(fn))

         elseif (ndims==3) then

           call checkread(nf90_get_var(ncFileID,VarID,values,start=(/1,1,nstart/),count=(/ GIMAX,GJMAX,nfetch /)),varname,trim(fn))

         elseif (ndims==4) then
           call checkread(nf90_get_var(ncFileID, VarID, values,start=(/1,1,1,nstart/), &
                           count=(/GIMAX,GJMAX,KMAX,nfetch /)),varname,trim(fn))
        endif

!! !  if(Nrec<nrecords)then
!! !  endif
!! !  write(*,*)'date start '
!! !  call datefromsecondssince1970(ndate,var_date(1))
!! !  write(*,*)'date end '
!! !  call datefromsecondssince1970(ndate,var_date(nfetch))
!! !  period=(var_date(2)-var_date(1))/3600.


        if ((ndims==3).or.(ndims==2)) then

          do n=1,nfetch
            do j=1,min(GJMAX,J_MAX)
              do i=1,min(GIMAX,I_MAX)

                ijn=i+(j-1)*I_MAX+(n-1)*I_MAX*J_MAX  
                var(ijn)=values(i,j,n,1)  !*scale_factor+add_offset  !check that one

              enddo
            enddo
         enddo

        else  !ndims==4)

          do n=1,nfetch
            do k=1,min(KMAX,Z_MAX)
              do j=1,min(GJMAX,J_MAX)
                do i=1,min(GIMAX,I_MAX)

                  ijkn=i+(j-1)*I_MAX+(k-1)*I_MAX*J_MAX+(n-1)*I_MAX*J_MAX*Z_MAX
                  var(ijkn)=values(i,j,k,n)   !*scale_factor+add_offset !check that one

                enddo
              enddo
            enddo
          enddo

        endif

! *** close file
        call checkread(nf90_close(ncFileID),varname,trim(fn))


  end subroutine ReadNCfile



      end module module_readnc


