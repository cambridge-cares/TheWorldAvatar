! <mod_readnc.f90 - A component of the City-scale
!                 Chemistry Transport Model EPISODE-CityChem>
!*****************************************************************************! 
!* 
!* EPISODE - An urban-scale air quality model
!* ========================================== 
!* Copyright (C) 2018  NILU - Norwegian Institute for Air Research
!*                     Instituttveien 18
!*                     PO Box 100
!*                     NO-2027 Kjeller
!*                     Norway
!*
!*                     Contact persons: Gabriela Sousa Santos - gss@nilu.no
!*                                      Paul Hamer - pdh@nilu.no
!*
!* Unless explicitly acquired and licensed from Licensor under another license,
!* the contents of this file are subject to the Reciprocal Public License ("RPL")
!* Version 1.5, https://opensource.org/licenses/RPL-1.5 or subsequent versions as
!* allowed by the RPL, and You may not copy or use this file in either source code
!* or executable form, except in compliance with the terms and conditions of the RPL. 
!*
!* All software distributed under the RPL is provided strictly on an "AS IS" basis, 
!* WITHOUT WARRANTY OF ANY KIND, EITHER EXPRESS OR IMPLIED, AND LICENSOR HEREBY 
!* DISCLAIMS ALL SUCH WARRANTIES, INCLUDING WITHOUT LIMITATION, ANY WARRANTIES OF
!* MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE, QUIET ENJOYMENT, OR NON-INFRINGEMENT.
!* See the RPL for specific language governing rights and limitations under the RPL.
!*
!* ========================================== 
!* The dispersion model EPISODE (Grønskei et. al., 1993; Larssen et al., 1994;
!* Walker et al., 1992, 1999; Slørdal et al., 2003, 2008) is an Eulerian grid model 
!* with embedded subgrid models for calculations of pollutant concentrations resulting 
!* from different types of sources (area-, line- and point sources). EPISODE solves the 
!* time dependent advection/-diffusion equation on a 3 dimensional grid. 
!* Finite difference numerical methods are applied to integrate the solution forward in time. 
!* It also includes extensions as the implementation of a simplified EMEP photochemistry 
!* scheme for urban areas (Walker et al. 2004) and a scheme for Secondary Organic Aerosol 
!* implemented by Håvard Slørdal
!*
!* Grønskei, K.E., Walker, S.E., Gram, F. (1993) Evaluation of a model for hourly spatial
!*    concentrations distributions. Atmos. Environ., 27B, 105-120.
!* Larssen, S., Grønskei, K.E., Gram, F., Hagen, L.O., Walker, S.E. (1994) Verification of 
!*    urban scale time-dependent dispersion model with sub-grid elements in Oslo, Norway. 
!*    In: Air poll. modelling and its appl. X. New York, Plenum Press.
!* Slørdal, L.H., McInnes, H., Krognes, T. (2008): The Air Quality Information System AirQUIS. 
!*    Info. Techn. Environ. Eng., 1, 40-47, 2008.
!* Slørdal, L.H., Walker, S.-E., Solberg, S. (2003) The Urban Air Dispersion Model EPISODE 
!*    applied in AirQUIS. Technical Description. NILU TR 12/2003. ISBN 82-425-1522-0.
!* Walker, S.E., Grønskei, K.E. (1992) Spredningsberegninger for on-line overvåking i Grenland. 
!*    Programbeskrivelse og brukerveiledning. Lillestrøm, 
!*    Norwegian Institute for Air Research (NILU OR 55/92).
!* Walker, S.E., Slørdal, L.H., Guerreiro, C., Gram, F., Grønskei, K.E. (1999) Air pollution 
!*    exposure monitoring and estimation. Part II. Model evaluation and population exposure. 
!*    J. Environ. Monit, 1, 321-326.
!* Walker, S.-E., Solberg, S., Denby, B. (2003) Development and implementation of a 
!*    simplified EMEP photochemistry scheme for urban areas in EPISODE. NILU TR 13/2013. 
!*    ISBN 82-425-1524-7
!* ========================================== 
!*
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
!*****************************************************************************! 

      module mod_readnc

!*****************************************************************************!
!*     Module mod_readnc contains i/o utilities for opening and
!*     reading of netCDF files in accord with CF-convention
!*****************************************************************************!
!       2017 Matthias Karl, HZG, This is an original CityChem subroutine
! ----------------------------------------------------------------------------------
! REVISION HISTORY
!
!    xx Dat 201x  Name: Line  Description of Change
!
! ----------------------------------------------------------------------------------

      contains

       subroutine checkread(status,varname)

! *** The nf90 are functions which return 0 if no error occur.
! *** checkread is only a subroutine which check wether the function returns zero

          use netcdf
          implicit none
          integer, intent ( in) :: status
          character (len=*),intent(in)::varname

          if(status /= nf90_noerr) then 
             print *, trim(nf90_strerror(status))
             print *, "variable ",varname
             print *, "error in NetCDF_ml"
             call STOPIT('mod_readnc: NetCDF_ml: check status')
          end if

      end subroutine checkread



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
          integer :: xtype,ndims,dimids(100)

!! !  Nrec=size(var,3)
           Nrec=nfetch

           print *,'  reading ',trim(fileName)

! *** open an existing netcdf dataset
           call checkread(nf90_open(path=trim(fileName),mode=nf90_nowrite,ncid=ncFileID),varname)


! *** test if the variable is defined and get varID:
           statusn = nf90_inq_varid(ncid = ncFileID, name = varname, varID = VarID)

           if(statusn == nf90_noerr) then     
              print *, 'variable exists: ',trim(varname)
           else
              print *, 'variable does not exist in IC file: ',varname,nf90_strerror(statusn)
              call STOPIT('mod_readnc: NetCDF_ml: check status')
          endif

! *** find dimensions id
          call checkread(nf90_Inquire_Variable(ncID=ncFileID,VarID=VarID,xtype=xtype,ndims=ndims,dimids=dimids),varname)

          idimID = dimids(1)
          jdimID = dimids(2)
          if (ndims>3)  kdimID    = dimids(3)
          if (ndims>2)  timedimID = dimids(ndims)

! *** get dimensions length
         call checkread(nf90_inquire_dimension(ncid=ncFileID, dimID=idimID,  len=GIMAX),varname)
         call checkread(nf90_inquire_dimension(ncid=ncFileID, dimID=jdimID,  len=GJMAX),varname)
         KMAX=1
         if (ndims>3) then
           call checkread(nf90_inquire_dimension(ncid=ncFileID, dimID=kdimID,  len=KMAX),varname)
         endif
         call checkread(nf90_inquire_dimension(ncid=ncFileID, dimID=timedimID,  len=nrecords),varname)

!debug
!         print *, 'mod_readnc: sizes ',GIMAX,GJMAX,KMAX,nrecords


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

         if (ndims==3) then

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
         call checkread(nf90_get_att(ncFileID, VarID, "add_offset", add_offset),varname)
         call checkread(nf90_get_att(ncFileID, VarID, "scale_factor", scale_factor),varname)


! *** get time variable
!!  !  call check(nf90_inq_varid(ncid = ncFileID, name = "time", varID = timeDimID))
!!  !  call check(nf90_get_var(ncFileID, timeDimID, var_date,start=(/ nstart /),count=(/ nfetch /)))

! *** get concentration variable
    !     write(*,*)'Reading record',nstart,' to ',nstart+nfetch-1
         if (ndims==3) then

           call checkread(nf90_get_var(ncFileID,VarID,values,start=(/1,1,nstart/),count=(/ GIMAX,GJMAX,nfetch /)),varname)

         elseif (ndims==4) then
           call checkread(nf90_get_var(ncFileID, VarID, values,start=(/1,1,1,nstart/), &
                           count=(/GIMAX,GJMAX,KMAX,nfetch /)),varname)
        endif

!! !  if(Nrec<nrecords)then
!! !  endif
!! !  write(*,*)'date start '
!! !  call datefromsecondssince1970(ndate,var_date(1))
!! !  write(*,*)'date end '
!! !  call datefromsecondssince1970(ndate,var_date(nfetch))
!! !  period=(var_date(2)-var_date(1))/3600.


        if (ndims==3) then

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


!!!!!!!!!!!!!!!!!!! for testing 3D concentration output file by Kang @ Dec.2, 2019
!             if(trim(varname) .eq. "NO2") then
!               open(unit=1115,file="../OUTPUT/3D_field_readnc.txt")  !!
!                open(unit=1116,file="../OUTPUT/3D_field_para_readnc.txt")  !!          
!                do 351 k=1,Z_MAX
!                    do 353 j = 1,J_MAX

!                      write(1115,*) (values(i,j,k,1),i=1,I_MAX)
!  353               continue
!  351           continue
!  350          continue

!                 write(1116,*)"read restart nc file, input: I_MAX=",I_MAX,", J_MAX=", &
!                              J_MAX, ", Z_MAX=",Z_MAX, "nfetch=",nfetch
!                write(1116,*)"read in nc file: varname=",varname,', trim(varname)=',&
!                                trim(varname), ", idimID=",idimID, ", jdimID=",jdimID,&
!                       ", kdimID=",kdimID,", timedimID=",timedimID,", GIMAX=", GIMAX, &
!                        ", GJMAX=",GJMAX, ", KMAX=",KMAX,", nrecords=", nrecords,", filename=",filename
               
!                close(1115)
!                close(1116)
!            endif
!!!!!!!!!!!!!!!!!!! end for testing 3D concentration output file by Kang @ Dec.2, 2019



! *** close file
        call checkread(nf90_close(ncFileID),varname)


  end subroutine ReadNCfile






!***************************************
! GetCDF is for READING netCDF files
!***************************************
subroutine ReadNCfile_kang(varname,fileName,var,I_MAX,J_MAX,Z_MAX,nstart,nfetch)

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
          integer :: xtype,ndims,dimids(100)

!! !  Nrec=size(var,3)
           Nrec=nfetch

           print *,'  reading ',trim(fileName)

! *** open an existing netcdf dataset
           call checkread(nf90_open(path=trim(fileName),mode=nf90_nowrite,ncid=ncFileID),varname)


! *** test if the variable is defined and get varID:
           statusn = nf90_inq_varid(ncid = ncFileID, name = varname, varID = VarID)

           if(statusn == nf90_noerr) then     
              print *, 'variable exists: ',trim(varname)
           else
              print *, 'variable does not exist in IC file: ',varname,nf90_strerror(statusn)
              call STOPIT('mod_readnc: NetCDF_ml: check status')
          endif

! *** find dimensions id
          call checkread(nf90_Inquire_Variable(ncID=ncFileID,VarID=VarID,xtype=xtype,ndims=ndims,dimids=dimids),varname)

          idimID = dimids(1)
          jdimID = dimids(2)
          if (ndims>3)  kdimID    = dimids(3)
          if (ndims>2)  timedimID = dimids(ndims)

! *** get dimensions length
         call checkread(nf90_inquire_dimension(ncid=ncFileID, dimID=idimID,  len=GIMAX),varname)
         call checkread(nf90_inquire_dimension(ncid=ncFileID, dimID=jdimID,  len=GJMAX),varname)
         KMAX=1
         if (ndims>3) then
           call checkread(nf90_inquire_dimension(ncid=ncFileID, dimID=kdimID,  len=KMAX),varname)
         endif
         call checkread(nf90_inquire_dimension(ncid=ncFileID, dimID=timedimID,  len=nrecords),varname)

!debug
!         print *, 'mod_readnc: sizes ',GIMAX,GJMAX,KMAX,nrecords


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

         if (ndims==3) then

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
         call checkread(nf90_get_att(ncFileID, VarID, "add_offset", add_offset),varname)
         call checkread(nf90_get_att(ncFileID, VarID, "scale_factor", scale_factor),varname)


! *** get time variable
!!  !  call check(nf90_inq_varid(ncid = ncFileID, name = "time", varID = timeDimID))
!!  !  call check(nf90_get_var(ncFileID, timeDimID, var_date,start=(/ nstart /),count=(/ nfetch /)))

! *** get concentration variable
    !     write(*,*)'Reading record',nstart,' to ',nstart+nfetch-1
         if (ndims==3) then

           call checkread(nf90_get_var(ncFileID,VarID,values,start=(/1,1,nstart/),count=(/ GIMAX,GJMAX,nfetch /)),varname)

         elseif (ndims==4) then
           call checkread(nf90_get_var(ncFileID, VarID, values,start=(/1,1,1,nstart/), &
                           count=(/GIMAX,GJMAX,KMAX,nfetch /)),varname)
        endif

!! !  if(Nrec<nrecords)then
!! !  endif
!! !  write(*,*)'date start '
!! !  call datefromsecondssince1970(ndate,var_date(1))
!! !  write(*,*)'date end '
!! !  call datefromsecondssince1970(ndate,var_date(nfetch))
!! !  period=(var_date(2)-var_date(1))/3600.


        if (ndims==3) then

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
        call checkread(nf90_close(ncFileID),varname)


  end subroutine ReadNCfile_kang




      end module mod_readnc


