! <ricon.f90 - A component of the City-scale
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

      subroutine RICON

!       2017 Matthias Karl, HZG, This is an original CityChem subroutine.
!       Subroutine to read initial concentrations.
!       The subroutine checks old grid model concentrations from files.
!       This version only treats the internal model domain (1:NX,1:NY,1:NZ)
! ----------------------------------------------------------------------------------
! REVISION HISTORY
!
!    xx Dat 201x  Name: Line  Description of Change
!
! ----------------------------------------------------------------------------------

      use mod_util
      use mod_main
      use mod_site
      use mod_time
      use mod_conc
      use mod_grid
      use mod_readnc


      implicit none
      
!     Local variables

      double precision    :: CMINV
!      REAL    :: scalv
      double precision,allocatable :: FAKE_C(:,:,:,:)
      INTEGER      :: IC,IX,IY,IZ


! *** CMINV    - Old grid model concentration minimum value
! *** scale    - Scale factor
! *** FAKE_C   - Dummy concentration array
! *** IC       - Index of compound
! *** IX,IY,IZ - Main grid indices
! *** TEXT1,2  - Textstrings

!netCDF local variables
        integer           :: p
        integer           :: nstart
        integer           :: nfetch
        character(len=10) :: varname
        double precision  :: field1D(NY*NX*NZ*1)
!netCDF


      IF (.NOT. ALLOCATED(FAKE_C)) ALLOCATE(FAKE_C(nc,nx,ny,nz))

! NETCDF: The OLDC files will be replaced by one netcdf file

!MSK start
!    3-D IC file not existing while RESTART
      if ((.not.IC2GRIDFE).and.(restart.eq.1)) then
          call STOPIT('RICON: IC 3-D FILE NOT EXISTING FOR RESTART')
      endif
!MSK end


!MSK If 3-D ICON files exist and RESTART option is true
!    Open netcdf and read all chemical compound concentrations
!    into "FAKE_C"     
        if (restart==1) then

        ! I_MAX,J_MAX,K_MAX, dimensions of "var"(input) NX,NY,NZ

!     Go through all compounds:

          DO 100 IC = 1,NC

             nstart  = 1
             nfetch  = 1

             varname = CMPND(IC)
             if ( trim(CMPND(IC)).eq."PM2.5") then
                 varname = "PM25      "
             endif

!      Open and read concentration field for compound IC from ICON file

             call ReadNCfile(varname,IC2GRIDFN,field1D,NX,NY,NZ,nstart,nfetch)

!      Read the variable concentration into FAKE_C
!      field1D has order i,j,k,n  (n=1)

             p=1
             do IZ=1,NZ
            !   do IX=1,NX
               do IY=1,NY
                 do IX=1,NX  !!!! changed by Kang @ Dec. 3nd, 2019
 
                   FAKE_C(IC,IX,IY,IZ) = field1D(p)
                   p=p+1

! ***  Negative data is not tolerated, necessary check for the initial boundary values:

                   if (FAKE_C(IC,IX,IY,IZ) .lt. 0.) then
                      if (MESSFE) write(MESSUN,2010)    &
                         IX,IY,IZ,FAKE_C(IC,IX,IY,IZ),CUNIT(IC),CMPND(IC)
                      call STOPIT('RICON: Negative concentrations in ICON restart file')
                   endif

                 enddo
               enddo
             enddo

! ***   Go through all main grid model cells and fill the main grid C() field (excl. boundary cells)

             do IZ=1,NZ
               do IY=1,NY
                 do IX=1,NX

                     C(IC,IX+1,IY+1,IZ) = FAKE_C(IC,IX,IY,IZ)

                enddo
               enddo
             enddo

!!!!!!!!!!!!!!!!!!! for testing 3D concentration output file by Kang @ Dec.2, 2019
!             if(IC .eq. 3) then
!                open(unit=1111,file="../OUTPUT/3D_field_read_Fake_C.txt")  !!
!                open(unit=1113,file="../OUTPUT/3D_field_read_C.txt")  !!
!                open(unit=1112,file="../OUTPUT/3D_field_para_read.txt")  !!        
!                do 351 IZ=1,NZ
!                    do 353 IY = 1,NY


!                      write(1111,*) (FAKE_C(3,IX,IY,IZ),IX=1,NX)
!                      write(1113,*) (C(3,IX+1,IY+1,IZ),IX=1,NX)
!  353               continue
!  351           continue
!  350          continue

!                 write(1112,*)"read 3D concentration nc file, input: NX=",NX,", NY=", &
!                              NY, "NZ=",NZ, "nfetch=",nfetch
!                 write(1112,*)"read from nc file: varname=",varname, ", IC2GRIDFN=",IC2GRIDFN
               
!                close(1113)
!                close(1112)
!                close(1111)
!            endif
!!!!!!!!!!!!!!!!!!! end for testing 3D concentration output file by Kang @ Dec.2, 2019


  100     CONTINUE












        else

!MSK start
!MSK !     Restart == 0
!MSK !     If not restart then use the BCON values as initial concentrations
        endif

! ***   Now check the whole main grid concentration field C()
! ***   if all cells have values (main grid and boundary cells)
! ***   Define old grid model concentration minimum value:

         DO IC = 1,NC
           CMINV = 0.
           IF (CUNIT(IC)(1:7) .EQ. 'mol/cm3') CMINV = 1.E-20
           DO IZ=1,NZ+NBCZ
             DO IY=1,NY+2*NBCY
               DO IX=1,NX+2*NBCX

! ***         Negative data is not tolerated:
                IF (C(IC,IX,IY,IZ) .LT. 0.) THEN
                   IF (MESSFE) WRITE (MESSUN,2020)    &
                      IX,IY,IZ,C(IC,IX,IY,IZ),CUNIT(IC),CMPND(IC)
                   call STOPIT('RICON: Negative concentrations in main grid C() field')
                ENDIF

              ENDDO
            ENDDO
          ENDDO
        ENDDO

        DEALLOCATE (FAKE_C)

      RETURN

 2010 FORMAT ('RICON: Initial grid model conc. C(',I3,',',I3,',',I3,') = ',E10.1,1X,A10,    &
             ' for comp. ',A10,' is negative!')
 2020 FORMAT ('RICON: Main grid model conc. C(',I3,',',I3,',',I3,') = ',E10.1,1X,A10,    &
             ' for comp. ',A10,' is negative!')


! *** End of subroutine RICON

      end subroutine ricon
