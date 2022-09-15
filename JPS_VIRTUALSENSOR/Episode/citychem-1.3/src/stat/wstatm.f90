! <wstatm.f90 - A component of the City-scale
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

      subroutine WSTATM

!       2016 Matthias Karl, HZG, This is an original CityChem subroutine
!       The subroutine write main grid daily average concentrations to file.
! ----------------------------------------------------------------------------------
! REVISION HISTORY
!
!    xx Dat 201x  Name: Line  Description of Change
!
!    this subroutine is changed by Kang @ CARES, Dec. 2019 to write out average/instantanous 
!!   (determined by ("averaged_output" read from input control file for concentration data) 
!     emission data @ main grid (ground level) to .dat file. 
!
!     averaged_output=0, then it is instantaneous concentration at last timestep
!     =1, average concentration for each hour.
! ----------------------------------------------------------------------------------

      use mod_main
      use mod_site
      use mod_time
      use mod_conc
      use mod_stat
      use mod_writenc

      implicit none

! Local variables

      character(len=10) :: TEXT1
      character(len=10) :: TEXT2
      character(len=10) :: namefield
      character(len=10) :: unitname
      character(len=23) :: validity

      integer :: IC,K,IY,IX

! TEXT1,2 - Textstrings
! IC      - Index of compound

      integer :: Nhh_in

      integer, dimension(4,1) :: mdate

      logical :: dopacking  = .false.
      logical :: dofloat    = .false.
      logical :: domirror   = .false.

! Array arguments

      double precision        :: field2D(NX,NY,1)

!!=====================================================
!! write out emission data at main grid (ground level) to .dat file
!! added by Kang @ CARES, Dec. 23, 2019
!!=======================================================
      double precision, allocatable ::  fieldmain(:,:,:,:)   !!! main grid (ground level) gas concentrtions
      integer :: II
      real,   allocatable :: mxm_i(:,:)
      real,   allocatable :: mxm_j(:,:)

! Allocate
        if (.not. allocated(mxm_i) )    allocate(mxm_i(NY,NX))
        if (.not. allocated(mxm_j) )    allocate(mxm_j(NY,NX))        
      if (.not. allocated(fieldmain) )  allocate(fieldmain(NX,NY,1,NC))

!!=======================================================


! New write routine based on wconcm.for, main grid concentration

      print *,'wstatm: called every hour'

! Go through all compounds

      DO 100 IC = 1,NC


! Check for negative daily average concentrations

          DO IY=1,NY
            DO IX=1,NX

              IF ( CMAVED(IX,IY,IC) .lt. 0.0 ) THEN
                print *,'WSTATM: CMAVED < 0', IC,IX,IY,CMAVED(IX,IY,IC)
                call stopit('WSTATM: stopped because CMAVED<0')
              ENDIF

            ENDDO
          ENDDO


! Write the netCDF output file of all species,
! main grid hourly average concentrations (1 m above ground level)

          if (ND1STAFE) then

              ! one time step 
              Nhh_in = 1

              ! current simulation date
              mdate(1,Nhh_in) = YEAR
              mdate(2,Nhh_in) = MNTH
              mdate(3,Nhh_in) = DAYM
              mdate(4,Nhh_in) = HOUR

              namefield = CMPND(IC)
              if ( trim(CMPND(IC)).eq."PM2.5") then
                  namefield = "PM25      "
              endif

              unitname = "ug/m^3"

              if (averaged_output) then
                 validity = 'averaged'
              else
                 validity = 'instantaneous'
              endif

              dopacking = .false.
              dofloat   = .true.
              !domirror  = .true.
              domirror  = .false.

              ! 28.01.2017: field2D(IX,IY,1) is the right orientation
              do 110 IX = 1,NX
                do 120 IY = 1,NY

                field2D(IX,IY,1) = CMAVED(IX,IY,IC)

!!===================================================================
!! write out emission data at main grid (ground level) to .dat file
!! added by Kang @ CARES, Dec. 23, 2019
!!===================================================================
                fieldmain(IX,IY,1,IC) = CMAVED(IX,IY,IC)
!!===================================================================

  120           continue
  110         continue

              call writeconcfield(ND1STAFN, namefield,unitname,field2D(1:NX,1:NY,1), NX, NY, 1,   &
                                 Nhh_in, mdate, validity, dopacking, dofloat, domirror)

          endif

! ascii write routine
!
!            CALL W3DSURFFLD(MSTAUN(IC),MSTAFM(IC),IC,TEXT1,TEXT2,NX,NY,NC,  &
!                 MX,MY,MC, CMAVED)
!
!            IF (MESSFE) WRITE (MESSUN,2020)     &
!                 MOD(YEAR,100),MNTH,DAYM
!
!          ENDIF

! Next compound

  100 CONTINUE
  
!!===================================================================
!! write out emission data at main grid (ground level) to .dat file
!! added by Kang @ CARES, Dec. 23, 2019
!! Further changed @ Mar. 10,2020 by Kang for Center point main grid results and BC data output.
!!===================================================================

      open(unit=1226,file="../OUTPUT/mainground_hour_center.dat")  !! 3D data at center point of main cell

      write(1226,2140)(CMPND(IC),IC=1,NC)
      
 
            do  IX = 1,NX
                do  IY = 1,NY
                    mxm_i(IY,IX) = SITEX0 + (IX-1)*DX + 0.5*DX 
                    mxm_j(IY,IX) = SITEY0 + (IY-1)*DY + 0.5*DY         
             write(1226,2150) (mdate(II,Nhh_in),II=1,4), mxm_i(IY,IX),mxm_j(IY,IX),Z(1)-0.5*DZ(1),&
                              (fieldmain(IX,IY,1,IC), IC=1,NC) 
                  
                enddo
          enddo


      close(1226)

!$$$$$$       open(unit=1114,file="../OUTPUT/mainground_hour.dat")  !! open receptor hourly emission datafile
!$$$$$$ 
!$$$$$$       write(1114,2140)(CMPND(IC),IC=1,NC)
  

!$$$$$$           do  IX = 1,NX
!$$$$$$               do  IY = 1,NY
!$$$$$$                   mxm_i(IY,IX) = SITEX0 + (IX-1)*DX 
!$$$$$$                   mxm_j(IY,IX) = SITEY0 + (IY-1)*DY  
!$$$$$$              write(1114,2150)  (mdate(II,Nhh_in),II=1,4), mxm_i(IY,IX),mxm_j(IY,IX),Z(1),(fieldmain(IX,IY,1,IC), IC=1,NC)  
!$$$$$$             
!$$$$$$             enddo
!$$$$$$         enddo 


        if (allocated(fieldmain))     deallocate(fieldmain)

        if (allocated(mxm_i))     deallocate(mxm_i)
        if (allocated(mxm_j))     deallocate(mxm_j)
!$$$$$$        close(1114)



   
!!================================================================== 

      RETURN

 2000 format (4I2.2,2X)
 2010 format (A10)
 2020 format ('WSTATM: Write main grid daily average concentrations',     &
         ' for date ',3I2.2)

! End of subroutine WSTATM
!!==================================================================
!! write out emission data at main grid (ground level) to .dat file
!! added by Kang @ CARES, Dec. 23, 2019
!!==================================================================
 2140 format ("Year, Month, Day, Hour, X(m), Y(m), Z(m), ",22A10)
 2150 format (I4,3I3,3F12.3,22F12.3)
!!==================================================================
      end subroutine wstatm
