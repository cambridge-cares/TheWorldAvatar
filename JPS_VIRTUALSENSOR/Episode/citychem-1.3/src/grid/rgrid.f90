! <rgrid.f90 - A component of the City-scale
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

      subroutine RGRID

!     The subroutine reads grid model data from the Main input file.
!       2017 Matthias Karl, HZG, CityChem extension:
!            Substantial changes to the existing rgrid routine of EPISODE
!            in order to read netcdf filenames, bcon filenames and bcon values.
!            EPISODE newbc, newc and oldc files are not supported.
! ----------------------------------------------------------------------------------
! Based on:
! Version Episode 5.5 (May29, 2012) prepared for BB-Stand-Alone
! Original source code of EPISODE by Sam-Erik Walker (NILU)
!
! Sam-Erik Walker
! Norwegian Institute for Air Research (NILU)
! Instituttveien 18 P.O. Box 100
! N-2027 Kjeller, NORWAY
! Tel: +47 63898000 Fax: +47 63898050
! E-mail: sam-erik.walker@nilu.no
!
! ----------------------------------------------------------------------------------
! ----------------------------------------------------------------------------------
! REVISION HISTORY
!
!    18 Apr 2018  M. Karl: L460  Read 3D BCON offset for PM
!    24 Apr 2018  M. Karl: L408  GRIDPS option 4 for cc70bio chemistry
!
! ----------------------------------------------------------------------------------

      use mod_util
      use mod_main
      use mod_site
      use mod_conc
      use mod_grid

      implicit none
      
! *** Local variables

      INTEGER I
      INTEGER IC
      CHARACTER(len=256) TXTSTR
      LOGICAL LEOF

! *** I      - Index
! *** IC     - Index of compound
! *** TXTSTR - Text string
! *** LEOF   - If end of file then true else false


!_NEST_Start:
      IF (.NOT. ALLOCATED(bconfn))  ALLOCATE(bconfn(nc))
!MSK      IF (.NOT. ALLOCATED(newbcfn)) ALLOCATE(newbcfn(nc))
!MSK      IF (.NOT. ALLOCATED(NEWCFN))  ALLOCATE(NEWCFN(NC))
      IF (.NOT. ALLOCATED(OLDCFN))  ALLOCATE(OLDCFN(NC))
!MSK      IF (.NOT. ALLOCATED(OLDCFN_NEST))  ALLOCATE(OLDCFN_NEST(NC))
!_NEST_End.


!_NEST_Start:
      IF (.NOT. ALLOCATED(bconun))  ALLOCATE(bconun(nc))
!MSK      IF (.NOT. ALLOCATED(newbcun))  ALLOCATE(newbcun(nc))
!MSK      IF (.NOT. ALLOCATED(NEWCUN))  ALLOCATE(NEWCUN(NC))
      IF (.NOT. ALLOCATED(OLDCUN))  ALLOCATE(OLDCUN(NC))
!MSK      IF (.NOT. ALLOCATED(OLDCUN_NEST))  ALLOCATE(OLDCUN_NEST(NC))
!_NEST_End.


!_NEST_Start:
      IF (.NOT. ALLOCATED(bconfe))  ALLOCATE(bconfe(nc))
!MSK      IF (.NOT. ALLOCATED(newbcfe)) ALLOCATE(newbcfe(nc))
!MSK      IF (.NOT. ALLOCATED(NEWCFE))  ALLOCATE(NEWCFE(NC))
      IF (.NOT. ALLOCATED(OLDCFE))  ALLOCATE(OLDCFE(NC))
!MSK      IF (.NOT. ALLOCATED(OLDCFE_NEST))  ALLOCATE(OLDCFE_NEST(NC))
!_NEST_End.


!_NEST_Start:
      IF (.NOT. ALLOCATED(bconfv))  ALLOCATE(bconfv(nc))
!MSK      IF (.NOT. ALLOCATED(newbcfv)) ALLOCATE(newbcfv(nc))
!MSK      IF (.NOT. ALLOCATED(NEWCFV))  ALLOCATE(NEWCFV(NC))
      IF (.NOT. ALLOCATED(OLDCFV))  ALLOCATE(OLDCFV(NC))
!MSK      IF (.NOT. ALLOCATED(OLDCFV_NEST))  ALLOCATE(OLDCFV_NEST(NC))
!_NEST_End.


!_NEST_Start:
      IF (.NOT. ALLOCATED(bconfm))  ALLOCATE(bconfm(nc))
!MSK      IF (.NOT. ALLOCATED(newbcfm)) ALLOCATE(newbcfm(nc))
!MSK      IF (.NOT. ALLOCATED(NEWCFM))  ALLOCATE(NEWCFM(NC))
      IF (.NOT. ALLOCATED(OLDCFM))  ALLOCATE(OLDCFM(NC))
!MSK      IF (.NOT. ALLOCATED(OLDCFM_NEST))  ALLOCATE(OLDCFM_NEST(NC))
!_NEST_End.

      IF (.NOT. ALLOCATED(GRIDAS)) ALLOCATE(GRIDAS(NC))
      IF (.NOT. ALLOCATED(GRIDDS)) ALLOCATE(GRIDDS(NC))
      IF (.NOT. ALLOCATED(GRIDVS)) ALLOCATE(GRIDVS(NC))
!_NEST_Start:
!_ORG   IF (.NOT. ALLOCATED(C))      ALLOCATE(C(NC,NX,NY,NZ))
!MSK      IF (.NOT. ALLOCATED(C))      ALLOCATE(C(nc,1-nbcx:nx+nbcx,    &
!MSK                                                1-nbcy:ny+nbcy,    &
!MSK                                                       nz+nbcz))
!MSK index null not allowed
      IF (.NOT. ALLOCATED(C))      ALLOCATE(C(nc,1:nx+2*nbcx,    &
                                                1:ny+2*nbcy,    &
                                                       nz+nbcz))
!_NEST_End.
      IF (.NOT. ALLOCATED(DCDT))   ALLOCATE(DCDT(NC,NX,NY,NZ))

!_lhs_LSGRID_reduction_START_June_2011:      
      IF (.NOT. ALLOCATED(lsrc_dcdt)) ALLOCATE(lsrc_dcdt(NC,NX,NY,NZ))
!_lhs_LSGRID_reduction_END_June_2011.
      
      IF (.NOT. ALLOCATED(D2CDX2)) ALLOCATE(D2CDX2(NX,NY))

      IF (.NOT. ALLOCATED(D2CDY2)) ALLOCATE(D2CDY2(NX,NY))

      IF (.NOT. ALLOCATED(EXCZ))   ALLOCATE(EXCZ(NC,NX,NY))

!MSK not used      IF (.NOT. ALLOCATED(GCC))    ALLOCATE(GCC(NX,NY,NC))

!_NEST_Start:
!_ORG      IF (.NOT. ALLOCATED(BC))     ALLOCATE(BC(NC))
!MSK      IF (.NOT. ALLOCATED(BC))     ALLOCATE(BC(NC,1-nbcx:nx+nbcx,    &
!MSK                                                 1-nbcy:ny+nbcy,    &
!MSK                                                        nz+nbcz))

!MSK new allocation of BC, index ull is not allowed
      IF (.NOT. ALLOCATED(BC))     ALLOCATE(BC(NC, 1:nx+2*nbcx,    &
                                                   1:ny+2*nbcy,    &
                                                   nz+nbcz  ))

      IF (.NOT. ALLOCATED(BC_nest))     &
                  ALLOCATE(BC_nest(NC,1-nbcx_nest:nx_nest+nbcx_nest,    &
                                     1-nbcy_nest:ny_nest+nbcy_nest,    &
                                                nz_nest+nbcz_nest))

      IF (.NOT. ALLOCATED(IC_BC_nest))     &
               ALLOCATE(IC_BC_nest(nc,1-nbcx_nest:nx_nest+nbcx_nest,    &
                                      1-nbcy_nest:ny_nest+nbcy_nest,    &
                                                  nz_nest+nbcz_nest))

      IF (.NOT. ALLOCATED(IC_C_nest))     &
               ALLOCATE(IC_C_nest(nc,nx_nest,ny_nest,nz_nest))

!_NEST_End.

!_NEST_Start:
! ***  Control variables for total model mass and volume and their fluxes: (nc)

      IF (.NOT. ALLOCATED(TOTMASS))   ALLOCATE(TOTMASS(NC))
      IF (.NOT. ALLOCATED(TOTVOL))    ALLOCATE(TOTVOL(NC))
      IF (.NOT. ALLOCATED(VOLAVG))    ALLOCATE(VOLAVG(NC))
      IF (.NOT. ALLOCATED(WESTB_VF))  ALLOCATE(WESTB_VF(NC))
      IF (.NOT. ALLOCATED(WESTB_MF))  ALLOCATE(WESTB_MF(NC))
      IF (.NOT. ALLOCATED(NORTHB_VF)) ALLOCATE(NORTHB_VF(NC))
      IF (.NOT. ALLOCATED(NORTHB_MF)) ALLOCATE(NORTHB_MF(NC))
      IF (.NOT. ALLOCATED(EASTB_VF))  ALLOCATE(EASTB_VF(NC))
      IF (.NOT. ALLOCATED(EASTB_MF))  ALLOCATE(EASTB_MF(NC))
      IF (.NOT. ALLOCATED(SOUTHB_VF)) ALLOCATE(SOUTHB_VF(NC))
      IF (.NOT. ALLOCATED(SOUTHB_MF)) ALLOCATE(SOUTHB_MF(NC))
      IF (.NOT. ALLOCATED(BOTB_VF))   ALLOCATE(BOTB_VF(NC))
      IF (.NOT. ALLOCATED(BOTB_MF))   ALLOCATE(BOTB_MF(NC))
      IF (.NOT. ALLOCATED(TOPB_VF))   ALLOCATE(TOPB_VF(NC))
      IF (.NOT. ALLOCATED(TOPB_MF))   ALLOCATE(TOPB_MF(NC))
!_NEST_End.


! *** Read grid model add to result indicator:
      IF (MAINFE) THEN
          CALL GETDAT(MAINUN,TXTSTR,LEOF) 
          IF (TXTSTR(1:1) .NE. '<') THEN
              READ (TXTSTR,*) GRIDAI
          ENDIF
          print *,'rgrid: add grid model',GRIDAI
      ENDIF

      IF (GRIDAI .LT. 0) GRIDAI = 0
      IF (GRIDAI .GT. 1) GRIDAI = 1

      IF (MESSFE) WRITE (MESSUN,2000) GRIDAI

! *** Read type of horizontal advection scheme:
      IF (MAINFE) THEN
          CALL GETDAT(MAINUN,TXTSTR,LEOF) 
          IF (TXTSTR(1:1) .NE. '<') THEN
              READ (TXTSTR,*) (GRIDAS(IC),IC = 1,NC)
          ENDIF
          print *,'rgrid: hor advection',(GRIDAS(IC),IC = 1,NC)
      ENDIF

! *** Go through all compounds:
      DO IC = 1,NC

          IF (GRIDAS(IC) .LE. 0) THEN
              IF (MESSFE) WRITE (MESSUN,2010) GRIDAS(IC)
              GRIDAS(IC) = 0
          ENDIF

          IF (GRIDAS(IC) .EQ. 1) THEN
              IF (MESSFE) WRITE (MESSUN,2020) GRIDAS(IC)
          ENDIF

          IF (GRIDAS(IC) .EQ. 2) THEN
              IF (MESSFE) WRITE (MESSUN,2030) GRIDAS(IC)
          ENDIF

          IF (GRIDAS(IC) .GE. 3) THEN
              IF (MESSFE) WRITE (MESSUN,2040) GRIDAS
              CALL STOPIT('RGRID: No such horisontal advection scheme!')
          ENDIF

! *** Next compound
      ENDDO

! *** Read type of horisontal diffusion scheme:
      IF (MAINFE) THEN
          CALL GETDAT(MAINUN,TXTSTR,LEOF) 
          IF (TXTSTR(1:1) .NE. '<') THEN
              READ (TXTSTR,*) (GRIDDS(IC),IC = 1,NC)
          ENDIF
          print *,'rgrid: hor diffusion',(GRIDDS(IC),IC = 1,NC)
      ENDIF

! *** Go through all compounds:
      DO IC = 1,NC

          IF (GRIDDS(IC) .LE. 0) THEN
              IF (MESSFE) WRITE (MESSUN,2100) GRIDDS(IC)
              GRIDDS(IC) = 0
          ENDIF

          IF (GRIDDS(IC) .EQ. 1) THEN
              IF (MESSFE) WRITE (MESSUN,2110) GRIDDS(IC)
          ENDIF

          IF (GRIDDS(IC) .GE. 2) THEN
              IF (MESSFE) WRITE (MESSUN,2120) GRIDDS(IC)
              CALL STOPIT('RGRID: No such horisontal diffusion scheme!')
          ENDIF

! *** Next compound:
      ENDDO

! *** Read type of vertical scheme:
      IF (MAINFE) THEN
          CALL GETDAT(MAINUN,TXTSTR,LEOF) 
          IF (TXTSTR(1:1) .NE. '<') THEN
              READ (TXTSTR,*) (GRIDVS(IC),IC = 1,NC)
          ENDIF
          print *,'rgrid:  vertical scheme',(GRIDVS(IC),IC = 1,NC)
      ENDIF

! *** Go through all compounds:
      DO IC = 1,NC

!_LHS_SOA_May_2007_Start:

        IF (GRIDVS(IC) .LE. 0) THEN
          IF (MESSFE) WRITE (MESSUN,2200) GRIDVS(IC)
          GRIDVS(IC) = 0
        ENDIF

        IF (GRIDVS(IC) .EQ. 1) THEN
          IF (MESSFE) WRITE (MESSUN,2210) GRIDVS(IC)
        ENDIF

        IF (GRIDVS(IC) .EQ. 2) THEN
          IF (MESSFE) WRITE (MESSUN,2220) GRIDVS(IC)
        ENDIF

        IF (GRIDVS(IC) .EQ. 3) THEN
          IF (MESSFE) WRITE (MESSUN,2230) GRIDVS(IC)
        ENDIF

        IF (GRIDVS(IC) .EQ. 4) THEN
          IF (MESSFE) WRITE (MESSUN,2235) GRIDVS(IC)
        ENDIF

        IF (GRIDVS(IC) .EQ. 5) THEN
          IF (MESSFE) WRITE (MESSUN,2236) GRIDVS(IC)
        ENDIF

        IF (GRIDVS(IC) .GT. 5) THEN
          IF (MESSFE) WRITE (MESSUN,2240) GRIDVS(IC)
          CALL STOPIT('RGRID: No such vertical adv./diff. scheme!')
        ENDIF

!_LHS_SOA_May_2007_End.

! *** Next compound:
      ENDDO

! *** Read type of dry deposition scheme:
      IF (MAINFE) THEN
        CALL GETDAT(MAINUN,TXTSTR,LEOF) 
        IF (TXTSTR(1:1) .NE. '<') THEN
          READ (TXTSTR,*) GRIDDDS
        ENDIF
          print *,'rgrid:  drydep',GRIDDDS
      ENDIF

      IF (GRIDDDS .LE. 0) THEN
        IF (MESSFE) WRITE (MESSUN,2300) GRIDDDS
        GRIDDDS = 0
      ENDIF

      IF (GRIDDDS .EQ. 1) THEN
        IF (MESSFE) WRITE (MESSUN,2310) GRIDDDS
      ENDIF

      IF (GRIDDDS .GE. 2) THEN
        IF (MESSFE) WRITE (MESSUN,2320) GRIDDDS
        CALL STOPIT('RGRID: No such dry deposition scheme!')
      ENDIF

! *** Read type of wet deposition scheme:
      IF (MAINFE) THEN
        CALL GETDAT(MAINUN,TXTSTR,LEOF) 
        IF (TXTSTR(1:1) .NE. '<') THEN
          READ (TXTSTR,*) GRIDWDS
        ENDIF
          print *,'rgrid:  wetdep',GRIDWDS
      ENDIF

      IF (GRIDWDS .LE. 0) THEN
        IF (MESSFE) WRITE (MESSUN,2400) GRIDWDS
        GRIDWDS = 0
      ENDIF

      IF (GRIDWDS .EQ. 1) THEN
        IF (MESSFE) WRITE (MESSUN,2410) GRIDWDS
      ENDIF

      IF (GRIDWDS .GE. 2) THEN
        IF (MESSFE) WRITE (MESSUN,2420) GRIDWDS
        CALL STOPIT('RGRID: No such wet deposition scheme!')
      ENDIF

! *** Read type of photochemical scheme:
      IF (MAINFE) THEN
        CALL GETDAT(MAINUN,TXTSTR,LEOF) 
        IF (TXTSTR(1:1) .NE. '<') THEN
          READ (TXTSTR,*) GRIDPS
        ENDIF
          print *,'rgrid:  photochem',GRIDPS
      ENDIF

      IF (GRIDPS .LE. 0) THEN
        IF (MESSFE) WRITE (MESSUN,2500) GRIDPS
        GRIDPS = 0
      ENDIF
      IF (GRIDPS .EQ. 1) THEN
        IF (MESSFE) WRITE (MESSUN,2510) GRIDPS
      ENDIF

!MSK start
      IF (GRIDPS .EQ. 2) THEN
        IF (MESSFE) WRITE (MESSUN,2520) GRIDPS
      ENDIF

      IF (GRIDPS .EQ. 3) THEN
        IF (MESSFE) WRITE (MESSUN,2530) GRIDPS
      ENDIF

      IF (GRIDPS .GE. 4) THEN
        IF (MESSFE) WRITE (MESSUN,2540) GRIDPS
      ENDIF
!MSK end

! *** Read type of BC/IC
      IF (MAINFE) THEN
        CALL GETDAT(MAINUN,TXTSTR,LEOF) 
        IF (TXTSTR(1:1) .NE. '<') THEN
          READ (TXTSTR,*) bcictype
        ENDIF
        print *,'rgrid:  BC/IC type',bcictype
      ENDIF

! *** Read background concentrations filename:

!_NEST_Start:
       DO IC = 1,NC

        IF (MAINFE) THEN
!MSK
          if (bcictype==1) then
            CALL GETDAT(MAINUN,TXTSTR,LEOF)
            IF (TXTSTR(1:1) .NE. '<') THEN
              READ (TXTSTR,*) BCONFV(IC)
            ENDIF
            print *,'rgrid:  BC/IC value',BCONFV(IC)
            BCONFE(IC) = .false.
!MSK
          else
            CALL GETDAT(MAINUN,TXTSTR,LEOF)
            print *,'rgrid:  backgr conc file: ',TXTSTR
            CALL GETFNV(TXTSTR,BCONFN(IC),BCONFE(IC),1,1,BCONFV(IC))
          endif

        ELSE
          TXTSTR(1:1) = '<'
          CALL GETFNV(TXTSTR,BCONFN(IC),BCONFE(IC),1,1,    &
                     BCONFV(IC))
        ENDIF

        ENDDO
!_NEST_End.

!MSK start
! *** Offset of 3-D BCON input
      IF (MAINFE) THEN
        CALL GETDAT(MAINUN,TXTSTR,LEOF) 
        IF (TXTSTR(1:1) .NE. '<') THEN
          READ (TXTSTR,*) scaleo3
        ENDIF
        print *,'rgrid:  BCON O3-offset',scaleo3
      ENDIF
      IF (MAINFE) THEN
        CALL GETDAT(MAINUN,TXTSTR,LEOF) 
        IF (TXTSTR(1:1) .NE. '<') THEN
          READ (TXTSTR,*) scalepm
        ENDIF
        print *,'rgrid:  BCON PM-offset',scalepm
      ENDIF
!MSK end

!MSK start
! *** Restart option IC
      IF (MAINFE) THEN
        CALL GETDAT(MAINUN,TXTSTR,LEOF) 
        IF (TXTSTR(1:1) .NE. '<') THEN
          READ (TXTSTR,*) restart
        ENDIF
        print *,'rgrid:  RESTART option',restart
      ENDIF
!MSK end

!MSK start
! *** Read initial grid model concentrations filenames:
      IF (MAINFE) THEN
          CALL GETDAT(MAINUN,TXTSTR,LEOF)
          print *,'netcdf initial 3-D conc filename ',TXTSTR
          if(TXTSTR(1:1)==' ') then
             IC2GRIDFE=.false.
             print *,'no netcdf initial 3-D conc input for RESTART'
          else
             CALL GETFNV(TXTSTR,IC2GRIDFN,IC2GRIDFE,1,1,IC2GRIDFV)
             IC2GRIDFE=.true.
          endif
      ELSE
          TXTSTR(1:1) = '<'          
          CALL GETFNV(TXTSTR,IC2GRIDFN,IC2GRIDFE,1,1,IC2GRIDFV)
      ENDIF
!MSK end

!MSK start
!MSK New netCDF output of 3D main grid concentrations at last hour of simulation
      IF (MAINFE) THEN
          CALL GETDAT(MAINUN,TXTSTR,LEOF)
          print *,'netcdf last hour 3-D conc filename ',TXTSTR
          if(TXTSTR(1:1)==' ') then
             IC1GRIDFE=.false.
             print *,'no netcdf last hour 3-D conc output for RESTART'
          else
             CALL GETFNV(TXTSTR,IC1GRIDFN,IC1GRIDFE,1,1,IC1GRIDFV)
             IC1GRIDFE=.true.
          endif
      ELSE
          TXTSTR(1:1) = '<'          
          CALL GETFNV(TXTSTR,IC1GRIDFN,IC1GRIDFE,1,1,IC1GRIDFV)
      ENDIF
!MSK end

!MSK start
!MSK New netCDF output 2D for photochemistry diagnostics
      IF (MAINFE) THEN
          CALL GETDAT(MAINUN,TXTSTR,LEOF)
          print *,'netcdf short-lived CP hourly 2D filename ',TXTSTR
          if(TXTSTR(1:1)==' ') then
             NH1GRIDFE=.false.
             print *,'no netcdf short-lived CP hourly 2D output'
          else
             CALL GETFNV(TXTSTR,NH1GRIDFN,NH1GRIDFE,1,1,NH1GRIDFV)
             NH1GRIDFE=.true.
          endif
      ELSE
          TXTSTR(1:1) = '<'          
          CALL GETFNV(TXTSTR,NH1GRIDFN,NH1GRIDFE,1,1,NH1GRIDFV)
      ENDIF
!MSK end

!MSK start
! *** Read new grid model (3D) concentrations filenames:
!MSK     Currently not used
!MSK     Now commented new grid model (3D) save files
!MSK
!MSK      DO 120 IC = 1,NC
!MSK
!MSK        IF (MAINFE) THEN
!MSK          CALL GETDAT(MAINUN,TXTSTR,LEOF)
!MSK            print *,'rgrid:  new grid model (3D) file: ',TXTSTR
!MSK
!MSK            if(TXTSTR(1:1)==' ') then
!MSK              NEWCFE(IC)=.false.
!MSK              NEWCFV(IC)=MISS 
!MSK              print *,'no 3D IC file for', IC
!MSK           else
!MSK             CALL GETFNV(TXTSTR,NEWCFN(IC),NEWCFE(IC),1,1,    &
!MSK                         NEWCFV(IC))
!MSK           endif
!MSK        ELSE
!MSK           TXTSTR(1:1) = '<'
!MSK           CALL GETFNV(TXTSTR,NEWCFN(IC),NEWCFE(IC),1,1,    &
!MSK                     NEWCFV(IC))
!MSK        ENDIF
!MSK
!MSK  120 CONTINUE
!MSK end


!MSK *** Next is the subgrid BC files, NEWBCFN
!MSK     Currently not used
!MSK     Now commented subrid BC and IC files

!_NEST_Start:
! *** Read new boundary condition concentrations filename for the inner NEST-domain:
!MSK      DO IC = 1,NC
!MSK
!MSK        IF (MAINFE) THEN
!MSK          CALL GETDAT(MAINUN,TXTSTR,LEOF)
!MSK            print *,'rgrid:  subgrid boundary condition file: ',TXTSTR
!MSK
!MSK          if(TXTSTR(1:1)==' ') then
!MSK             NEWBCFE(IC)=.false.
!MSK             print *,'no subgrid BC file'
!MSK          else
!MSK            TXTSTR(1:1) = '<'
!MSK            CALL GETFNV(TXTSTR,NEWBCFN(IC),NEWBCFE(IC),1,1,    &
!MSK                     NEWBCFV(IC))
!MSK          endif
!MSK
!MSK        ENDIF
!MSK
!MSK      ENDDO
!_NEST_End.


!_CITYDELTA_NEST_Start:

!  ***  Read new initial concentrations filename for the inner NEST-domain:
!
!MSK     Now commented subrid BC and IC files
!MSK       DO IC = 1,NC
!MSK 
!MSK         IF (MAINFE) THEN
!MSK           CALL GETDAT(MAINUN,TXTSTR,LEOF)
!MSK             print *,'rgrid:  subgrid initial condition file: ',TXTSTR
!MSK 
!MSK           if(TXTSTR(1:1)==' ') then
!MSK              OLDCFE_NEST(IC)=.false.
!MSK              print *,'no subgrid IC file'
!MSK           else
!MSK             TXTSTR(1:1) = '<'
!MSK             CALL GETFNV(TXTSTR,OLDCFN_NEST(IC),OLDCFE_NEST(IC),1,1,    &
!MSK                       OLDCFV_NEST(IC))
!MSK           endif
!MSK 
!MSK         ENDIF
!MSK 
!MSK       ENDDO

!_CITYDELTA_NEST_End.

 1000 format (A256)

 2000 format ('RGRID: Grid model add to indicator = ',I5)
 2010 format ('RGRID: Grid model horisontal advection scheme = ',I5,    &
             ' no horisontal advection will be performed!')
 2020 format ('RGRID: Grid model horisontal advection scheme = ',I5,    &
             ' positive definite 4th degree Bott scheme')
 2030 format ('RGRID: Grid model horisontal advection scheme = ',I5,    &
             ' positive definite and monotone 4th degree Bott scheme')
 2040 format ('RGRID: Grid model horisontal advection scheme = ',I5,    &
             ' this scheme is not defined!')

 2100 format ('RGRID: Grid model horisontal diffusion scheme = ',I5,    &
             ' no horisontal diffusion will be performed!')
 2110 format ('RGRID: Grid model horisontal diffusion scheme = ',I5,    &
             ' fully explicit 2D (x,y) forward Euler scheme')
 2120 format ('RGRID: Grid model horisontal diffusion scheme = ',I5,    &
             ' this scheme is not defined!')


 2200 format ('RGRID: Grid model vertical scheme = ',I5,    &
             ' no vertical advection/diffusion will be performed!')
 2210 format ('RGRID: Grid model vertical scheme = ',I5,    &
             ' combined advection/diffusion scheme')
 2220 format ('RGRID: Grid model vertical scheme = ',I5,    &
             ' timesplitted advection/diffusion scheme')
 2230 format ('RGRID: Grid model vertical scheme = ',I5,    &
             ' advection and Crank-Nicholson diffusion scheme')
 2235 format ('RGRID: Grid model vertical scheme = ',I5,    &
             ' no advection and Crank-Nicholson diffusion scheme')
 2236 format ('RGRID: Grid model vertical scheme = ',I5,    &
             ' no diffusion and upstream advection scheme')
 2240 format ('RGRID: Grid model vertical scheme = ',I5,    &
             ' this scheme is not defined!')


 2300 format ('RGRID: Dry deposition scheme = ',I5,    &
             ' no dry deposition will be performed!')
 2310 format ('RGRID: Grid model dry deposition scheme = ',I5,    &
             ' Ordinary dry deposition')
 2320 format ('RGRID: Grid model dry deposition scheme = ',I5,    &
             ' this scheme is not defined!')

 2400 format ('RGRID: Wet deposition scheme = ',I5,    &
             ' no wet deposition will be performed!')
 2410 format ('RGRID: Grid model wet deposition scheme = ',I5,    &
             ' Ordinary wet deposition')
 2420 format ('RGRID: Grid model wet deposition scheme = ',I5,    &
             ' this scheme is not defined!')

 2500 format ('RGRID: Grid model photochemical scheme = ',I5,    &
             ' no photochemical calculations will be performed!')
 2510 format ('RGRID: Grid model photochemical scheme = ',I5,    &
             ' Photochemical balance between NO, NO2 and O3')
!MSK start
 2520 format ('RGRID: Grid model photochemical scheme = ',I5,    &
             ' EMEP03 reactions scheme applied')
 2530 format ('RGRID: Grid model photochemical scheme = ',I5,    &
             ' EMEP 45 reactions scheme applied')
 2540 format ('RGRID: Grid model photochemical scheme = ',I5,    &
             ' CC70BIO (+SOA) reactions scheme applied')
 2550 format ('RGRID: Grid model photochemical scheme = ',I5,    &
             ' this scheme is not defined!')
!MSK end

 2600 format ('RGRID: Coarse background conc. for comp. nr. ',    &
             I3,' filename  = ',256A1)
 2605 format ('RGRID: Coarse background conc. for comp. nr. ',    &
             I3,' filevalue = ',E8.1)
 2610 format ('RGRID: Old grid model conc. for compound nr. ',    &
             I3,' filename  = ',256A1)
 2615 format ('RGRID: Old grid model conc. for compound nr. ',    &
             I3,' filevalue = ',E8.1)
 2620 format ('RGRID: New grid model conc. for compound nr. ',    &
             I3,' filename  = ',256A1)

 2720 format ('RGRID: New Fine Grid Boundary conc. for compound nr. ',    &
             I3,' filename  = ',256A1)
 2820 format ('RGRID: New Fine Grid Initial  conc. for compound nr. ',    &
             I3,' filename  = ',256A1)


! *** End of subroutine RGRID

      end subroutine rgrid
