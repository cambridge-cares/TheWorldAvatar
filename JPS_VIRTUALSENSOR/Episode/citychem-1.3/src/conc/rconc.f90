! <rconc.f90 - A component of the City-scale
!                 Chemistry Transport Model EPISDOE-CityChem>
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
!*****************************************************************************! 

      subroutine  rconc

!     The subroutine reads concentrations data from the main input file.
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
!    xx Dat 201x  Name: Line  Description of Change
!
! ----------------------------------------------------------------------------------

      use mod_util
      use mod_main
      use mod_site
      use mod_conc
      use mod_depo

      implicit none

!     Local variables

      INTEGER       :: I,IC
      CHARACTER(len=256) :: TXTSTR
      LOGICAL       :: LEOF

!     I,IC   - Indices
!     TXTSTR - Text string
!     LEOF   - If end of file then true else false

!     Read number of compounds:
      IF (MAINFE) THEN
          CALL GETDAT(MAINUN,TXTSTR,LEOF) 
          IF (TXTSTR(1:1) .NE. '<') THEN
              READ (TXTSTR,*) NC
          ENDIF
!_LHS_SOA_June_2007_Start:
          CALL GETDAT(MAINUN,TXTSTR,LEOF) 
          IF (TXTSTR(1:1) .NE. '<') THEN
              READ (TXTSTR,*) n_nochem
          ENDIF

          CALL GETDAT(MAINUN,TXTSTR,LEOF) 
          IF (TXTSTR(1:1) .NE. '<') THEN
              READ (TXTSTR,*) n_advect
          ENDIF

      ENDIF

      IF (MESSFE) WRITE (MESSUN,2000) NC
      IF (MESSFE) WRITE (MESSUN,2001) n_nochem
      IF (MESSFE) WRITE (MESSUN,2002) n_advect
!_LHS_Start:
      IF (MESSFE) WRITE (MESSUN,2003) NR
!_LHS_End.

!_LHS_SOA_June_2007_End.

!MSK      IF (.NOT. ALLOCATED(MCONFN)) ALLOCATE(MCONFN(NC))
!_NEST_Start:
      IF (.NOT. ALLOCATED(MERRFN)) ALLOCATE(MERRFN(NC))
      IF (.NOT. ALLOCATED(MANAFN)) ALLOCATE(MANAFN(NC))
!_NEST_End.
      IF (.NOT. ALLOCATED(SCONFN)) ALLOCATE(SCONFN(NC))
!MSK      IF (.NOT. ALLOCATED(MCONUN)) ALLOCATE(MCONUN(NC))
!_NEST_Start:
      IF (.NOT. ALLOCATED(MERRUN)) ALLOCATE(MERRUN(NC))
      IF (.NOT. ALLOCATED(MANAUN)) ALLOCATE(MANAUN(NC))
!_NEST_End.
      IF (.NOT. ALLOCATED(SCONUN)) ALLOCATE(SCONUN(NC))
!MSK      IF (.NOT. ALLOCATED(MCONFE)) ALLOCATE(MCONFE(NC))
!_NEST_Start:
      IF (.NOT. ALLOCATED(MERRFE)) ALLOCATE(MERRFE(NC))
      IF (.NOT. ALLOCATED(MANAFE)) ALLOCATE(MANAFE(NC))
!_NEST_End.
      IF (.NOT. ALLOCATED(SCONFE)) ALLOCATE(SCONFE(NC))
!MSK      IF (.NOT. ALLOCATED(MCONFV)) ALLOCATE(MCONFV(NC))
!_NEST_Start:
      IF (.NOT. ALLOCATED(MERRFV)) ALLOCATE(MERRFV(NC))
      IF (.NOT. ALLOCATED(MANAFV)) ALLOCATE(MANAFV(NC))
!_NEST_End.
      IF (.NOT. ALLOCATED(SCONFV)) ALLOCATE(SCONFV(NC))
!MSK      IF (.NOT. ALLOCATED(MCONFM)) ALLOCATE(MCONFM(NC))
!_NEST_Start:
      IF (.NOT. ALLOCATED(MERRFM)) ALLOCATE(MERRFM(NC))
      IF (.NOT. ALLOCATED(MANAFM)) ALLOCATE(MANAFM(NC))
!_NEST_End.
      IF (.NOT. ALLOCATED(SCONFM)) ALLOCATE(SCONFM(NC))
!MSK
      if (.not. allocated(icmpnd)) allocate(icmpnd(nc))

      IF (.NOT. ALLOCATED(CMPND))  ALLOCATE(CMPND(NC))
      IF (.NOT. ALLOCATED(CUNIT))  ALLOCATE(CUNIT(NC))
      IF (.NOT. ALLOCATED(CMOLW))  ALLOCATE(CMOLW(NC))
      IF (.NOT. ALLOCATED(THALF))  ALLOCATE(THALF(NC))
      IF (.NOT. ALLOCATED(COUT))   ALLOCATE(COUT(NC))
      IF (.NOT. ALLOCATED(CM))     ALLOCATE(CM(NX,NY,NC))
      IF (.NOT. ALLOCATED(CR))     ALLOCATE(CR(NC,NR))
!MSK      IF (.NOT. ALLOCATED(CS))     ALLOCATE(CS(NC,NXS,NYS,MLS))

!_NEST_Start:
      IF (.NOT. ALLOCATED(c_an))   ALLOCATE(c_an(NX,NY,NC))
      IF (.NOT. ALLOCATED(c_err))  ALLOCATE(c_err(NX,NY,NC))
!_NEST_End.

      IF (.NOT. ALLOCATED(DDEPOI)) ALLOCATE(DDEPOI(NC))
      IF (.NOT. ALLOCATED(WDEPOI)) ALLOCATE(WDEPOI(NC))
      IF (.NOT. ALLOCATED(DDEPV))  ALLOCATE(DDEPV(NC))
      IF (.NOT. ALLOCATED(WDEPSR)) ALLOCATE(WDEPSR(NC))

      MC = NC
!_LHS_Start:
      MR = NR
!_LHS_End.

!     Read compound names:                         CMPND(IC),
!          compound units:                         CUNIT(IC),
!          indicators for output of concentration: COUT(IC),
!          indicators for dry deposition:          DDEPOI(IC),
!          indicators for wet deposition:          WDEPOI(IC),
!          molecular weights:                      CMOLW(IC),
!          dry deposition velocity (cm/s):         DDEPV(IC),
!          wet deposition scavenging ratio:        WDEPSR(IC),
!          compound radio-active half-life time:   THALF(IC),
!          (-9900 means no radioactive decay)
! *
! * Compound          Unit  ConI DDepI WDepI  MolW     DDepV  WDepSR   THalf
! *
! 'OP'           'mol/cm3'     0     0     0    16   0.0E+00 0.0E+00 0.0E+00 O(3P)
! 'OD'           'mol/cm3'     0     0     0    16   0.0E+00 0.0E+00 0.0E+00 O(1D)
! 'OH'           'mol/cm3'     1     0     0    17   0.0E+00 0.0E+00 0.0E+00
! 'O3'           'mol/cm3'     1     1     1    48   0.5E+00 0.0E+00 0.0E+00 Ozone
! 'HO2'          'mol/cm3'     0     0     0    33   0.0E+00 0.0E+00 0.0E+00
! 'H2O2'         'mol/cm3'     0     1     0    34   0.5E+00 0.0E+00 0.0E+00
! 'NO'           'mol/cm3'     1     0     0    30   0.0E+00 0.0E+00 0.0E+00 Nitrogenoxide
! 'NO2'          'mol/cm3'     1     1     1    46   0.2E+00 0.0E+00 0.0E+00 Nitrogendioxide
! 'NO3'          'mol/cm3'     1     0     0    62   0.0E+00 0.0E+00 0.0E+00 Nitrate
! 'N2O5'         'mol/cm3'     0     0     0   108   0.0E+00 0.0E+00 0.0E+00
! 'HNO3'         'mol/cm3'     1     1     1    63   1.0E+00 0.0E+00 0.0E+00
! 'SO2'          'mol/cm3'     0     1     1    64   0.2E+00 0.0E+00 0.0E+00 Sulphurdioxide
! 'Sulphate'     'mol/cm3'     0     1     0    96   0.1E+00 0.0E+00 0.0E+00
! 'H2'           'mol/cm3'     0     1     0     2   0.0E+00 0.0E+00 0.0E+00 Hydrogen
! 'CH4'          'mol/cm3'     0     0     0    16   0.0E+00 0.0E+00 0.0E+00 Methane
! 'CH3O2'        'mol/cm3'     0     0     0    47   0.0E+00 0.0E+00 0.0E+00
! 'HCHO'         'mol/cm3'     1     0     0    30   0.0E+00 0.0E+00 0.0E+00 Formaldehyde
! 'CH3OH'        'mol/cm3'     0     0     0 -9900   0.0E+00 0.0E+00 0.0E+00 Methanol
! 'CH3O2H'       'mol/cm3'     0     1     0    48   0.5E+00 0.0E+00 0.0E+00
! 'CO'           'mol/cm3'     1     0     0    28   0.0E+00 0.0E+00 0.0E+00 Carbon monoxide
! 'C2H6'         'mol/cm3'     0     0     0    30   0.0E+00 0.0E+00 0.0E+00 Ethane
! 'C2H5O2'       'mol/cm3'     0     0     0    61   0.0E+00 0.0E+00 0.0E+00
! 'CH3CHO'       'mol/cm3'     0     0     0    44   0.0E+00 0.0E+00 0.0E+00 Acetaldehyde
! 'CH3COO2'      'mol/cm3'     0     0     0    59   0.0E+00 0.0E+00 0.0E+00
! 'PAN'          'mol/cm3'     1     0     1   121   0.0E+00 0.0E+00 0.0E+00 CH3COO2NO2
! 'C2H5OH'       'mol/cm3'     0     0     0    46   0.0E+00 0.0E+00 0.0E+00
! 'nC4H10'       'mol/cm3'     1     0     0    58   0.0E+00 0.0E+00 0.0E+00 n-Butane
! 'secC4H9O2'    'mol/cm3'     0     0     0    89   0.0E+00 0.0E+00 0.0E+00
! 'CH3COC2H5'    'mol/cm3'     0     0     0 -9900   0.0E+00 0.0E+00 0.0E+00
! 'CH3COCHO2CH3' 'mol/cm3'     0     0     0   103   0.0E+00 0.0E+00 0.0E+00
! 'CH3COCOCH3'   'mol/cm3'     0     0     0 -9900   0.0E+00 0.0E+00 0.0E+00
! 'C2H4'         'mol/cm3'     0     0     0    28   0.0E+00 0.0E+00 0.0E+00 Ethene
! 'CH2O2CH2OH'   'mol/cm3'     0     0     0 -9900   0.0E+00 0.0E+00 0.0E+00
! 'C3H6'         'mol/cm3'     0     0     0    42   0.0E+00 0.0E+00 0.0E+00 Propene
! 'CH3CHO2CH2OH' 'mol/cm3'     0     0     0 -9900   0.0E+00 0.0E+00 0.0E+00
! 'oXylen'       'mol/cm3'     0     0     0   106   0.0E+00 0.0E+00 0.0E+00 C6H4(CH3)CH3
! 'oXylOHO2'     'mol/cm3'     0     0     0 -9900   0.0E+00 0.0E+00 0.0E+00 C6H3H(O2)CH3(OH)CH3(O2)
! 'Memaldial'    'mol/cm3'     0     0     0 -9900   0.0E+00 0.0E+00 0.0E+00 CH3COCH=CHCH0
! 'CH3COCHO'     'mol/cm3'     0     0     0 -9900   0.0E+00 0.0E+00 0.0E+00
! 'MemalO2'      'mol/cm3'     0     0     0 -9900   0.0E+00 0.0E+00 0.0E+00 CH3COCHOH-CH(O2)CHO
! 'HCOCHO'       'mol/cm3'     0     0     0 -9900   0.0E+00 0.0E+00 0.0E+00
! 'Isoprene'     'mol/cm3'     0     0     0    68   0.0E+00 0.0E+00 0.0E+00 C5H8
! 'IsopO2'       'mol/cm3'     0     0     0 -9900   0.0E+00 0.0E+00 0.0E+00 OHC5H8O2
! 'MVKetone'     'mol/cm3'     0     0     0 -9900   0.0E+00 0.0E+00 0.0E+00 CH3COCH=CH2
! 'MVKO2'        'mol/cm3'     0     0     0 -9900   0.0E+00 0.0E+00 0.0E+00 OHCH3COCHCH2O2
!
      DO 100 IC = 1,NC

          IF (MAINFE) THEN
              CALL GETDAT(MAINUN,TXTSTR,LEOF)
              IF (TXTSTR(1:1) .NE. '<') THEN
                  READ (TXTSTR,*) icmpnd(IC),CMPND(IC),CUNIT(IC),    &
                                 COUT(IC),DDEPOI(IC),WDEPOI(IC),   &
                                 CMOLW(IC),DDEPV(IC),WDEPSR(IC),   &
                                 THALF(IC)
              ENDIF
          ENDIF

          IF (MESSFE) THEN
            WRITE (MESSUN,2020) CMPND(IC),CUNIT(IC)
            WRITE (MESSUN,2021) CMPND(IC),COUT(IC),DDEPOI(IC),WDEPOI(IC)
            WRITE (MESSUN,2022) CMPND(IC),CMOLW(IC)
            WRITE (MESSUN,2023) CMPND(IC),DDEPV(IC)
            WRITE (MESSUN,2024) CMPND(IC),WDEPSR(IC)
            WRITE (MESSUN,2025) CMPND(IC),THALF(IC)
          ENDIF

!         If negative molecular weight then missing data:
          IF (CMOLW(IC) .LE. 0.) CMOLW(IC) = MISS

!         If negative dry deposition velocity then missing data:
          IF (DDEPV(IC) .LE. 0.) DDEPV(IC) = 0.

!         Convert dry deposition velocity from cm/s to m/s:
          IF (DDEPV(IC) .NE. MISS) DDEPV(IC) = 0.01*DDEPV(IC)

!         If negative radioactive half-life then missing data:
          IF (THALF(IC) .LE. 0.) THALF(IC) = MISS

  100 CONTINUE


!
!     Instantaneous or NTS-Averaged output of main grid concentrations:

      IF (MAINFE) THEN
          CALL GETDAT(MAINUN,TXTSTR,LEOF) 
          IF (TXTSTR(1:1) .NE. '<') THEN
              READ (TXTSTR,*) I_NTS
          ENDIF
          print *,'rconc: instant or average',I_NTS
      ENDIF

      IF (I_NTS == 0)THEN
         averaged_output = .false.
      ELSE
         averaged_output = .true.       
      ENDIF

      if (averaged_output) then
        if (MESSFE) &
         write (MESSUN,*) 'RCONC: AVERAGED_OUTPUT is Applied.'
      else
        if (MESSFE)  &
         write (MESSUN,*) 'RCONC: AVERAGED_OUTPUT is not Applied.' 
      endif    


!MSK start
!MSK New netCDF output for main grid replaces old ASCII/BIN output
      IF (MAINFE) THEN
          CALL GETDAT(MAINUN,TXTSTR,LEOF)
          print *,'netcdf output main grid hourly 3D filename ',TXTSTR
          if(TXTSTR(1:1)==' ') then
             NH1CONFE=.false.
             print *,'no netcdf main grid hourly 3D output'
          else
             CALL GETFNV(TXTSTR,NH1CONFN,NH1CONFE,1,1,NH1CONFV)
             NH1CONFE=.true.
          endif
      ELSE
          TXTSTR(1:1) = '<'          
          CALL GETFNV(TXTSTR,NH1CONFN,NH1CONFE,1,1,NH1CONFV)
      ENDIF

!MSK  Optional main grid conc output
!MSK  Read main grid concentration filenames:
!MSK      DO 150 IC = 1,NC
!MSK
!MSK      IF (MAINFE) THEN
!MSK
!MSK          CALL GETDAT(MAINUN,TXTSTR,LEOF)
!MSK          print *,'rconc:  main grid conc filename: ',TXTSTR
!MSK
!MSK          if(TXTSTR(1:1)==' ') then
!MSK             MCONFE(IC)=.false.
!MSK             print *,'no main grid conc file for', IC
!MSK          else
!MSK             CALL GETFNV(TXTSTR,MCONFN(IC),MCONFE(IC),1,1,MCONFV(IC))
!MSK             MCONFE(IC)=.true.
!MSK          endif
!MSK      ELSE
!MSK          TXTSTR(1:1) = '<'          
!MSK          CALL GETFNV(TXTSTR,MCONFN(IC),MCONFE(IC),1,1,MCONFV(IC))
!MSK      ENDIF
!MSK
!MSK  150 CONTINUE
!MSK end


!_NEST_Start:
!     Read main grid analytical concentration filenames
!
!      DO IC = 1,NC
!
!      IF (MAINFE) THEN
!          CALL GETDAT(MAINUN,TXTSTR,LEOF)
!          IF (TXTSTR(1:1) .NE. '<') THEN
!              READ (TXTSTR,1000) MANAFN(IC)
!          ENDIF
!      ELSE
!          TXTSTR(1:1) = '<'
!          CALL GETFNV(TXTSTR,MANAFN(IC),MANAFE(IC),1,1,
!     &                MANAFV(IC))
!      ENDIF
!
!      IF (MESSFE) THEN 
!	    WRITE (MESSUN,2031) IC,
!     &    (MANAFN(IC)(I:I),I=1,INDEX(MANAFN(IC),' ')-1)
!
!	ENDIF
!
!      ENDDO
!
! Read main grid concentration error (computed - analytic) filenames
!
!      DO IC = 1,NC
!
!      IF (MAINFE) THEN
!          CALL GETDAT(MAINUN,TXTSTR,LEOF)
!          IF (TXTSTR(1:1) .NE. '<') THEN
!              READ (TXTSTR,1000) MERRFN(IC)
!          ENDIF
!      ELSE
!          TXTSTR(1:1) = '<'
!          CALL GETFNV(TXTSTR,MERRFN(IC),MERRFE(IC),1,1,
!     &                MERRFV(IC))
!      ENDIF
!
!      IF (MESSFE) THEN 
!	    WRITE (MESSUN,2032) IC,
!     &    (MERRFN(IC)(I:I),I=1,INDEX(MERRFN(IC),' ')-1)
!
!	ENDIF
!
!      ENDDO
!_NEST_End.

!MSK not found in main input
!     Read  sub grid concentration filenames:
!      DO 160 IC = 1,NC
!
!      IF (MAINFE) THEN
!          CALL GETDAT(MAINUN,TXTSTR,LEOF)
!          IF (TXTSTR(1:1) .NE. '<') THEN
!              READ (TXTSTR,1000) SCONFN(IC)
!          ENDIF
!      ELSE
!          TXTSTR(1:1) = '<'
!          CALL GETFNV(TXTSTR,SCONFN(IC),SCONFE(IC),1,1,  &
!                     SCONFV(IC))
!      ENDIF
!
!!      IF (MESSFE) THEN
!!          WRITE (MESSUN,2040) IC,
!!     &    (SCONFN(IC)(I:I),I=1,INDEX(SCONFN(IC),' ')-1)
!!      ENDIF
!
!  160 CONTINUE

!     Read receptors concentration filenames:

      IF (MAINFE) THEN
          CALL GETDAT(MAINUN,TXTSTR,LEOF)
          print *,'receptor conc filename',TXTSTR
          IF (TXTSTR(1:1) .NE. '<') THEN
              READ (TXTSTR,1000) RCONFN
          ENDIF
      ELSE
          TXTSTR(1:1) = '<'          
          CALL GETFNV(TXTSTR,RCONFN,RCONFE,1,1,RCONFV)
      ENDIF


!     Read line sources concentration filenames:

      IF (MAINFE) THEN
          CALL GETDAT(MAINUN,TXTSTR,LEOF)
          print *,'line source conc filename',TXTSTR
          IF (TXTSTR(1:1) .NE. '<') THEN
              READ (TXTSTR,1000) LCONFN
          ENDIF
      ELSE
          TXTSTR(1:1) = '<'
          CALL GETFNV(TXTSTR,LCONFN,LCONFE,1,1,LCONFV)

      ENDIF

!!      IF (MESSFE) THEN 
!!	    WRITE (MESSUN,2060)
!!     &    (LCONFN(I:I),I=1,INDEX(LCONFN,' ')-1)
!!	ENDIF
!
      RETURN

 1000 FORMAT (A256)

 2000 FORMAT ('RCONC: Total number of compounds = ',I3)
 2001 FORMAT ('RCONC: Number of compounds not in the chemistry = ',I3)
 2002 FORMAT ('RCONC: Number of these compounds that are advected = ',I3)
 2003 FORMAT ('RCONC: Number of receptor point for Dimensioning CR = ',I6) 
 2010 FORMAT ('RCONC: Too many compounds, maximum = ',I3)
 2020 FORMAT ('RCONC: Compound = ',A10,' Unit = ',A10)
 2021 FORMAT ('RCONC: Compound = ',A10,' Output indicators cdw = ',3I1)
 2022 FORMAT ('RCONC: Compound = ',A10,' Mol. weight = ',F10.3)
 2023 FORMAT ('RCONC: Compound = ',A10,   &
             ' Dry dep. velocity = ',E9.2,' cm/s')
 2024 FORMAT ('RCONC: Compound = ',A10,   &
             ' Wet dep. scavenging ratio = ',E9.2)
 2025 FORMAT ('RCONC: Compound = ',A10,   &
             ' Radioactive half-life = ',E9.2,' s')
 2030 FORMAT ('RCONC: Main grid concentrations compound nr. ',I3,   &
             ' file = ',256A1)
!_NEST_Start:
! 2031 FORMAT ('RCONC: Main grid analyt conc for compound nr. ',I3,
!     &        ' file = ',256A1)
! 2032 FORMAT ('RCONC: Main grid error conc for compound nr. ',I3,
!     &        ' file = ',256A1)
!_NEST_End.
 2040 FORMAT ('RCONC: Sub  grid concentrations compound nr. ',I3,   &
             ' file = ',256A1)
 2050 FORMAT ('RCONC: Receptors    concentrations  file  = ',256A1)
 2060 FORMAT ('RCONC: Line sources concentrations  file  = ',256A1)

!     End of subroutine RCONC

      end subroutine rconc
