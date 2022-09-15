! <rbcon.f90 - A component of the City-scale
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

      subroutine RBCON

!******************************************************************
! ***
! *** The subroutine reads background concentrations from files.
! ***
! *** MSK 17.09.2019: Three methods for background concentrations
! ***                 will be supported from now on:
! *** 1) bcictype==1
! ***    Read constant value from main input file and homogeneously
! ***    distribute over 3D main grid
! *** 2) bcictype==2
! ***    Read hourly value from ebas input file or constant value
! ***    from main input file and homogeneously distribute over 
! ***    3D main grid. (Alternative Sondes_method is not used)
! *** 3) bcictype==3
! ***    Read hourly 3D BC file as input field for the 3D main grid
! ***
! ***
!******************************************************************
!       2016 Matthias Karl, HZG, CityChem extension:
!            With option bcictype==3 a full 3-D BCON field (not only boundaries)
!            from a CMAQ concentration output file is read using routine
!            r4dbcfld every hour for all compounds ic
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
!           2017  M. Karl: Possible to offset BCON values from 3D BCON file
!    18 Apr 2018  M. Karl: L261      Add scalepm to PM-compounds from 3D BCON file
!    17 Sep 2019  M. Karl: L319-L328 Delete constant BCON value for Sulphate
!    17 Sep 2019  M. Karl:           Restructured to allow three BCON methods
!
! ----------------------------------------------------------------------------------

      USE mod_util
      USE mod_main
      USE mod_site
      USE mod_time
!_NEST_Start:
      USE mod_mete
!_NEST_End.
      USE mod_conc
      USE mod_grid
!MSK start
      use mod_writenc
!MSK end

!!DEC$ IF DEFINED (emep68)
!_LHS_SOA_June_2007_Start:
!      USE My_SOA_ml,  only : FIRST_NONVOL, LAST_SOA
!_LHS_SOA_June_2007_End.
!!DEC$ ENDIF

      implicit none

! *** Declaration of local variables:

!MSK      REAL         :: BCMINV
      double precision  :: BCMINV
      REAL         :: scal
      INTEGER      :: IC
      INTEGER      :: K
      INTEGER      :: I
      INTEGER      :: J
      CHARACTER(len=10) :: TEXT1
      CHARACTER(len=10) :: TEXT2
      LOGICAL      :: LEOF

!_NEST_Start:
!MSK      LOGICAL      :: ATDATE
!MSK      LOGICAL      :: ATTIME
!_NEST_End.

! *** BCMINV - Background concentration minimum value
! *** IC     - Index of compound
! *** K      - Dummy index
! *** LEOF   - If end of file then true else false
! *** ATDATE - If at given date          then true else false
! *** ATTIME - If at given date and time then true else false


      INTEGER           :: m_emep
      INTEGER           :: d_emep
      INTEGER           :: h_emep

!MSK start EBAS file
      integer           :: ICNO
      real              :: BC_start
      real              :: BC_end
      real              :: BC_VAL
      real              :: BC_FLAG
      real              :: PMBC_VAL
      real              :: O3BC
      real              :: NOBC
      real              :: SUNHR
      real,parameter    :: ebasmiss1 = 999.99
      real,parameter    :: ebasmiss2 = 9999.99
      real,parameter    :: ebasmiss3 = 9999.9
!MSK end

      REAL, ALLOCATABLE :: BC_COL(:)

      LOGICAL           :: EMEP_COLOUMN_EXIST
      LOGICAL           :: Sondes_method

!MSK start netCDF output
      integer :: IX, IY
      real    :: gridwidth
      real,   allocatable :: sxm_i(:,:)
      real,   allocatable :: sxm_j(:,:)
!MSK end

! *** End declaration of local variables.


!MSK 26.01.2016 EMEP column and sondes will not be used
!      EMEP_COLOUMN_EXIST = .TRUE.
      EMEP_COLOUMN_EXIST = .false.
!MSK 17.09.2019 EMEP sondes not used
!      Sondes_method      = .TRUE.
      Sondes_method      = .false. 


!MSK start
! CREATE NETCDF OUTPUT FILES
! *** Open diagnostic 2-D output netCDF file for photochemistry
!     Selected chemical species will be written to the file
      if (ATTIME(BDAT) .AND. ITS .EQ. 1)  then

! Allocate
        if (.not. allocated(sxm_i) )    allocate(sxm_i(NY,NX))
        if (.not. allocated(sxm_j) )    allocate(sxm_j(NY,NX))

        gridwidth = DX
        !**  create a grid with the x- and y- coordinates of the main grid centre-points
        do IX = 1, NX
          do IY = 1, NY
            sxm_i(IY,IX) = SITEX0 + (IX-1)*DX 
            sxm_j(IY,IX) = SITEY0 + (IY-1)*DY
          enddo
        enddo

!  HOURLY 2D
        if (NH1GRIDFE) then
            call CreateNCfileGrid(NH1GRIDFN, NX, NY, NZ, sxm_i, sxm_j, Z ,gridwidth, utmzone,EPSGN,SITEX0,SITEY0)
        endif

! *** Open restart file for 3-D output of main grid concentration
!     At last time step of simulation

!  HOURLY 3D
        if (IC1GRIDFE) then
          call CreateNCfileGrid(IC1GRIDFN, NX, NY, NZ, sxm_i, sxm_j, Z ,gridwidth, utmzone,EPSGN,SITEX0,SITEY0)
        endif

! Deallocate
        if (allocated(sxm_i))     deallocate(sxm_i)
        if (allocated(sxm_j))     deallocate(sxm_j)


      endif   !ATTIME(BDAT), ITS=1

! END CREATE NETCDF OUTPUT FILES
!MSK end

! *** Go through all compounds:

      DO 100 IC = 1,NC

!MSK 17.09.2019: Option bcictype 3
!MSK 26.01.2016: We read the hourly 3D BC files

        if (bcictype == 3) then

!MSK start chose binary format to save disk space
          BCONFM(IC) = 2

! ***   Open boundary condition file:
          if (ATTIME(BDAT) .AND. ITS .EQ. 1)  then
             CALL OPIFIL(BCONFN(IC),BCONUN(IC),BCONFE(IC),BCONFM(IC))
          endif

!    check if 3-D BC file is not existing
          if (.not.BCONFE(IC)) then
             print *,'rbcon: BC 3-D FILE for cmp. ',IC,' is missing'
             CALL STOPIT('RBCON: BC 3-D FILE NOT EXISTING')
          endif
!MSK end


! ***   Check if file exists: (For AirQUIS applications "BCONFE(IC)" = .FALSE.)

!MSK 17.09.2019     IF (BCONFE(IC) .AND. .NOT. EMEP_COLOUMN_EXIST) THEN

! ***     Yes, boundary condition file exist, but it is not an EMEP column data set.
!
! ***     Read background concentration from 3-D file:
!
! ***     Below we read the boundary values of the four dimensional field
! ***     BC(IC,I,J,K). This variable is defined with spatial dimensiones:
! ***     I = 1-NBCX,NX+NBCX, J = 1-NBCY,NY+NBCY and K = 1,NZ+NBCZ.
! ***     A local variable within R4DBCFLD, LOGICAL JUST_BV, decide whether
! ***     just the boundary values are read or if the whole field is read
! ***     into BC(IC,I,J,K). If only the boundary values are read the internal
! ***     domain values can be whatever (these should be set equal to MISS).

! ***     The file-extension is deciding the file format.


!MSK          DO K=1,NZ+NBCZ

!MSK            CALL R4DBCFLD(BCONUN(IC),BCONFM(IC),IC,K,TEXT1,TEXT2,  &
            CALL R4DBCFLD(BCONUN(IC),BCONFM(IC),IC, TEXT1,TEXT2,  &
                                     NC,NX,NY,NZ,NBCX,NBCY,NBCZ,  &
                                     MC,MX,MY,MZ,MBCX,MBCY,MBCZ,BC)

!MSK start Set Offset for O3-concentration BCON

            if (cmpnd(ic)(1:2) == 'O3' ) then
              do K=1,NZ+NBCZ
                do I=1,NX+2*NBCX
                  do J=1,NY+2*NBCY
                    BC(IC,I,J,K) = BC(IC,I,J,K) + scaleo3
                  enddo
                enddo
              enddo
            endif
            if (cmpnd(ic)(1:2) == 'PM' ) then
              do K=1,NZ+NBCZ
                do I=1,NX+2*NBCX
                  do J=1,NY+2*NBCY
                    BC(IC,I,J,K) = BC(IC,I,J,K) + scalepm
                  enddo
                enddo
              enddo
            endif

!MSK 04.09.2017 Set BCON for H2SO4 at surface to 0.00001 ug/m^3
!MSK 17.09.2019 BCONCC2.1 produces 3-D bcon of H2SO4
!MSK          if (cmpnd(ic)(1:8) == 'Sulphate' ) then
!MSK            do I=1,NX+2*NBCX
!MSK              do J=1,NY+2*NBCY
!MSK                BC(IC,I,J,1) = 0.00001
!MSK              enddo
!MSK            enddo
!MSK          endif
!MSK end

!MSK start
! ***     If not first timestep of the simulation,
! ***     Only fill the surrounding border and the NZ+NBCZ layer with values, 
! ***     zero in the rest of the main grid

            if ( .not. ( ATTIME(BDAT) .and. ITS .EQ. 1)  ) then

              do K=1,NZ
                do I=1+NBCX,NX+NBCX
                  do J=1+NBCY,NY+NBCY
                    BC(IC,I,J,K) = 0.0
                  enddo
                enddo
              enddo

            endif

!MSK end

!MSK          ENDDO

!MSK test print
!        DO K=1,NZ+NBCZ
!          DO I=1,NX+2*NBCX
!            DO J=1,NY+2*NBCY
!              print *,'rbcon: BC ',IC,I,J,K, BC(IC,I,J,K) 
!            ENDDO
!          ENDDO
!        ENDDO

        else if (bcictype == 2) then


!_LHS_EMEP_August_2007_Start:

!MSK 26.01.2016: EMEP_COLUM_EXIST is false, will not enter the below branch
!MSK 17.09.2019     ELSEIF (BCONFE(IC) .AND. EMEP_COLOUMN_EXIST) THEN

! ***     Read background concentration from 1D-EMEP file:

            IF (Sondes_method) THEN

! ***       Alternative 1: Version based on Davids Sondes.in file:

              ALLOCATE (BC_COL(NZ+NBCZ))

              CALL NXTDAT(BCONUN(IC),LEOF)
              IF(LEOF)THEN
                CALL STOPIT('RBCON: Unexpected EOF read!')
              ENDIF
              READ (BCONUN(IC),*) m_emep,d_emep,h_emep,  &
                              (BC_COL(K),K=1,NZ+NBCZ)
              DO K = 1,NZ+NBCZ
                DO I = 1-NBCX,NX+NBCX
                  DO J = 1-NBCY,NY+NBCY

! ***_Variable:   If the coloumn-values are applied:
                  BC(IC,I,J,K) = BC_COL(K)
! ***_Constant:   If only the value from the lowermost layer is applied:
!                  BC(IC,I,J,K) = BC_COL(1)
! ***_Constant:   If the average of some of the lowermost layer values 
!                 are applied:
!                  BC(IC,I,J,K) = 0.5*(BC_COL(1) + BC_COL(2))

                  ENDDO
                ENDDO
              ENDDO

              DEALLOCATE (BC_COL)

            ELSE

!MSK 17.09.2019: Option bcictype 2
!MSK             Alternative 2: Version based on the Peter Wind input
!MSK             We read the hourly 1D BC file from EBAS
!MSK             We should have at least O3, NO2 and PM2.5
!MSK             If no ebas file, we use BC=0
!MSK             Calculate PM10 based on PM2.5
!MSK             Calculate NO based on PSS


! ***   Open ascii boundary condition file at first time step
              if (ATTIME(BDAT) .AND. ITS .EQ. 1)  then
                BCONFM(IC) = 1
                if (BCONFE(IC)) then
                  CALL OPIFIL(BCONFN(IC),BCONUN(IC),BCONFE(IC),BCONFM(IC))
                else
! ***   BC files for O3, NO2 and PM2.5 must be provided
                   if (cmpnd(ic)(1:2) == 'O3' ) then
                      print *,'rbcon: BC 1-D FILE for cmp. ',IC,' is missing'
                      CALL STOPIT('RBCON: BC 1-D FILE NOT EXISTING')
                   endif
                   if (cmpnd(ic)(1:3) == 'NO2' ) then
                      print *,'rbcon: BC 1-D FILE for cmp. ',IC,' is missing'
                      CALL STOPIT('RBCON: BC 1-D FILE NOT EXISTING')
                   endif
                   if (cmpnd(ic)(1:5) == 'PM2.5' ) then
                      print *,'rbcon: BC 1-D FILE for cmp. ',IC,' is missing'
                      CALL STOPIT('RBCON: BC 1-D FILE NOT EXISTING')
                   endif
                endif
              endif

! ***   Accept missing BC file, set BC_VAL to zero if no file
              if (BCONFE(IC)) then
                 CALL NXTDAT(BCONUN(IC),LEOF)
                 READ (BCONUN(IC),*) BC_start,BC_end,BC_VAL,BC_FLAG
                 print *,'BC_VAL for IC ',IC,BC_VAL
              else
                 BC_VAL = 0.0
              endif
! ***   Avoid negative or missing values
              if (BC_VAL .LT. 0.) BC_VAL = 0.0
! ***   Store BC value of PM2.5 to calculate PM10
              if (cmpnd(ic)(1:5) == 'PM2.5' ) then
                 PMBC_VAL = BC_VAL
              endif
              if (cmpnd(ic)(1:4) == 'PM10' ) then
                 BC_VAL = 1.5 * PMBC_VAL
              endif
! ***   Calculate BC value of NO based on PSS assumption
!         Seinfeld & Pandis 2006, pp.210
!         jNO2/k3 = 10ppb
!         [NO] = 10ppb * [NO2]/[O3]
!         [NO]ug/m3 = 0.735 * [NO]ppb
              if (cmpnd(ic)(1:2) == 'O3' ) then
                 O3BC = BC_VAL
              endif
              if (cmpnd(ic)(1:3) == 'NO2' ) then
                 NOBC = 10.0*BC_VAL/(max(O3BC,1.0))
                 NOBC = NOBC * 0.735
              endif
              if (cmpnd(ic)(1:3) == 'NO ' ) then
                 ICNO = IC
              endif

              DO K = 1,NZ+NBCZ
                DO I=1, NX+2*NBCX
                  DO J=1, NY+2*NBCY

                    BC(IC,I,J,K) = BC_VAL
                    if ( (BC(IC,I,J,K) .eq. ebasmiss1).or.   &
                         (BC(IC,I,J,K) .eq. ebasmiss2).or.   &
                         (BC(IC,I,J,K) .eq. ebasmiss3)  ) then
                      BC(IC,I,J,K) = 0.0
                    endif

! ***   Photostationary state assumption for NO
                    if (IC .eq. NC) then
                      BC(ICNO,I,J,K) = NOBC
                    endif

                  ENDDO
                ENDDO
              ENDDO

            ENDIF  ! end input from EBAS-emep file.



!MSK        ELSE
        else if (bcictype == 1) then

!MSK 26.01.2016: Option bcictype 1
!MSK             We use the constant value from main input file

! ***     Boundary condition file does not exist, i.e., BCONFE(IC) = .FALSE.  

! ***     Main data file with value exists? (This is not the AirQUIS case).

          IF (MAINFE .AND. BCONFV(IC) .NE. MISS) THEN

! ***       Setting homogeneous background concentrations for all compounds:

            DO K=1,NZ+NBCZ
!MSK              DO I=1-NBCX,NX+NBCX
!MSK                DO J=1-NBCY,NY+NBCY
              DO I=1, NX+2*NBCX
                DO J=1, NY+2*NBCY

                  BC(IC,I,J,K) = BCONFV(IC)

                ENDDO
              ENDDO
            ENDDO
            !print *,'BC(pnc) 1,1,1): ',BC(IC,1,1,1)

!MSK start TEST
! ***     Set BC in top-most layer to zero
!            BC(:,:,:,NZ+NBCZ) = 0.0 
!MSK start TEST

!MSK start
! ***     If not first timestep of the simulation,
! ***     Only fill the surrounding border and the NZ+NBCZ layer with values, 
! ***     zero in the rest of the main grid

          if ( .not. ( ATTIME(BDAT) .and. ITS .EQ. 1)  ) then

            do K=1,NZ
              do I=1+NBCX,NX+NBCX
                do J=1+NBCY,NY+NBCY
                  BC(IC,I,J,K) = 0.0
                enddo
              enddo
            enddo

          endif

!MSK end

          ENDIF    ! (MAINFE .AND. BCONFV(IC) .NE. MISS) 

        endif    ! bcictype


! ***   In AirQUIS (both SA and Integrated) applications, only the lines below are executed:

! ***   Checking data: 

! ***   Define background concentration minimum value:

        BCMINV = 0.
        IF (CUNIT(IC)(1:7) .EQ. 'mol/cm3') BCMINV = 1.E-20
        
!_NEST_Start:
! ***   Missing data is set equal to zero:

        DO K=1,NZ+NBCZ
!MSK          DO I=1-NBCX,NX+NBCX
!MSK            DO J=1-NBCY,NY+NBCY
          DO I=1, NX+2*NBCX
            DO J=1, NY+2*NBCY

              IF( BC(IC,I,J,K) .EQ. MISS) BC(IC,I,J,K) = 0.

            ENDDO
          ENDDO
        ENDDO



! ***   Negative data is not tolerated:
        DO K=1,NZ+NBCZ
!MSK          DO I=1-NBCX,NX+NBCX
!MSK            DO J=1-NBCY,NY+NBCY
          DO I=1, NX+2*NBCX
            DO J=1, NY+2*NBCY

              IF (BC(IC,I,J,K) .LT. 0.) THEN
                IF (MESSFE) WRITE (MESSUN,2010)  &
                   BC(IC,I,J,K),CMPND(IC)(1:10)
                CALL STOPIT('RBCON: Negative background concentration!')
              ENDIF

            ENDDO
          ENDDO
        ENDDO

! ***   Take into consideration the minimum value defined:
        DO K=1,NZ+NBCZ
!MSK          DO I=1-NBCX,NX+NBCX
!MSK            DO J=1-NBCY,NY+NBCY
          DO I=1, NX+2*NBCX
            DO J=1, NY+2*NBCY

              BC(IC,I,J,K) = MAX(BC(IC,I,J,K),BCMINV)
              !print *,'rbcon: BC ',IC,I,J,K, BC(IC,I,J,K) 

            ENDDO
          ENDDO
        ENDDO
!_NEST_End.


!_LHS_SOA_June_2007_Start:

!!DEC$ IF DEFINED (emep68)
!
!       IF( IC == LAST_SOA + 1 )THEN
!! ***    Calculate the boundary value of orgPM2p5 as the sum of
!! ***    primary NON-VOLATILEs at the boundaries: Since we only
!! ***    consider the NON-VOLATILE contribution we somewhat under-
!! ***    estimate the boundary values of the total organic PM2p5.
!
!! ***    Scaling factor used in the scaling from [molecules/cm3] 
!! ***    to [ug/m3]:
!
!         scal = 1.0E+12/avogad
!
!         DO K=1,NZ+NBCZ
!           DO I=1-NBCX,NX+NBCX
!             DO J=1-NBCY,NY+NBCY
!
!               BC(IC,I,J,K) = BC(IC,I,J,K) + scale *   &
!                (  BC(FIRST_NONVOL,I,J,K)  *CMOLW(FIRST_NONVOL)  &
!                 + BC(FIRST_NONVOL+1,I,J,K)*CMOLW(FIRST_NONVOL+1)  &
!                 + BC(FIRST_NONVOL+2,I,J,K)*CMOLW(FIRST_NONVOL+2)  &
!                 + BC(FIRST_NONVOL+3,I,J,K)*CMOLW(FIRST_NONVOL+3) )!
!
!             ENDDO
!           ENDDO
!         ENDDO
!
!       ENDIF  ! End of ELSE branch of: IF (IC == LAST_SOA + 1 = 69) THEN

!!DEC$ ENDIF (emep68)

!_LHS_SOA_June_2007_End.

      if (messfe) then
        write(messun,*) 
        write(messun,'(A,E12.5)')      &
!MSK         'RBCON: BC(IC, 1-NBCX, 1-NBCY,1)       = ',BC(IC,1-NBCX,1-NBCY,1)
         'RBCON: BC(IC, 1, 1,1)       = ',BC(IC,1,1,1)
        write(messun,'(A,E12.5)')      &
!MSK         'RBCON: BC(IC,NX+NBCX,NY+NBCY,1)       = ',BC(IC,NX+NBCX,NY+NBCY,1)     
         'RBCON: BC(IC,NX+2*NBCX,NY+2*NBCY,1)       = ',BC(IC,NX+2*NBCX,NY+2*NBCY,1)   
        write(messun,'(A,E12.5)')      &
!MSK         'RBCON: BC(IC, 1-NBCX, 1-NBCY,NZ+NBCZ) = ',BC(IC,1-NBCX,1-NBCY,NZ+NBCZ)
         'RBCON: BC(IC, 1, 1, NZ+NBCZ) = ',BC(IC,1,1, NZ+NBCZ)
        write(messun,'(A,E12.5)')      &
!MSK         'RBCON: BC(IC,NX+NBCX,NY+NBCY,NZ+NBCZ) = ',BC(IC,NX+NBCX,NY+NBCY,NZ+NBCZ)     
         'RBCON: BC(IC,NX+2*NBCX,NY+2*NBCY,NZ+NBCZ) = ',BC(IC,NX+2*NBCX,NY+2*NBCY,NZ+NBCZ)  
      endif

! ***   Close file:
      IF(ATDATE(EDAT))      &
          CALL CLIFIL(BCONFN(IC),BCONUN(IC),BCONFE(IC),BCONFM(IC))

  100 CONTINUE    ! IC loop

      RETURN

 2000 format ('RBCON: Read background    conc. for compound',A10)
 2010 format ('RBCON: Background    conc. BC = ',E10.1,1X,A10,      &
             ' for comp. ',A10,' is negative!')
 2020 format ('RBCON: BCONFV = ',E12.5,' for comp. ',A10)

! *** End of subroutine RBCON

      end subroutine rbcon
