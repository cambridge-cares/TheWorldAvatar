! <turb_ebudget.f90 - A component of the City-scale
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
!*****************************************************************************! 

      subroutine turb_ebudget(ANALYTIC,G,SIGMA,D_ADIAB,KAPPA,CP,RHO_AIR,  &
                             LAMBDA_E,S_SLOPE,TAY_PRI,AG_DAY,AG_NIGHT,   &
                             THETA_D,U1,U2,ZU1,ZU2,Z0H,ZR,TM,RNN,RKIN,   &
                             RALB,IMON,IDAY,IHOUR,LTOGMT,CLON,CLAT,      &
                             LOWER_LIM_L,UST,TST,QST,CL)
       
! ----------------------------------------------------------------------
!
!     SUBROUTINE TURB_EBUDGET computes FRICTION VELOCITY from wind 
!     measurements at two heights and parameterized surface heat flux
!    (van Ulden and Holtslag, 1985).
!
!     The DYER AND HICKS RELATIONS ARE USED MODIFIED BYE
!     HOLTSLAG FOR VERY STABLE SITUATIONS (SEE WESSELS, 1984,
!     KNMI REPORT WR 84-6; SEE ALSO STABILITY FUNCTIONS
!     PSIM AND PSIH).
!
!     INPUT:
!           ANALYTIC    : If ANALYTIC is .TRUE. the Holtslag (1984) 
!                         analytic solution is applied with use of 
!                         the constant value of 0.08 K for theta_star.
!                         If ANALYTIC is .FALSE. the given LOWER_LIM_L
!                         is applied as a lower boundary of the positive 
!                         values of L. This value should be rather close
!                         to ZU2, since this will give Z/L close to 1,
!                         i.e. the border of the validity of the applied
!                         M-O theory. 
!            G          : Acceleration due to gravity (9.81 m/s2).
!            SIGMA      : Stefan-Boltzmanns constant (5.67E-8 Jm-2K-4)
!            D_ADIAB    : The dry_adiabatic temp. gradient (0.01 K/m).
!            KAPPA      : von Karmans constant (0.4).
!            CP         : Specific heat at constant pressure.
!            RHO_AIR    : Air density.
!            LAMBDA_E   : Heat of vaporization of liquid water.
!            S_SLOPE    : Slope of the sat. specific humidity with T.
!            TAY_PRI    : Taylor-Priestly constant [0=dry,1=wet].
!            AG_DAY     : Daytime Soil conduction parameter 
!                         (5 for grass; in vU&H,85)
!            AG_NIGHT   : Nighttime Soil conduction parameter
!                         (5 for grass; in vU&H,85)
!            THETA_D    : Temperature scale (0.033 in vU&H,85).
!
!            U1, U2     : Wind speed at ZU1 and ZU2 [m/s]
!                         (e.g. U1 = 0. and U2 = U10 for ZU2 = 10.
!                          and ZU1 = Z0.)
!            ZU1, ZU2   : Observ. Heights for Wind (ZU2 > ZU1) [m]
!                         (e.g. Z0 and 10. respectively.)
!            Z0H        : Surface roughness for heat (temperature) [m]
!                         (e.g. should be approx: 0.01*z0.
!            ZR         : Height of the TM-temperature below. [m]
!            TM         : Ambient temp. (rough estimate is OK) [K]
!            RNN        : Cloud cover fraction  [0.- 1.]
!            RKIN       : Incoming shortwave radiation 
!                         (-9900. if missing; parameterized by RADIAT)
!            RALB       : Surface albedo, Net SW: RKIN*(1-RALB)
!                         Surface albedo:    
!                               SURFACE      ALBEDO(%)   RALB
!                       1  :   DARK SOIL       10         0.10
!                       2  :   FOREST          15         0.15
!                       3  :   GRASS,LUSH      20         0.20
!                       4  :   SAND            25         0.25
!                       5  :   ICE,OLD         30         0.30
!                       6  :   ICE,NEW         35         0.35
!                       7  :   SNOW,OLD        40         0.40
!                       8  :   SNOW,NORMAL     60         0.60
!                       9  :   SNOW,FRESH      80         0.80
!
!           IMON        : Month (Local Time)
!           IDAY        : Day (LocalTime)
!           IHOUR       : Hour (Local Time)
!           LTOGMT      : Hour beteen GMT and Local Time (-1 for Norway wintertime)
!           CLON        : Longitude for current location
!           CLAT        : Latitude for current position
!           LOWER_LIM_L : User given lower boundary on the positive
!                         values of the Monin-Obukhov length.
!
!      Local Variables:
!            SINPHI     : Sine of solar elevation (from SINSUN)
!            QSTI       : Isothermal net radiation(from RADIAT)
!
!     OUTPUT:
!            UST        : Velocity scale                       [m/s]
!            TST        : Temperature scale                    [K]
!            QST        : Humidity scale                       [g/kg]
!            CL         : Monin-Obukhov length                 [m]
!
!
!                                           11-09-2006  L. H. Slørdal
! ---------------------------------------------------------------------
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
!           2016  M. Karl: Functions PSIM,TST_EBUDGET,OBUK are now in mod_mete.f90
!                          Modifications to use double precision variables
!
! ----------------------------------------------------------------------------------

!MSK start
        use mod_mete
!MSK end

        implicit none

!     External functions:
!MSK      REAL PSIM,TST_EBUDGET,OBUK
!MSK  functions are in mod_mete.for now

!     Global variables:
      logical,intent(IN) :: ANALYTIC
      integer,intent(IN) :: IDAY,IMON,IHOUR,LTOGMT
      real,intent(IN)    :: G,SIGMA,D_ADIAB,KAPPA,CP,RHO_AIR,LAMBDA_E,   &
                           S_SLOPE,TAY_PRI,AG_DAY,AG_NIGHT,THETA_D,     &
                           U1,U2,ZU1,ZU2,Z0H,ZR,TM,RNN,RKIN,RALB,       &
                           CLON,CLAT,LOWER_LIM_L
      real,intent(OUT)   :: UST,TST,QST,CL

!	Local variables:
      integer :: N,NRIT
      real    :: ALFA,SINPHI,QSTI,CLP,ZETA,CL0,TCL,CLN,DL,ADL,ADL_LIM
             
! ----------------------------------------------------------------------

!_ORG:      NRIT=20
      NRIT    = 50
!_ORG:      ADL_LIM = 0.05      
      ADL_LIM = 0.005
            
      ALFA    = 5.0

      IF(RKIN < 0.0)THEN
! ***   Calculation of solar elevation; Time input is given as LOCAL TIME.
! ***   LTOGMT is the number of hours between LOCAL TIME and GMT/ZULU.
! ***   Examples:  LTOGMT = -1  for Norway in wintertime.
! ***              LTOGMT = -2  for Norway in summertime.
! ***              LTOGMT = -4  for ABU DHABI summertime.  
        call sinsun(SINPHI,CLON,CLAT,IMON,IDAY,IHOUR,LTOGMT)
      ENDIF
!
! *** Initialise for Surface Flux Parameterisation
      call radiat(SIGMA,SINPHI,TM,RNN,RKIN,RALB,QSTI) 

!_ORG:      CL   = 36.0
      CL   = 1.0E5
!_ORG:      IF (QSTI .LT. 0.) CL = -36.0 
      IF (QSTI .LT. 0.) CL = -1.0E5

!MSK        UST=(U2-U1)*KAPPA/( ALOG(ZU2/ZU1)-PSIM(ZU2/CL)+PSIM(ZU1/CL))
        UST=(U2-U1)*KAPPA/( ALOG(ZU2/ZU1)-PSIM(DBLE(ZU2/CL))+PSIM(DBLE(ZU1/CL)))
        TST=TST_EBUDGET(QST,SIGMA,D_ADIAB,KAPPA,CP,RHO_AIR,LAMBDA_E,    &
                     S_SLOPE,TAY_PRI,AG_DAY,AG_NIGHT,THETA_D,          &
                     UST,Z0H,ZR,TM,QSTI)

      DO 20 N=1,NRIT
!_ORG:    UST=(U2-U1)*KAPPA/( ALOG(ZU2/ZU1)-PSIM(ZU2/CL)+PSIM(ZU1/CL))
!_ORG:    TST=TST_EBUDGET(QST,SIGMA,D_ADIAB,KAPPA,CP,RHO_AIR,LAMBDA_E,
!_ORG     &               S_SLOPE,TAY_PRI,AG_DAY,AG_NIGHT,THETA_D,
!_ORG     &               UST,Z0H,ZR,TM,QSTI)
          CLP  = CL
!MSK          CL   = OBUK(KAPPA,G,UST,TST,TM)
          CL   = OBUK(UST,TST,TM)
          ZETA = ZU2/CL
        IF(ANALYTIC)THEN
            IF(ZETA .GT. 1.0) THEN
! ***       Very stable, critical CL
              TST = 0.09*(1.-0.5*RNN*RNN)
              CL0 = ALFA*ZU2/ALOG(ZU2/ZU1) 
              CLN = KAPPA*U2*U2*TM/(2.0*G*TST*ALOG(ZU2/ZU1)*ALOG(ZU2/ZU1))
              TCL = 2.0*CL0
              IF(CLN >= TCL) THEN
                  CL = (CLN-CL0) + SQRT(CLN*CLN - 2.0*CL0*CLN)
              ELSE
                  CL = SQRT(0.5*CL0*CLN)
              ENDIF
! ****      Critical CL=CL0, No extrapolation below CL0
              IF(CL < CL0) CL=CL0
! ***       Updating UST with the final CL value (TST = 0.09*(...)):
!MSK            UST=(U2-U1)*KAPPA/( ALOG(ZU2/ZU1)-PSIM(ZU2/CL)+PSIM(ZU1/CL))
            UST=(U2-U1)*KAPPA/( ALOG(ZU2/ZU1)-PSIM(DBLE(ZU2/CL))+PSIM(DBLE(ZU1/CL)))
            ENDIF  ! IF(ZETA > 1.0)
          ELSE  ! Now we are not applying the analytic solution:
          IF(CL .GT. 0 .AND. CL .LT. LOWER_LIM_L) CL = LOWER_LIM_L
! ***     Updating UST and TST with the final CL value:
!MSK            UST=(U2-U1)*KAPPA/( ALOG(ZU2/ZU1)-PSIM(ZU2/CL)+PSIM(ZU1/CL))
            UST=(U2-U1)*KAPPA/( ALOG(ZU2/ZU1)-PSIM(DBLE(ZU2/CL))+PSIM(DBLE(ZU1/CL)))
            TST=TST_EBUDGET(QST,SIGMA,D_ADIAB,KAPPA,CP,RHO_AIR,LAMBDA_E,   &
                         S_SLOPE,TAY_PRI,AG_DAY,AG_NIGHT,THETA_D,         &
                         UST,Z0H,ZR,TM,QSTI)           
        ENDIF
        DL  = (CL-CLP)/CLP
        ADL = ABS(DL)
        IF(ADL < ADL_LIM) GO TO 25
  20  CONTINUE
!_LHS:      PRINT *, 'NOT ACHIEVED CONVERGENCE in TURB_EBUDGET.'
!_LHS:      STOP

  25  CONTINUE

      RETURN
      end subroutine turb_ebudget



