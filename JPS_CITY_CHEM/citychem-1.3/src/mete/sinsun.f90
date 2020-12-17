! <sinsun.f90 - A component of the City-scale
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

      subroutine sinsun(SINPHI,CLON,CLAT,IMON,IDAY,IHOUR,LTOGMT)      

! ---------------------------------------------------------------------
!
!       Subroutine SINSUN determines the sine of the solar elevation  
!       (Appendix B: Holtslag and van Ulden, 1983, JCAM, 22, 517-529)
!
!          Input:
!              CLON      : Longitude (Degrees, EAST is positive)
!              CLAT      : Latitude  (Degrees, NORTH is positive)
!              IMON      : Month     (1-12, Local Time)
!              IDAY      : Day       (1-31, Local Time)
!              IHOUR     : Hour      (0-24, Local Time)
!              LTOGMT    : Number of hour between GMT and Local Time
!                          LTOGMT = -1 for Norway wintertime.
!                          LTOGMT = -2 for Norway summertime.
!
!          Output:
!              SINPHI    : Sine of solar elevation          
!
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
!    xx Dat 201x  Name: Line  Description of Change
!
! ----------------------------------------------------------------------------------

       implicit none

! *** Global variables:
      INTEGER,INTENT(IN) :: IMON,IDAY,IHOUR,LTOGMT
      REAL,INTENT(IN)    :: CLON,CLAT
      REAL,INTENT(OUT)   :: SINPHI
! *** Local variables 
      INTEGER,PARAMETER :: IMONTH(12)  &
                  = (/0,31,59,90,120,151,181,212,243,273,304,334/)
!      DATA IMONTH /0,31,59,90,120,151,181,212,243,273,304,334 /
      REAL,PARAMETER :: PI    = 3.141592654
      REAL,PARAMETER :: PI180 = 57.29577951

      INTEGER :: JMON,JDAY,JHOUR
      REAL    :: D,RLON,RLAT,TERM,SL,SINDEL,COSDEL,H

! **********************************************************************
! *** Convert to GMT. Assumes that IHOUR = [1,24] !!!!

      JHOUR = IHOUR + 1
      IF(JHOUR == 0)THEN
        PRINT *,'SINSUN: JHOUR is zero, should be 1 - 24'
        PRINT *,'SINSUN: PROGRAM TERMINATES'
        STOP
      ENDIF

        JMON = IMON
        JDAY = IDAY
        JHOUR = JHOUR + LTOGMT
        IF(JHOUR < 1)THEN
          JHOUR = 24 + JHOUR
!       Reduce JDAY by 1:
          JDAY = IDAY -1
          IF(JDAY == 0)THEN
!         Reduce the JMON by 1:
            JMON = JMON - 1 
            IF(JMON == 0) JMON = 12
          ENDIF        
        ELSEIF(JHOUR > 24)THEN
          JHOUR = JHOUR - 24
!       Increase JDAY by 1:
          JDAY = IDAY + 1
!       Since the formula for calculatng the solar height only
!       apply the expression: D=IMONTH(JMON) + JDAY  we only 
!       need to be concerned with the last day of the year.
          JMON = IMON
          IF(JDAY == 32 .AND. JMON == 12) THEN
              JMON = 1
              JDAY = 1
          ENDIF
        ENDIF 
!
! TEST:
!      JMON  = IMON
!      JDAY  = IDAY
!      JHOUR = IHOUR
!
! **********************************************************************
!
! *** Calculate Day-Number from Eq.(B1) H&vU,1983:
      D=IMONTH(JMON) + JDAY

! *** Converting longitude and latitude from degrees to radians:
      RLON=CLON/PI180
      RLAT=CLAT/PI180

! *** Calculating solar longitude (SL) from Eq.(B2) H&vU,1983:
      TERM=0.033*SIN(0.0175*D)
      SL=4.871+0.0175*D+TERM

! *** The sine of the solar declination follows from Eq.(B3) H&vU,1983:
      SINDEL=0.398*SIN(SL)
      COSDEL=SQRT(1-SINDEL**2)

! *** The hour angle (H) is given by Eq.(B4) H&vU,1983:
!     NOTE: In Eq.(B4) (H&vU,1983) this is: H = -RLON + ...., but this
!           is because the formula is assuming western longitude.
!     NOTE: IHOUR should be the "universal time in hours", i.e GMT.  
      H=RLON+0.043*SIN(2*SL)-TERM+0.262*JHOUR-PI

! *** The sine of the solar elevation is given by Eq.(B5) H&vU,1983: 
      SINPHI=SINDEL*SIN(RLAT)+COSDEL*COS(RLAT)*COS(H)

      RETURN
      end subroutine sinsun
      
