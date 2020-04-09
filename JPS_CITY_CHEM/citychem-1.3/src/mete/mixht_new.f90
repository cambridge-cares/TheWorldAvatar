! <mixht_new.f90 - A component of the City-scale
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

      subroutine mixht_new(NST_METHOD,NUST_METHOD,FC,TM, &
                  UST,CL,HKIN,DTHDZ,HMIX)
!   -----------------------------------------------------------------
!        Subroutine for mixing height calculations
!
!        INPUT:  NST_METHOD : Method for calculating neutral/stabe MH
!                NUST_METHOD: Method for calculating unstable MH
!                FC     : The Coriolis parameter.
!                G      : Acceleration due to gravity (9.81 m/s2)
!                UST    : The friction velocity
!                CL     : The Monin-Obukhov length
!                HKIN   : The kinematic turbulent heat flux
!                DTHDZ  : The vertical gradient of the pot. temp.
!                         obove the mixing height.
!
!        OUTPUT: HMIX   : The mixing height for the present hour.
!                         (NOTE: HMIX enters this routine with its value
!                          from the prceeding hour.)
!    --------------------------------------------------------------------
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

      integer :: NTST1,NST_METHOD,NUST_METHOD
      real :: FC,TM,UST,CL,HKIN,DTHDZ,HMIX

!     Local variables:
      integer :: NMAX,n
      real :: KAPPA,G,A,B,C1,C2,C3,C4,C5,C6,ACL,CLCRIT,DT,F1,F2,PROJ,HMIX_MIN
!MSK
      real :: C,C7

        KAPPA = 0.40
        G     = 9.81

        IF(HMIX .LT. 1.0)      HMIX   = 1.0
        IF(DTHDZ .LT. 1.0E-04) DTHDZ  = 1.0E-04

        ACL=ABS(CL)
 
!    *** NOTE: CRITICAL L FOR CHOICE OF FORMULAE (Taken from MEPDIM!)
!      CLCRIT = 3906.25*UST
!
!    *** A more stringent demand would be: |u*/(fL)| < 4 (van Ulden and 
!                                                         Holtslag, 1985)
        CLCRIT = UST / (4.0 * ABS(FC) )
!
!    ---------------------------------------------------------------------
         IF(NST_METHOD .EQ. 1 .AND. NUST_METHOD .EQ. 1)THEN

!        Original MEPDIM:
 
!        Calculations of mixing heigth
         IF(CL.LT.0.0 .OR. ACL.GT.CLCRIT) THEN
!           Unstable or neutral conditions:
            HMIX = 0.25*UST/FC
         ELSE
!	      Stable conditions:
            HMIX = 0.4*SQRT(UST*CL/FC)
         ENDIF

!        End of original MEPDIM.

      ELSEIF(NST_METHOD .GE. 1 .AND. NUST_METHOD .GT. 1)THEN
!    -------------------------------------------------------------------

!        Below either the Original MEPDIM method or the diagnostic 
!        expression of Nieuwstadt (1981) are applied for stable and
!        neutral conditions. Different prognostic formulas are 
!        applied for unstable conditions: either 1) the Encroachment
!        method, or 2) a simplified Batchvarova and Gryning (1991) method,
!        or 3) an advanced Batchvarova and Gryning (1991) method.	 
!
           IF(ACL .GT. CLCRIT) THEN
!           Neutral conditions:
              IF(NST_METHOD .EQ. 1)THEN
!              Original MEPDIM method applied for neutral conditions:
                 HMIX = 0.25*UST/FC
              ELSEIF(NST_METHOD .EQ. 2)THEN
!              Nieuwstadt (1981) for stable and neutral conditions, 
!              see also Seibert et al., (2000).
                 HMIX = SQRT(1.0 +((2.28 * UST)/(FC*ACL)))
                 HMIX = ACL * ( -1.0 + HMIX )/3.8
           ELSE
              PRINT *, ' Wrong NST_METHOD input values.'
              PRINT *, ' PROGRAM TERMINATES'
              STOP
           ENDIF

        ELSEIF(CL .GT. 0.0 .AND. CL .LE. CLCRIT)THEN
!           Stable conditions:
            IF(NST_METHOD .EQ. 1)THEN
!              Original MEPDIM method applied for stable conditions:      
                 HMIX = 0.4*SQRT(UST*CL/FC)
            ELSEIF(NST_METHOD .EQ. 2)THEN
!              Nieuwstadt (1981) for stable and neutral conditions, 
!              see also Seibert et al., (2000). 	
                 HMIX = SQRT(1.0 +((2.28 * UST)/(FC*ACL)))
                 HMIX = ACL * ( -1.0 + HMIX )/3.8
            ELSE
               PRINT *, ' Wrong NST_METHOD input values.'
               PRINT *, ' PROGRAM TERMINATES'
               STOP
            ENDIF
	
        ELSE
!           Unstable conditions:
            IF(NUST_METHOD .EQ. 2)THEN
!             Encroachment method:
                A = 0.0
                B = 0.0
                C = 0.0

            ELSEIF(NUST_METHOD .EQ. 3)THEN
!             Simple B-G method:
                A = 0.2
                B = 2.5
                C = 0.0

            ELSEIF(NUST_METHOD .EQ. 4)THEN
!             Advanced B-G method:
                A = 0.2
                B = 2.5
                C = 8.0

            ELSE

               PRINT *, ' Wrong NUST_METHOD input values.'
               PRINT *, ' PROGRAM TERMINATES'
               STOP

            ENDIF


!           Nieuwstadt (1981), Seibert et al., (2000):
!
!	    Applying Heun's method (2. order Runge Kutta) to solve the
!           prognostic encroachment equation. The encrochment equation is
!           also second order in h: h**2 = Const. * t, so 1 iteration is
!           enough!
!
!           DT = 3600 => NMAX =  1
!           DT = 360  => NMAX = 10
!           DT = 180  => NMAX = 20
!           DT =  90  => NMAX = 40
!

              DT   = 360.0
              NMAX = 10

!           Encroachment method:          A = B = C = 0.0
!           Simple Batchvarova-Gryning:   A = 0.2 
              B = 2.5 
              C = 0.0
!           Advanced Batchvarova-Gryning: A = 0.2 
              B = 2.5 
              C = 8.0

              C1 = (1.0 + 2*A)*HKIN/DTHDZ
              C2 = 2*B*UST*UST*UST*TM/(DTHDZ*G)

            DO n=1,NMAX

               IF(HMIX .LT. 1.0) HMIX = 1.0

!             --------------------------------------------------------------
!             Start..  Encroachment method:
!
!             F1 = HKIN/(DTHDZ * HMIX)
!	        PROJ = HMIX + (DT*F1)
!	        IF(PROJ .LT. 0.001)THEN
!	          PROJ = 0.001
!	          WRITE(NTST1,'(A35)') ' WARNING: PROJ IS LESS THAN 0.001! '
!	        ENDIF
!	        F2 = HKIN/(DTHDZ * PROJ  )
!             HMIX = HMIX + (0.5*DT*(F1+F2))
!
!             Slutt..  Encroachment method:
!             --------------------------------------------------------------
!
!	        Start..  Simple Batchvarova and Gryning, 1991:
!
!             F1 = (C1 / HMIX) + (C2 / (HMIX* HMIX))
!             PROJ = HMIX + (DT*F1)
!
!	        F2 = (C1 / PROJ)   + (C2 / (PROJ  * PROJ  ))
!
!             HMIX = HMIX + (0.5*DT*(F1+F2))
!
!	        Slutt..  Simple Batchvarova and Gryning, 1991:
!             ---------------------------------------------------------------
!
!	        Start..  Advanced Batchvarova and Gryning, 1991:
!
              C3 = (HKIN/DTHDZ)
              C4 = ((1.0 + (2.0*A))*HMIX) - (2.0*B*KAPPA*CL)
              C5 = C*UST*UST*TM

              C6 = ( (1+A)*HMIX - B*KAPPA*CL )*DTHDZ*G
              C7 = HMIX * HMIX

              F1 = C3 / (  (C7/C4) + (C5/C6) )

              PROJ = HMIX + (DT*F1)

              C4 = ((1.0 + (2.0*A))*PROJ) - (2.0*B*KAPPA*CL)

              C6 = ( (1+A)*PROJ - B*KAPPA*CL )*DTHDZ*G
              C7 = PROJ * PROJ

              F2 = C3 / (  (C7/C4) + (C5/C6) )

              HMIX = HMIX + (0.5*DT*(F1+F2))


!	        Slutt..  Advanced Batchvarova and Gryning, 1991.
!
!             We now have calculated HMIX for time previous hour + n * DT
!             and when n=NMAX we are at: previous hour + 3600.

!	        PRINT *, ' HMIX = ',HMIX

              ENDDO

            ENDIF 

! ----------------------------------------------------------------------

        ELSE

              PRINT *, ' Wrong NST_METHOD or NUST_METHOD input values.'
              PRINT *, ' PROGRAM TERMINATES'
              STOP

        ENDIF

        ! for testing:       
        !  HMIX = 250.0

      RETURN
      end subroutine mixht_new
