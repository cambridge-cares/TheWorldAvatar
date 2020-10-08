      SUBROUTINE UTMGEO (KOD,IZONIN,IZONOUT,EAST,NORTH,SLON,SLAT,
     &                   ISPHER)
C***********************************************************************
C     SUBROUTINE UTMGEO (KOD,IZONE,EAST,NORTH,SLON,SLAT,IS)
C
C     The latitude and longitude are returned seconds, not decimal 
C     degrees or other variation
C
C     The exact origins of this routine are unknown, but it is
C     believed to be based on some USGS conversion programs.  RWB
C
C     THIS PROGRAM PERFORMS THE FOLLOWING COMPUTATIONS--
C
C     1.  GEOGRAPHIC TO UTM COORDINATES
C         (ANY ZONE MAY BE EXTENDED THE FULL ZONE WIDTH OF EITHER
C         Adjacent ZONE--9 DEGREES FROM THE CENTRAL MERIDIAN--BY
C         PUNCHING THE ZONE NUMBER WITH THE GEOGRAPHIC COORDINATES.
C         IF NO ZONE NUMBER IS INPUT, THE NORMAL 6-DEGREE BAND IS USED)
C
C     2.  UTM TO GEOGRAPHIC COORDINATES
C         (WHEN CONVERTING PTS IN THE SOUTHERN HEMISPHERE, THE ZONE
C         NUMBER IS PRECEDED BY A MINUS SIGN.  EXTENDED ZONE BOUNDARIES
C         MAY BE USED, AS ABOVE)
C
C     FOR ANY OF THE ABOVE, ONE OF FIVE SHEROIDS MAY BE SELECTED BY
C     AND CONVERGENCE ANGLE MAY BE REQUESTED BY A 1 IN COL 80.
C     AN INTEGER IN COL 60 (CLARKE 1866 IS DEFAULT).  SCALE FACTOR
C
C
C     NB:
C     WITHIN UTMGEO, SOUTHERN HEMISPHERE LATITUDES AND EASTERN HEMISPHERE 
C     LONGITUDES ARE CONSIDERED NEGATIVE.  CALLING ROUTINES IN AERMOD
C     MODEL COMPONENTS ASSUME NEGATIVE FOR WEST LONGITUDE. THE ADJUSTMENT
C     TO POSITIVE FOR WEST LONGITUDE IS MADE UPON ENTERING UTMGEO, AND 
C     RESULTS ARE ADJUSTED BACK TO NEGATIVE FOR WEST BEFORE LEAVING UTMGEO.
C-----------------------------------------------------------------------

      IMPLICIT NONE

      DOUBLE PRECISION :: SLAT,SLON,NORTH,EAST,A(16),B(4),UTZ,SK,THET
      INTEGER :: ISPHER, KOD, IZONE, IZON2, IERR, IDLON
      INTEGER :: IZONIN, IZONOUT

C     DETERMINE SPHEROID PARAMETERS FROM ISPHER INPUT
C                            0=CLARK 1866(DEFAULT) / NAD 27
C                            1=CLARK1880
C                            2=BESSEL
C                            3=MODIFIED MURCURY 1968
C                            4=GRS 80 / NAD 83

C     If a spheroid is required that is not defined here, it is rec-
C     ommended that the appropriate semi-major and semi-minor axes
C     values be substituted for one of the unused parameter sets.
C
C

      IF (KOD .EQ. 555) THEN
C----    CONVERSION IS FROM LAT/LON TO UTM
         IZONE  = IZONIN
         IZON2  = IZONE

C----    Adjust for longitudes that cross the 180E/180W meridian.
C        Include tolerance of about 1m (0.00001 deg) to avoid 
C        reversing sign due to precision.
         if (slon .lt. -180.00001D0*3600.0D0) then
            slon = 360.0D0*3600.0D0 + slon
         else if (slon .gt. 180.00001D0*3600.0D0) then
            slon = -1.0D0*(360.0D0*3600.0D0 - slon)
         end if
         
C----    Reverse sign of input longitude;
C        UTMGEO uses positive for west longitude; calling 
C        routine uses standard convention of negative for west longitude
         SLON   = -1.0D0*SLON
         IDLON  = ABS(IDINT(SLON / 3600.0D0))
         
      ELSE IF (KOD .EQ. 333) THEN
C----    CONVERSION IS FROM UTM TO LAT/LON; ASSIGN ZONE
         IZONE  = IZONIN
         IZON2  = IZONE
      END IF

      A(5)=5.0D5
      A(6)=0.0D0

C---- Adjust A(6) value for South Latitude (SLAT<0 or IZONE<0)
      IF(KOD.EQ.555 .AND. SLAT.LT.0.0D0) A(6)=10.0D6
      IF(IZONE.LT.0) A(6)=10.0D6

      A(7) = 0.0D0
      A(8) = 0.9996D0

C---- Define axes for the datum
      SELECT CASE (ISPHER)

         CASE (0)
            A(15) = 6378206.4D0
            B(1)  = 6356583.8D0

         CASE (1)
            A(15) = 6378249.1450D0
            B(1)  = 6356514.86955D0

         CASE (2)
            A(15) = 6377397.155D0
            B(1)  = 6356078.96284D0

         CASE (3)
            A(15) = 6378150.0D0
            B(1)  = 6356768.337D0

         CASE (4)
            A(15) = 6378137.0D0
            B(1)  = 6356752.31414D0

      END SELECT

      A(16) = ((A(15)-B(1))/A(15))*((A(15)+B(1))/A(15))

C---- Compute coefficients for conversions
      CALL TMCOF(A)

C---- Test for type of conversion, geodetic to UTM or UTM to geodetic
      IF (KOD .EQ. 555) THEN
C----    Convert geodetic to UTM coordinates

C----    Test for zone input on geodetic to UTM indicating over ride of
C        normal 6 degree longitude band.

         IF(IZONE.EQ.0)GO TO 35
         IZONOUT = IZONE
         GO TO 22

C----    Compute central meridiam in seconds for enforced zone.
         IZONE=IZON2
  
  22     CONTINUE

         IF(ABS(IZONE) .LE. 30) THEN
            UTZ=30.0D0-DBLE(ABS(IZONE))
            A(9)=((UTZ*6.0D0)+3.0D0)*3600.0D0

         ELSE
            UTZ=DBLE(ABS(IZONE))-30.0D0
            A(9)=((UTZ*6.0D0)-3.0D0)*(-3600.0D0)
         END IF

         IF(IZON2.NE.0)GO TO 50
         GO TO 40

C----    Compute UTM zone (IZONE) and central meridian in seconds (A9)
C        for geodetic to UTM conversion where zone is not input.

  35     IZONE=30-(IDLON/6)
         IF(SLON.LT.0.0D0)IZONE=IDLON/6+31
         UTZ=30.0D0-DBLE(IZONE)
         A(9)=((UTZ*6.0D0)+3.0D0)*3600.0D0
         IF( SLAT.LT.0.0D0 )THEN
C----       Assign negative zone for south latitudes         
            IZONE = IZONE*(-1)
         END IF

  40     CONTINUE

  50     CALL TMFWD(SLAT,SLON,NORTH,EAST,A,IERR,SK,THET)
  
         IF( SLAT.LT.0.0D0 .AND. IZONE.GT.0 )THEN
C----       Assign negative zone for south latitudes         
            IZONOUT = IZONE*(-1)
         ELSE
            IZONOUT = IZONE
         END IF
         
C----    Reverse sign of longitude back to match input (negative for W)
         SLON = -1.0D0*SLON
                  
         GO TO 150

      ELSE IF( KOD .EQ. 333) THEN
C----    Convert UTM coordinates to geodetic

C----    Compute central meridian in seconds from IZONE input
         UTZ=30.0D0-DBLE(ABS(IZONE))
         A(9)=((UTZ*6.0D0)+3.0D0)*3600.0D0

         CALL TMINV(NORTH,EAST,SLAT,SLON,A,IERR,SK,THET)
         
C----    Adjust for longitudes that cross the 180E/180W meridian         
         if (slon .lt. -180.0D0*3600.0D0) then
            slon = 360.0D0*3600.0D0 + slon
         else if (slon .gt. 180.0D0*3600.0D0) then
            slon = -1.0D0*(360.0D0*3600.0D0 - slon)
         end if

C----    Reverse sign of longitude before returning (output negative for W)         
         SLON = -1.0D0*SLON
         
      END IF

  150 CONTINUE
  
      RETURN
      END SUBROUTINE


C***********************************************************************
C     SUBROUTINE TMCOF(A)
C
C     Purpose: To set up the coefficients to convert geodetic to
C              rectifying latitude and conversely
C
C     Called By:  UTMGEO
C
C     Calls To: <none>
C-----------------------------------------------------------------------
      SUBROUTINE TMCOF(A)

      IMPLICIT NONE

      DOUBLE PRECISION A(16),FAC
      A(10) = (((A(16)*(7.0D0/3.2D1)+(5.0D0/1.6D1))*A(16)+0.5D0)*A(16)+
     &         1.0D0)*A(16)*0.25D0

      A(1)= -(((A(10)*(1.95D2/6.4D1)+3.25D0)*A(10)+3.75D0)*A(10)+3.0D0)*
     &     A(10)

      A(2)=(((1.455D3/3.2D1)*A(10)+(7.0D1/3.0D0))*A(10)+7.5D0)*A(10)**2

      A(3)=-((7.0D1/3.0D0)+A(10)*(9.45D2/8.0D0))*A(10)**3

      A(4)=(3.15D2/4.0D0)*A(10)**4

      A(11)=(((7.75D0-(6.57D2/6.4D1)*A(10))*A(10)-5.25D0)*A(10)+3.0D0)*
     &      A(10)

      A(12)=(((5.045D3/3.2D1)*A(10)-(1.51D2/3.0D0))*A(10)+10.5D0)*
     &      A(10)**2

      A(13)=((1.51D2/3.0D0)-(3.291D3/8.0D0)*A(10))*A(10)**3

      A(14)=(1.097D3/4.0D0)*A(10)**4
C     A(1) to A(4) are for geodetic to rectifying latitude
C        conversion while A(11) to A(14) are coefficients for
C        rectifying to geodetic conversion.

      FAC=A(10)*A(10)

      A(10)=(((2.25D2/6.4D1)*FAC+2.25D0)*FAC+1.0D0)*(1.0D0-FAC)*
     &      (1.0D0-A(10))*A(15)

C     A(10) is now set to radius of sphere with great circle length
C        equal to spheroid meridian length.

      RETURN
      END


C***********************************************************************
C     SUBROUTINE TMINV(NORTH,EAST,SLAT,SLON,A,IERR,SK,THET)
C
C     PURPOSE: To computes latitude and longitude IN SECONDS (slat and
C              slon) from given rectangular coordinates x and y for
C              transverse mercator projection.
C
C     A = array of parameters used in computation, described by comments
C          for SUBR.TMFWD
C     IERR is set to 1 if grid distance from central meridian exceeds
C          0.2 of spheroid semimajor axis numerically or if absolute
C          value of rectifying latitude exceeds 1.47 radians.
C
C     ASSUMPTIONS
C          South latitudes and east longitude are negative.
C
C     MODIFICATIONS:
C          Moved calculation of B(10) to precede the IF test on values
C          out of range.   R.W. Brode, USEPA/OAQPS/AQMG, 01/12/09
C
C     Called By:  UTMGEO
C
C     Calls to:  <none>
C-----------------------------------------------------------------------
      SUBROUTINE TMINV(NORTH,EAST,SLAT,SLON,A,IERR,SK,THET)

      IMPLICIT NONE

      DOUBLE PRECISION SLAT,SLON,A(16),B(12),SINW,COSW,RN,T,TS,ETAS,
     & NORTH,EAST,X,Y,SK,BN,BNS,THET
      INTEGER IERR

      Y=NORTH
      X=EAST

      IERR=0
      B(9)=((A(5)-X)*1.0D-6)/A(8)
      B(10)=((Y-A(6))/A(8)+A(7))/A(10)

      IF ((DABS(B(9))-1.0D-7*A(15)*2.0D0 .GT. 0.0D0) .OR.
     &    (DABS(B(10))-1.47D0 .GT. 0.0D0)) THEN
         IERR=1
         SLAT=0.0D0
         SLON=0.0D0

      ELSE
         SINW=DSIN(B(10))
         COSW=DCOS(B(10))

         B(12)=COSW*COSW

         B(11)=(((A(14)*B(12)+A(13))*B(12)+A(12))*B(12)+A(11))
     &         *SINW*COSW+B(10)

         SINW=DSIN(B(11))
         COSW=DCOS(B(11))

         RN=DSQRT(1.0D0-A(16)*SINW*SINW)*1.0D6/A(15)

         T=SINW/COSW
         TS=T*T

         B(12)=COSW*COSW

         ETAS=A(16)*B(12)/(1.0D0-A(16))

         B(1)=RN/COSW

         B(2)=-T*(1.0D0+ETAS)*RN*RN/2.0D0

         B(3)=-(1.0D0+2.0D0*TS+ETAS)*B(1)*RN*RN/6.0D0

         B(4)=(((-6.0D0-ETAS*9.0D0)*ETAS+3.0D0)*TS+(6.0D0-ETAS*3.0D0)
     &         *ETAS+5.0D0)*T*RN**4/24.0D0

         B(5)=((TS*24.0D0+ETAS*8.0D0+28.0D0)*TS+ETAS*6.0D0+5.0D0)*
     &       B(1)*RN**4/120.0D0

         B(6)=(((ETAS*45.0D0-45.0D0)*TS+ETAS*162.0D0-90.0D0)*TS
     &        -ETAS*107.0D0-61.0D0)*T*RN**6/720.0D0

         B(7)=-(((TS*720.0D0+1320.0D0)*TS+662.0D0)*TS+61.0D0)*B(1)
     &         *RN**6/5040.0D0

         B(8)=(((TS*1575.0D0+4095.0D0)*TS+3633.0D0)*TS+1385.0D0)*T
     &         *RN**8/40320.0D0

         B(10)=B(9)*B(9)

C----    The values of SLAT and SLON are in seconds, not decimal degrees
         SLAT=((((B(8)*B(10)+B(6))*B(10)+B(4))*B(10)+B(2))*B(10)
     &         +B(11))*206264.8062470964D0
         SLON= (((B(7)*B(10)+B(5))*B(10)+B(3))*B(10)+B(1))*B(9)*
     &          206264.8062470964D0 + A(9)

crwb---  The following variables are not needed for AERMOD components:
crwb     Assign dummy values and return
         
         BN   = 0.0D0
         BNS  = 0.0D0 
         SK   = 0.0D0
         THET = 0.0D0
         
crwb         BN=B(9)*RN
crwb         BNS=BN**2

crwb         SK=1.0D0+((1.0D0+ETAS)/2.0D0)*BNS+(1.0D0+6.0D0*ETAS+9.0D0
crwb     &     *ETAS*ETAS+4.0D0*ETAS*ETAS*ETAS-24.0D0*ETAS*ETAS*TS-24.0D0
crwb     &     *ETAS*ETAS*ETAS*TS)*BNS*BNS/24.0D0+(BNS*BNS*BNS)/720.0D0

crwb         SK=SK*A(8)

C---- Compute convergence angle: THET

crwb        THET = (((((((-24.0D0*ETAS-27.0D0)*ETAS-7.0D0)*ETAS+1.0D0)*
crwb     &    ETAS)*TS)+(5.0D0*TS+3.0D0*TS**2)+((((11.0D0*ETAS+20.0D0)*
crwb     &    ETAS+9.0D0)*ETAS+2.0D0)*ETAS)+2.0D0)*(BN**5)*T/15.0D0)+
crwb     &    (T*BN) - (((45.0D0*TS+105.0D0)*TS+77.0D0)*TS+17.0D0)*
crwb     &    ((BN**7)*T/315.0D0)-(((1.0D0+TS-ETAS-2.0D0*ETAS**2)*BN**3)*
crwb     &    T/3.0D0)

crwb        THET=-THET*206264.8062470964D0

      END IF

      RETURN
      END

C***********************************************************************
C     SUBROUTINE TMFWD(SLAT,SLON,NORTH,EAST,A,IERR,SK,THET)

C     PURPOSE: To convert latitude and longitude in seconds (slat and
C              slon) to X and Y on transverse mercator projection.
C
C     A(1) to A(4) are coefficients used to convert geodetic latitude
C          to rectifying latitude,
C     A(5) is false easting,
C     A(6) is false northing,
C     A(8) is scale factor at central meridian
C     A(9) is central meridian in seconds
C     A(10) is radius of sphere having a great circle length equal to
C           spheroid meridian length
C     A(11) to A(14) are coefficients to convert rectifying latitude to
C           geodetic latitude
C     A(15) is semimajor axis of spheroid
C     A(16) is eccentricity squared.

C     IERR is set to 1 if lat exceeds 84 degrees, or
C          long exceeds 0.16 radians
C
C     MODIFICATIONS:
C          Moved calculation of B(10) to precede the IF test on values
C          out of range.   R.W. Brode, USEPA/OAQPS/AQMG, 01/12/09
C
C     Called By: UTMGEO
C
C     Calls To: <none>
C-----------------------------------------------------------------------
      SUBROUTINE TMFWD(SLAT,SLON,NORTH,EAST,A,IERR,SK,THET)

      IMPLICIT NONE

      DOUBLE PRECISION SLAT,SLON,A(16),B(12),SINP,COSP,RN,T,TS,ETAS,
     &                 NORTH,EAST,SK,THET
      INTEGER IERR

      IERR=0

      B(10)=(A(9)-SLON) *4.84813681109536D-6
      
      IF((DABS(SLAT)-302400.0D0 .GT. 0.0D0) .OR.
     &   (DABS(B(10))-0.16D0).GT. 0.0D0) THEN
         IERR=1
         EAST=0.0D0
         NORTH=0.0D0

      ELSE
        B(9)=SLAT*4.84813681109536D-6

        SINP=DSIN(B(9))
        COSP=DCOS(B(9))

        RN=A(15)/DSQRT(1.0D0-A(16)*SINP*SINP)

        T=SINP/COSP
        TS=T*T

        B(11)=COSP*COSP

        ETAS=A(16)*B(11)/(1.0D0-A(16))

        B(1)=RN*COSP

        B(3)=(1.0D0-TS+ETAS)*B(1)*B(11)/6.0D0

        B(5)=((TS-18.0D0)*TS+5.0D0+(14.0D0-58.0D0*TS)*ETAS)*B(1)*
     &       B(11)*B(11)/120.0D0

        B(7)=(((179.0D0-TS)*TS-479.0D0)*TS+61.0D0)*B(1)*B(11)**3
     &       /5040.0D0

        B(12)=B(10)*B(10)

        EAST=(((B(7)*B(12)+B(5))*B(12)+B(3))*B(12)+B(1))*B(10)*A(8)
     &       +A(5)

        B(2)=RN*B(11)*T/2.0D0

        B(4)=(ETAS*(9.0D0+4.0D0*ETAS)+5.0D0-TS)*B(2)*B(11)/12.0D0

        B(6)=((TS-58.0D0)*TS+61.0D0+(270.0D0-330.0D0*TS)*ETAS)*B(2)*
     &       B(11)*B(11)/360.0D0

        B(8)=(((543.0D0-TS)*TS-3111.0D0)*TS+1385.0D0)*B(2)*B(11)**3/
     &        20160.0D0

        NORTH=(((B(8)*B(12)+B(6))*B(12)+B(4))*B(12)+B(2))*B(12)+
     &      ((((A(4)*B(11)+A(3))*B(11)+A(2))*B(11)+A(1))
     &      *SINP*COSP+B(9))*A(10)

        NORTH=(NORTH-A(7))*A(8)+A(6)

crwb--- The following variables are not needed for AERMOD components:
crwb    Assign dummy values and return 

crwb        SK   = 0.0D0
crwb        THET = 0.0D0
        
C---- Compute scale factor: SK

crwb        SK=(((((-24.0D0*ETAS-48.0D0)*ETAS-28.0D0)*ETAS-4.0D0)*TS)+
crwb     &   (((4.0D0*ETAS+13.0D0)*ETAS+14.0D0)*ETAS+5.0D0))*((B(10)**4)/
crwb     &   24.0D0)*B(11)*B(11)+(1.0D0+ETAS)*B(11)*(B(10)*B(10))/
crwb     &   2.0D0+1.0D0

crwb        SK=SK*A(8)

C---- Compute convergence angle: THET

crwb        THET=(B(10)*SINP*(1.0D0+((B(10)**2)*B(11)/3.0D0)*(1.0D0+3.0D0
crwb     &     *ETAS+2.0D0*ETAS**2)+(B(10)**4)*((B(11)**2)/15.0D0)
crwb     &     *(2.0D0-TS)))*206264.8062470964D0

      END IF

      RETURN
      END

