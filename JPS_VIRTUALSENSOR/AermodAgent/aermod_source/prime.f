c----------------------------------------------------------------------
      subroutine prime1
c----------------------------------------------------------------------
c
c --- PRIME      Version:  1.0     Level:  970812                PRIME1
c                J. Scire, D. Strimaitis, EARTH TECH
c                Prepared for EPRI under contract WO3527-01
c
c --- PURPOSE:  Initialize the variables used by the PRIME
c               building downwash algorithm
c
c --- INPUTS:  none
c
c --- OUTPUT:
c
c     Common block /DFSN/ variables:
c           afac,xbyrmax,wiz0,wiy0,wfz,wfy,
c           dua_ua,xdecay,xdecayi,
c           rurliz,rurliy,urbniz,urbniy
c
c --- PRIME1 called by:  MAIN (Host)
c --- PRIME1 calls:      none
c----------------------------------------------------------------------

c --- Include common blocks
      USE MAIN1, ONLY: L_ORD_Turb
      USE PRIME_dfsn

      IMPLICIT NONE

c -----------------------
c --- /DFSN/ variables
c -----------------------

c --- Set the factor for defining when turb Approaches Asymptotic
c --- value, and also define the maximum allowed scaled distance
      afac   = 1.3D0
      xbyrmax= 15.0D0

c --- Turbulence intensities in wake (from Briggs rural curves)
      wiz0 = 0.06D0

C --- ORD_DWNW option - new limit on turbulence intensity
      IF (L_ORD_Turb) THEN
         wiz0 = 0.07D0                                             ! EMM
      END IF

      wiy0 = 0.08D0

c --- Wake Factors for sigw and sigv from Weil (1996)
      wfz = 1.7D0
      wfy = 1.7D0
c --- deltaU0/U0
      dua_ua = 0.7D0
c --- Power-law exponent for turbulence intensity change in distance
      xdecay  = 2.0D0/3.0D0
      xdecayi = 1.5D0

      return
      end

c----------------------------------------------------------------------
      subroutine numpr1
c----------------------------------------------------------------------
c
c --- PRIME      Version:  1.0     Level:  970812                NUMPR1
c                J. Scire, EARTH TECH
c                Prepared for EPRI under contract WO3527-01
c
c --- PURPOSE:  Initialize the variables used by the numerical
c               plume rise algorithm
c
c --- INPUTS:
c
c       Parameters:
c           MXENT, MXENTP1, MXNZ, MXNZP1, IO6
c
c --- OUTPUT:
c
c       Common block /NUMPARM/ variables:
c          GRAVI,RGAS,ZMIN,DS,NSTEP,SLAST,RP,ALPHAP(mxent),
c          BETAP(mxent),XCAT(mxentp1),NENT
c       Common block /AMBIENT/ variables:
c          ADIA,PTGRAD0,ZGPTA(mxnz),ZFACEA(mxnzp1)
c
c --- NUMPR1 called by:  MAIN (Host)
c --- NUMPR1 calls:      none
c----------------------------------------------------------------------

c --- Include common blocks
      USE PRIME_params
      USE PRIME_numparm
      USE PRIME_ambient
      USE MAIN1, ONLY : L_AWMA_ENTRAIN, AWMA_Beta_EntrainCoeff

      IMPLICIT NONE

      DOUBLE PRECISION :: DZ

      INTEGER :: NZAP1, NN, I

c -----------------------
c --- /NUMPARM/ variables
c -----------------------
c
c --- Set the acceleration due to gravity (m/s**2)
      gravi=9.80616D0

c --- Set the gas constant (m**2/s**2/deg. K)
      rgas=287.026D0

c --- Set the minimum plume centerline height (m)
      zmin=0.001D0

c --- Set the step size (m) in the numerical plume rise algorithm
      ds=1.0D0

c --- Set the internal save frequency of plume rise calculations (i.e.,
c     every DS*NSTEP meters) (NOTE: this the frequency with which the
c     results are saved internally -- not that passed back from the
c     NUMRISE routine)
      NSTEP=1

c --- Set the termination distance (m) of the plume rise calculation
      slast=5000.0D0

c --- Set the radiation coefficient (kg/m**2/deg. K**3/s)
      rp=9.1D-11

c --- Set the perturbed entrainment coefficients
c     ALPHAP (parallel direction), BETAP (normal direction)
      nent=0
      alphap(1)=0.11D0

C --- AWMA version D20350
C     Change entrainment constant from 0.6 to 0.35, which is set in modules.f,
C       if the ALPHA option AWMAEntrain downwash option was set
      if (.NOT. L_AWMA_ENTRAIN) THEN
         betap(1) = 0.6D0
      else
         betap(1) = AWMA_Beta_EntrainCoeff
c         betap(1) = 0.35d0
      end if

      xcat(1)=-9.0D9
      xcat(2)= 9.0D9

c -----------------------
c --- /AMBIENT/ variables
c -----------------------

c --- Set dry adiabatic lapse rate (deg. K/m)
      adia=0.00977D0

c --- Set minimum potential temperature lapse rate (deg. K/m)
      ptgrad0=0.0D0

c --- Set the default number of layers
      nza=45
      nzap1=nza+1
      if(nza.gt.mxnz)then
         write(io6,*)'ERROR in SUBR. NUMPR1 -- NZA is too large -- ',
     1   'NZA = ',nza,' MXNZ = ',mxnz
         stop
      endif
      if(nzap1.gt.mxnzp1)then
         write(io6,*)'ERROR in SUBR. NUMPR1 -- NZAP1 is too large -- ',
     1   'NZAP1 = ',nzap1,' MXNZP1 = ',mxnzp1
         stop
      endif

c --- Define the meteorological grid
c --- Set grid points every 10 m from 10-200 m
      dz=10.D0
      nn=1
      zgpta(nn)=dz
      do i=2,20
         nn=nn+1
         zgpta(nn)=zgpta(nn-1)+dz
      enddo
c --- Set grid points every 50 m from 250-500 m
      dz=50.D0
      do i=21,26
         nn=nn+1
         zgpta(nn)=zgpta(nn-1)+dz
      enddo
c --- Set grid points every 100 m from 600-2000 m
      dz=100.D0
      do i=27,41
         nn=nn+1
         zgpta(nn)=zgpta(nn-1)+dz
      enddo
c --- Set grid points every 500 m from 2500-4000 m
      dz=500.D0
      do i=42,45
         nn=nn+1
         zgpta(nn)=zgpta(nn-1)+dz
      enddo

c --- Compute the cell face heights from the grid point values
      zfacea(1)=0.0D0
      do i=2,nza
         zfacea(i)=0.5D0*(zgpta(i)+zgpta(i-1))
      enddo
      zfacea(nzap1)=zgpta(nza)+0.5D0*(zgpta(nza)-zgpta(nza-1))

      return
      end

c----------------------------------------------------------------------
cMACTEC      subroutine numrise(ldbhr,h,reff,texit,wexit,
cMACTEC     &                   ntr,xtr,ytr,ztr,rtr,linwake,numwake)
      subroutine numrise(ldbhr,h,reff,texit,wexit,ntr,capped,horiz,
     &                   capfact,xtr,ytr,ztr,rtr,linwake,numwake,ierr,
     &                   dbgunt)
c----------------------------------------------------------------------
c
c --- PRIME      Version:  1.0     Level:  980310               NUMRISE
c                X.(J.) Zhang, J. Scire, D. Strimaitis,  EARTH TECH
c
c                Adapted from CALPUFF routine NUMRISE
c                for EPRI under contract WO3527-01
c
c --- PURPOSE:  Compute plume rise using a numerical solution to the
c               Non-Boussinesq conservation laws.  Model allows:
c
c               (1) arbitrary ambient temperature stratifications
c               (2) arbitrary uni-directional wind stratification
c               (3) any size of finite emission source
c               (4) is free of the Boussinesq approximation
c               (5) radiative heat loss
c
c               Concurrently, compute diffusion (sigmas) in bldg wake
c               and determine plume/cavity interaction
c
c --- MODIFIED: To include error code (ierr) to track runtime error
c               associated with NN > MXNW.
c               R.W. Brode, U.S. EPA/OAQPS/AQMG, 10/19/2009
c
c --- MODIFIED: To incorporate BETA-test draft options for capped stack
c               (POINTCAP) and horizontal releases (POINTHOR). The POINTHOR
c               option currently assumes that horizontal release is
c               aligned with the wind direction for each hour.
c               R.W. Brode, MACTEC (f/k/a PES), Inc., 09/30/05
c               R.W. Brode, U.S. EPA/OAQPS/AQMG, 12/07/06
c
c --- MODIFIED: To resolve potential problem with some compilers due to the
c               variable xbi not being defined if LINWAKE is .FALSE.
c               The resolution was to split an IF-THEN statement.
c               R.W. Brode, PES, Inc. - 04/08/02
c
c --- MODIFIED: For use with the AERMOD model.  Modified the critical value
c               of PHI from 45 degrees to 20 degrees.  Replaced calls to
c               SIGY and SIGZ with calls to SIGYPR and SIGZPR.  Added call
c               to new WAKE_DFSN2 routine after plume height array is
c               complete to obtain final sigma tables and virtual source
c               terms.  Also returns linwake and numwake to calling routine.
c               R.W. Brode, PES, Inc. - 07/05/01
c
c --- INPUTS:
c         LDBHR - logical       - Flag for debug write statements
c             H - real          - Release height (m)
c          REFF - real          - Effective radius of release (m)
c            TP - real          - Exit temperature (deg. K)
c         WEXIT - real          - Exit velocity (m/s)
c           NTR - integer       - Number of points in trajectory passed
c                                 back to calling program (final point
c                                 is "final rise")
c        CAPPED - logical       - Flag for capped stacks
c        HORIZ  - logical       - Flag for horizontal releases
c
c     Common block /AMBIENT/ variables:
c           NZA,UAMB(mxnz),RAMB(mxnz),DEDZ(mxnzp1),TAMB(mxnz),
c           ZFACEA(mxnzp1),TAMB0,RAMB0
c     Common block /NUMPARM/ variables:
c           ZMIN, DS, NSTEP, SLAST, GRAVI
c     Common block /WAKEDAT/ variables:
c           HB, WB, XLB, RB, HR, XLR, XLC, XBADJ, YBADJ
c     Parameters:
c           MXNZ, MXNZP1, MXENT, MXENTP1, IO6
c
c --- OUTPUT:
c        XTR(ntr) - real          - Downwind distance from source (m)
c        YTR(ntr) - real          - Crosswind distance from source (m)
c        ZTR(ntr) - real          - Plume centerline height (m)
c        RTR(ntr) - real          - Plume radius (m)
c        IERR     - integer       - Error code, = 1 if NN > MXNW
c
c     Common block /WAKEDAT/ variables:
c           FQCAV
c
c --- NUMRISE called by:  PHEFF (HOST subroutine)
c --- NUMRISE calls:      ZMET,LUMP,RATE,MARCHING,UNLUMP
c                         ZSTREAM, POSITION, WAKE_U
c                         WAKE_DRDX, WAKE_DFSN, SIGZPR, SIGYPR,
c                         WAKE_FQC, WAKE_DFSN2
c----------------------------------------------------------------------
c --- Notation --- in (KG,M,S) units
c               NN:     Number of points along rise trajectory
c               X:      PLUME LOCATION (downwind from source)
c               Y:      PLUME LOCATION (crosswind from source)
c               Z:      PLUME HEIGHT
c               H:      Release height (flame ht., stack ht.)
c               ZE:     PLUME EQUILIBRIUM HEIGHT
c               S:      LENGTH ALONG PLUME CENTERLINE
c               R:      PLUME RADIUS
c               U:      PLUME HORIZONTAL VELOCITY
c               W:      PLUME VERTICAL VELOCITY
c               USC:    VELOCITY ALONG PLUME CENTERLINE
c               PHI:    ANGLE BETWEEN PLUME TRAJECTORY AND GROUND
c               TP:     PLUME TEMPERATURE
c               ua:     HORIZONTAL WIND SPEED
c               dudz:   WIND SHEAR
c               ta:     AMBIENT TEMPERATURE
c               dpdz:   AMBIENT POTENTIAL TEMPERATURE GRADIENT
c               ramb:   AMBIENT DENSITY
c               ra:     PLUME DENSITY
c               zmin:   Minimum plume centerline height (m)
c               ds:     Step size (m) in the numerical plume rise calc.
c               nstep:  Reporting frequency of numerical calc.
c               slast:  Termination distance (m) of plume rise calc.
c               gravi:  Acceleration due to gravity (m/s**2)
c----------------------------------------------------------------------
c --- Include files
      USE PRIME_params
      USE PRIME_ambient
      USE PRIME_numparm
      USE PRIME_wakedat
      USE PRIME_PLU
      USE PRM2_WAKEDAT, ONLY: DFSN2CALL, XTR_SAV, ZTR_SAV
      USE MAIN1, ONLY:  L_AWMA_UTURB, L_AWMA_UTURBHX

      IMPLICIT NONE

      INTEGER NTR
      DOUBLE PRECISION XTR(ntr),YTR(ntr),ZTR(ntr),RTR(ntr)

      DOUBLE PRECISION XT(mxnw),YT(mxnw),ZT(mxnw),RT(mxnw)
      DOUBLE PRECISION RHS(7),RHSTEMP(7),F(7),FTEMP(7)
      DOUBLE PRECISION H, MAX, REFF, WEXIT, TEXIT, DRDXA, DS0, ZCUMUL,
     &                 R15SRC, UA0, RA, TA, DUDZ0, DPDZ, XB,YB, ZB,
     &                 DUFAC, UA, DUDZ, DELTAT, FDUM, FB, FM,
     &                 UAM, WBYU, XMAXM, XMAX, XMAXB, ZNF, ZNF0, DMIN,
     &                 XNP, DXDS, DZDX, DZSTRM, DZDS, DYDS, UFAC, ZC,
     &                 DELTAZ, XBI, BASE, RISE, BIDSQ, SZI, SYI, ZFIN,
     &                 YFIN, XFIN, RFIN, XFIN0, X15R, DSFIN, DX15R, XBB,
     &                 XBE, DZDXB, DZDXE, DELN, RN, DELZFIN


      INTEGER I, NUMWAKE, ierr, IPOSITN, NNP, NP, NN, N15R, NBEG, NEND,
     &        IN, IPOS, JN, IT, DBGUNT

      logical ldb,ldbnn,ldbu,ldbhr,linwake
c MACTEC Begin change
c --- Specify CAPFACT parameter, factor by which initial effective
c     radius of plume is increased by raincap.
      DOUBLE PRECISION CAPFACT
      LOGICAL CAPPED, HORIZ
c MACTEC End change

c --- Use LDB as a local switch for more extensive debug output
      ldb=ldbhr
c !!!      ldb=.FALSE.
c !!!      ldbu=ldb
      ldbu=.FALSE.

      ierr = 0
      NN = 0

      linwake=.FALSE.
      X=0.0D0
      Y=0.0D0
      Z=MAX(H,ZMIN)
      S=0.0D0
      R=REFF
      U=0.000002D0
      w=wexit
      tp=texit
      drdxa=0.0D0
      ipositn=4

c --- Store stepping length
      ds0=ds

c --- Introduce ZCUMUL to track change in the vertical coordinate of the
c --- trajectory that arises from streamline inclination from horizontal
c --- This adjustment is applied outside of the system of rise equations
c --- out to a distance of 15R from downwind face of building
      zcumul=0.0D0
      r15src=xbadj+(xLb+15.D0*Rb)

c --- Get met. variables at release height
      call ZMET(z,ua0,ra,ta,dudz0,dpdz)

c --- Apply reduction in wake wind speed
      xb=x-xbadj
      yb=y-ybadj
      zb=MAX(z,zmin)
      call POSITION(xb,yb,zb,ipositn)
      ufac=1.0D0
      dufac=0.0D0

      if(ipositn.LT.4) then
         call WAKE_U(.FALSE.,xb,yb,zb,ufac,dufac,DBGUNT)
      end if
      ua=ufac*ua0
      dudz=ufac*dudz0+dufac*ua0

c --- Use Briggs plume rise estimates to set distance scale
c --- Compute initial buoyancy flux (m**4/s**3)
      deltat=MAX(tp-ta,0.0D0)
      fdum=w*r*r/tp
      fb=gravi*fdum*deltat
c --- Compute momentum flux (m**4/s**2)
      fm=w*fdum*ta

c MACTEC Begin change
      if (capped) then
c ---    Adjust vertical and horizontal velocities of plume to account
c ---    for influence of cap; recalc FM
         r = CAPFACT * reff
         u = wexit / (CAPFACT**2)
         w = 0.001D0
         fm=w*w*r*r*ta/tp
      else if (horiz) then
c ---    Adjust vertical and horizontal velocities of plume to account
c ---    for horizontal release; recalc FM
         u = wexit
         w = 0.001D0
         fm=w*w*r*r*ta/tp
      end if
c MACTEC End change

c --- Final neutral rise distance
      uam=MAX(ua,1.0D0)
c --- Momentum only: (do not base xmax on case where w<uam)
      wbyu=MAX(1.0D0,w/uam)
      xmaxm=8.0D0*r*wbyu*(1.0D0+3.0D0/wbyu)**2
      if(fb.le.0.0D0)then
c ---    No buoyancy, momentum only
         xmax=xmaxm
      elseif(fb.lt.55.0D0)then
c ---    Buoyancy flux < 55 m**4/s**3
         xmaxb=49.0D0*fb**0.625D0
         xmax=MAX(xmaxm,xmaxb)
      else
c ---    Buoyancy flux .GE. 55 m**4/s**3
         xmax=119.D0*fb**0.4D0
      endif

c --- Use Briggs neutral rise to identify "minimal rise" cases
c --- Compute Briggs neutral final rise
      if(fb.LE.0.0D0) then
c ---    No buoyancy, momentum only
         znf=6.0D0*r*w/uam
      elseif(fb.LT.55.0D0) then
c ---    Buoyancy flux < 55 m**4/s**3
         znf=21.425D0*(fb**0.75D0)/uam
      else
c ---    Buoyancy flux .GE. 55 m**4/s**3
         znf=38.71D0*(fb**0.60D0)/uam
      endif
c --- Set minimum rise to 0.1 m
      znf0=MAX(0.1D0,znf)

c --- Guard against step length greater than likely rise
      dmin=0.5D0*znf0
      if(ds.GT.dmin) then
         ds=dmin
         if(ldb) then
            write(DBGUNT,*)'NUMRISE - initial step reset'
            write(DBGUNT,'(1x,a,3(f12.4,2x))')'znf,ds0,ds  :',znf,ds0,ds
         endif
      endif

c --- INDIRECT VARIABLES
      USC=DSQRT(U*U+W*W)
      PHI=DATAN(W/U)
c --- PARAMETERS
      NP=NSTEP
      XNP=DBLE(NP)
      nnp=1

c --- START MARCHING LOOP
      DEN=RA*TA/texit
      call LUMP(ua,ta,f)

999   continue

c --- Set local debug logical
      if(nnp.LT.150) then
         ldbnn=ldb
      else
         ldbnn=.FALSE.
      endif

c --- Define coordinates of plume relative to bldg.-defined origin
      xb=x-xbadj
      yb=y-ybadj
      zb=MAX(z+zcumul,zmin)
c --- Obtain mean streamline slopes here (within 15R of building)
      dxds=0.0D0
      dzds=0.0D0
      dzdx=0.0D0
      dzstrm=0.0D0
      call POSITION(xb,yb,zb,ipositn)
      if(ipositn.GT.2 .AND. x.LE.r15src) then
         call ZSTREAM(hb,wb,xLb,rb,xLr,hr,xb,yb,zb,dzdx)
         dxds=U/USC
         dzds=dzdx*dxds
         dzstrm=dzds*ds
      endif
c --- Define the crosswind velocity component = zero
      dyds=0.0D0
      v=0.0D0

c --- Compute RHS of rate equations for this location
      call RATE(ua,dudz,ra,dpdz,ta,drdxa,rhs)

c --- PREDICTOR MARCHING
      call MARCHING(f,ftemp,rhs,ds)
      call UNLUMP(ua,ta,ra,ftemp)

c --- Extract met and apply reduction in wake wind speed
      zb=MAX(z+zcumul+dzstrm,zmin)
      call ZMET(zb,ua0,ra,ta,dudz0,dpdz)
      call POSITION(xb,yb,zb,ipositn)
      ufac=1.0D0
      dufac=0.0D0

      if(ipositn.LT.4) then
         call WAKE_U(ldbu,xb,yb,zb,ufac,dufac,DBGUNT)
      end if
      ua=ufac*ua0
      dudz=ufac*dudz0+dufac*ua0
      call RATE(ua,dudz,ra,dpdz,ta,drdxa,rhstemp)

c --- CORRECTOR
      DO I=1,7
         RHS(I)=0.5D0*(RHSTEMP(I)-RHS(I))
      ENDDO
      call MARCHING(ftemp,f,rhs,ds)
      call UNLUMP(ua,ta,ra,f)

c --- Compute incremental change in plume height to account for
c --- streamline ascent/descent, and add to cumulative change
      zcumul=zcumul+dzstrm
c --- Apply cumulative adjustment to plume height
      zc=MAX(z+zcumul,zmin)
c --- Define coordinates of plume relative to bldg.-defined origin
      xb=x-xbadj
      yb=y-ybadj
      zb=zc
      call POSITION(xb,yb,zb,ipositn)

c --- Numerical procedure may result in small negative downwind
c --- distance:  reset to zero and go to next step
      if(x.LT.0.0D0) then
         x=0.0D0
         s=s+ds
         nnp=nnp-1
         goto 96
      endif

c --- Write debug output if in debug mode
      if(ldbnn)then
         if(mod(nnp,1000).eq.1) write(DBGUNT,112)
112      format(/3x,'NNP',7x,'X',6x,'Y',6x,'Z',6x,'R',6x,'U',5x,'V',
     1   6x,'W',5x,'USC',5x,'PHI',4x,'DEN',3x,'TP',5x,'UA',5x,'RA',4x,
     2   'TA',6x,'DUDZ',4x,'DPDZ',3x,'DZDS',3x,'DYDS',1x,'IPOS',
     3   1x,'DELTAZ')
         deltaz=zc-h
         write(DBGUNT,114)nnp,x,y,zc,r,u,v,w,usc,phi,den,tp,ua,ra,ta,
     &    dudz,dpdz,dzds,dyds,ipositn,deltaz
114      format(1x,i5,f9.2,3f7.2,4f7.2,
     1    f8.4,f6.3,f7.2,f6.2,f7.3,f7.2,f8.3,
     2    f8.3,2f7.3,i5,f7.3)
      endif

c --- When trajectory inclination falls below 20 degrees, ignoring
c --- streamline descent, check for wake influence
      if(phi.LE.0.349065850398866D0 .AND. ipositn.LT.4) then
         if(.not.LINWAKE) then
c ---       Plume centerline has just entered wake
            linwake=.TRUE.
            xbi=xb
c ---       Use unadjusted rise for BID
            base=MAX(H,ZMIN)
            rise=MAX(0.0D0,z-base)
            bidsq=(rise/3.5D0)**2
c ---       Guard against x.LE.0 due to precision
            if(x.LE.0.0D0) then
               szi=DSQRT(bidsq)
               syi=szi
            else
               call SIGZPR(x,z,szi)
               szi=DSQRT(szi**2+bidsq)
               call SIGYPR(x,z,syi)
               syi=DSQRT(syi**2+bidsq)
            endif

c ---       Normal debug output
            if(ldbhr) then
               write(DBGUNT,*) ' '
               write(DBGUNT,*)'NUMRISE call to WAKE_DFSN -- A'
               write(DBGUNT,'(1x,a,4(f12.5,2x))')'x,y,z,z+zcum: ',
     &                                            x,y,z,zc
               write(DBGUNT,'(1x,a,3(f12.5,2x))')'ds,u,w      : ',
     &                                            ds,u,w
               write(DBGUNT,'(1x,a,2(f12.5,2x))')'xb,phi      : ',
     &                                            xb,phi
               write(DBGUNT,'(1x,a,2(f12.5,2x))')'szi,syi     : ',
     &                                            szi,syi
            endif

c ---       Compute table of sigmas and growth rate in wake region
            call WAKE_DFSN(ldb,xbi,szi,syi,z,DBGUNT)
            numwake = nwak
         endif
c ---    Select plume radius growth rate for this location
         call WAKE_DRDX(x,drdxa)
      endif

c --- Process new position
      S=S+DS
      if(DBLE(NNP/NP).eq.DBLE(NNP)/XNP) THEN
         NN=NNP/NP
         if(nn.gt.mxnw)then
            write(DBGUNT,*)' '
            write(DBGUNT,*)'Error in Subr. NUMRISE -- NN too large:'
            write(DBGUNT,*)'                          NN   = ',nn
            write(DBGUNT,*)'                          MXNW = ',mxnw
            write(DBGUNT,*)' '
            write(DBGUNT,*)'Source parameters may be suspect.'
            write(DBGUNT,*)'Do not use Clearinghouse procedure for '
            write(DBGUNT,*)'capped/horizontal stacks with PRIME!'
            write(DBGUNT,*)' '
            ierr = 1
            return
         endif
         XT(NN)=X
         YT(nn)=Y
         ZT(NN)=zc
         RT(NN)=R
c --- CHECK FOR PLUME EQUILIBRIUM HEIGHT
         IF(x .ge. xmax) THEN
            ZFIN=zc
            YFIN=Y
            XFIN=X
            RFIN=R
            GOTO 97
         ENDIF
      ENDIF

c --- Extract met and apply reduction in wake wind speed
96    call ZMET(zb,ua0,ra,ta,dudz0,dpdz)
      ufac=1.0D0
      dufac=0.0D0

      if(ipositn.LT.4) then
         call WAKE_U(.FALSE.,xb,yb,zb,ufac,dufac,DBGUNT)
      end if
      ua=ufac*ua0
      dudz=ufac*dudz0+dufac*ua0

c --- Next increment
      NNP=NNP+1
c --- Stop rise at local maximum (excludes streamline descent effects)
      if(w.lt.0.0D0)then
         zfin=zc
         yfin=y
         xfin=x
         rfin=r
         go to 97
      endif

c --- Adjust ds toward ds0 for next step
      if(ds.LT.ds0) ds=MIN(ds0,2.0D0*ds)

      IF(S.LT.SLAST) GOTO 999
      ZFIN=zc
      YFIN=Y
      XFIN=X
      RFIN=R

97    CONTINUE

c --- Complete trajectory out to "15R" if required, to account for
c --- streamline slope (vertical only)
      xfin0=xfin
      x15r=r15src-xfin0
      if(x15r.GT.0.0D0) then
c ---    Set stepsize
         dsfin=DBLE(nstep)*ds
         dx15r=x15r/DBLE(mxnw-nn)
         dx15r=MAX(dsfin,dx15r)
c ---    Set range for additional steps
         n15r=MIN(IDINT(x15r/dx15r),mxnw-nn)
         nbeg=nn+1
         nend=nn+n15r
         do in=nbeg,nend
c ---       Define coordinates of plume relative to bldg.-defined origin
            xbb=xt(in-1)-xbadj
            xbe=xbb+dx15r
            yb=yt(in-1)-ybadj
            zb=zt(in-1)
c ---       Obtain mean streamline slope
            dzdx=0.0D0
            call POSITION(xbb,yb,zb,ipos)
            if(ipos.GT.2) then
               call ZSTREAM(hb,wb,xLb,rb,xLr,hr,xbb,yb,zb,dzdxb)
               call ZSTREAM(hb,wb,xLb,rb,xLr,hr,xbe,yb,zb,dzdxe)
               dzdx=0.5D0*(dzdxb+dzdxe)
            endif
            xt(in)=xt(in-1)+dx15r
            yt(in)=yfin
            zt(in)=MAX(zmin,zt(in-1)+dzdx*dx15r)
            rt(in)=rfin
            zcumul=zcumul+dzdx*dx15r

c ---       Check for wake entry if this has not already happened
            if(.not.LINWAKE) then
               if(ipos.LT.4) then
c ---             Plume centerline has just entered wake
                  linwake=.TRUE.
c ---             Set "internal" variable names
                  x=xt(in)
                  z=zt(in)-zcumul
                  xbi=x-xbadj
c ---             Use unadjusted rise for BID
                  base=MAX(H,ZMIN)
                  rise=MAX(0.0D0,z-base)
                  bidsq=(rise/3.5D0)**2
                  call SIGZPR(x,z,szi)
                  szi=DSQRT(szi**2+bidsq)
                  call SIGYPR(x,z,syi)
                  syi=DSQRT(syi**2+bidsq)

c ---             Normal debug output
                  if(ldbhr) then
                     write(DBGUNT,*) ' '
                     write(DBGUNT,*)'NUMRISE call to WAKE_DFSN -- B'
                     write(DBGUNT,'(1x,a,4(f12.5,2x))')'x,y,z,z+zcum:',
     &                                                  x,yfin,z,zt(in)
                     write(DBGUNT,'(1x,a,2(f12.5,2x))')'xb,phi      :',
     &                                                  xbi,phi
                     write(DBGUNT,'(1x,a,2(f12.5,2x))')'szi,syi     :',
     &                                                  szi,syi
                  endif

c ---             Compute table of sigmas and growth rate in wake region
                  call WAKE_DFSN(ldb,xbi,szi,syi,z,DBGUNT)
                  numwake = nwak
               endif
c ---          Select plume radius growth rate for this location
               call WAKE_DRDX(x,drdxa)
            endif

         enddo
c ---    Update nn and reset "fin" data
         nn=nend
         xfin=xt(nn)
         zfin=zt(nn)
      endif

c --- Construct trajectory arrays for calling program
      if(nn.GT.ntr) then
c ---    Sample a subset of the nn points
         xtr(ntr)=xfin
         ytr(ntr)=yfin
         ztr(ntr)=zfin
         rtr(ntr)=rfin
         if(nn.LE.2*ntr) then
c ---       Fill elements with nearest values
            deln=DBLE(nn)/DBLE(ntr)
            do in=1,ntr-1
               jn=IDINT(DBLE(in)*deln)
               xtr(in)=xt(jn)
               ytr(in)=yt(jn)
               ztr(in)=zt(jn)
               rtr(in)=rt(jn)
            enddo
         else
c ---       Use sliding step-size to sample nearfield more frequently
            deln=2.0D0*DBLE(nn-ntr)/DBLE(ntr*(ntr-1))
            rn=0.0D0
            do in=1,ntr-1
               rn=rn+1.0D0+DBLE(in-1)*deln
               jn=IDINT(rn)
               xtr(in)=xt(jn)
               ytr(in)=yt(jn)
               ztr(in)=zt(jn)
               rtr(in)=rt(jn)
            enddo
         endif
      else
c ---    Fill elements directly
         do in=1,nn
            xtr(in)=xt(in)
            ytr(in)=yt(in)
            ztr(in)=zt(in)
            rtr(in)=rt(in)
         enddo
c ---    Fill excess elements with final rise properties
         do it=nn+1,ntr
            xtr(it)=xfin
            ytr(it)=yfin
            ztr(it)=zfin
            rtr(it)=rfin
         enddo
      endif

c --- Restore step size (may have changed)
      ds=ds0

CRWB  NOTE!  xbi is not defined if LINWAKE = .FALSE.  The following should solve this
      if(LINWAKE) then
c ---    Determine maximum fraction of plume captured in cavity
         if (xbi.LT.(xLb+xLR)) then
c ---       Plume centerline enters wake boundary before clearing cavity
            call WAKE_FQC(ldb,xbi,xtr,ztr,mxntr,DBGUNT)
         else
            fqcav = 0.0D0
         endif
      else
         fqcav=0.0D0
      endif

c --- Normal debug output
      if(ldbhr) then
         delzfin=zfin-h
         write(DBGUNT,*)
         write(DBGUNT,*)'      Initial Plume Temperature = ',texit
         write(DBGUNT,*)'             Buoyancy flux (FB) = ',fb
         write(DBGUNT,*)'             Momentum flux (FM) = ',fm
         write(DBGUNT,*)'  Neutral dist. to final rise   = ',xmax
         write(DBGUNT,*)'  Calc distance to final rise   = ',xfin0
         write(DBGUNT,*)'Distance from final rise to 15R = ',x15r
         write(DBGUNT,*)'Total distance tabulated (XFIN) = ',xfin
         write(DBGUNT,*)'    Final Y displacement (YFIN) = ',yfin
         write(DBGUNT,*)'      Final plume height (ZFIN) = ',zfin
         write(DBGUNT,*)'     Final plume rise (DELZFIN) = ',delzfin
         write(DBGUNT,*)'      Final plume radius (RFIN) = ',rfin
         write(DBGUNT,*)'Cumul. streamline adj. (ZCUMUL) = ',zcumul
         write(DBGUNT,*)
         write(DBGUNT,*)'    Fraction of plume in CAVITY = ',fqcav
         write(DBGUNT,*)
      endif

C --- AWMA/Petersen:
C       Save the trajectory arrays from the first call to WAKE_DFSN for
C       use in WAKE_DFSN2
      if(ldb) then
c ---    Write the arrays passed back to the calling routine
         write(DBGUNT,26)
26       format('  -- Trajectory arrays before CALL WAKE_DFSN2')
         write(DBGUNT,28)
         do i=1,ntr
            write(DBGUNT,32)i,xtr(i),ytr(i),ztr(i),rtr(i),rtr(i)*0.8D0
         enddo
         write(DBGUNT,*)
      endif

C --- AWMA version D20350
      IF (L_AWMA_UTURB .OR. L_AWMA_UTurbHX) THEN
        XTR_SAV = XTR
        ZTR_SAV = ZTR
      END IF

c
c --- Compute new table of sigmas and growth rate in wake region with
c --- full array of plume heights for use in calls to ambient sigmas.
c --- Also redefines virtual source sigmas.
      if (linwake) then
         DFSN2CALL = .true.
         call WAKE_DFSN2(ldb,xbi,szi,syi,xtr,ztr,ntr,DBGUNT)
      end if

c --- Extended debug output
      if(ldb) then
c ---    Write the arrays passed back to the calling routine
         write(DBGUNT,27)
27       format('  -- Trajectory arrays after CALL WAKE_DFSN2')
         write(DBGUNT,28)
28       format(/4x,'I',10x,'XTR',8x,'YTR',8x,'ZTR',8x,'RTR',8x,'sz?'/)
         do i=1,ntr
            write(DBGUNT,32)i,xtr(i),ytr(i),ztr(i),rtr(i),rtr(i)*0.8D0
32          format(i5,3x,5(f10.4,1x))
         enddo
         write(DBGUNT,*)
      endif

      RETURN
      END

c----------------------------------------------------------------------
      subroutine rate(ua,dudz,ra,dpdz,ta,drdxa,rhs)
c----------------------------------------------------------------------
c
c --- PRIME      Version:  1.0     Level:  970812                  RATE
c                X. Zhang, J. Scire,   EARTH TECH
c
c                Adapted from CALPUFF routine RATE
c                for EPRI under contract WO3527-01
c
c --- PURPOSE:  Compute the right-hand side of the equations
c
c --- INPUTS:
c         UA - real    - Current ambient wind speed (m/s)
c       DUDZ - real    - Current wind shear (1/s)
c         RA - real    - Current ambient air density (kg/m**3)
c       DPDZ - real    - Current ambient potential temperature gradient
c                        (deg. K/m)
c         TA - real    - Current ambient air temperature (deg. K)
c     ALPHA0 - real    - Plume entrainment coefficient (parallel)
c      DRDXA - real    - Growth rate of plume radius due to ambient turb
c
c     Common block /PLU/ variables:
c           X,R,U,V,W,USC,PHI,DEN,TP
c     Common block /NUMPARM/ variables:
c           GRAVI, RP,
c           NENT, ALPHAP(mxent), BETAP(mxent), XCAT(mxentp1),
c     Parameters:
c           MXENT, MXENTP1
c
c --- OUTPUT:
c        RHS(7) - real     - Right hand terms
c
c --- RATE called by:  NUMRISE
c --- RATE calls:      none
c----------------------------------------------------------------------
c --- Include files
      USE PRIME_numparm
      USE PRIME_PLU
      USE MAIN1, ONLY : L_AWMA_ENTRAIN, AWMA_Beta_EntrainCoeff

      IMPLICIT NONE

      DOUBLE PRECISION RHS(7)
      DOUBLE PRECISION ALPHA, ALPHA0, BETA, BETA0, DRDXA, RA, UA, TA,
     &                 RHS1A, DUDZ, DPDZ
      INTEGER I, NENTP1
c ---   Constants:
c          GRAVI - Gravitational acceleration (m/s**2)
c          RP    - Radiation coefficient (kg/m**2/deg. K**3/s)
c --- Set default entrainment coefficients
      data alpha0/0.11D0/, beta0/0.6D0/
c --- Define the entrainment coefficients
      alpha=alpha0

C --- AWMA version D20350
C     Change entrainment constant from 0.6 to 0.35, which is set in modules.f,
C       if the ALPHA option AWMAEntrain downwash option was set
      if (.NOT. L_AWMA_ENTRAIN) THEN
      beta=beta0
      else
         beta = AWMA_Beta_EntrainCoeff
c         beta = 0.35d0
      end if

      if(nent.gt.0)then
c ---    Check if the plume is in the area where perturbed entrainment
c ---    coefficients apply
         if(x.lt.xcat(1))go to 99
         nentp1=nent+1
         do i=2,nentp1
            if(x.le.xcat(i))then
               alpha=alphap(i-1)
               beta=betap(i-1)
               go to 99
            endif
         enddo
c ---    Override any ambient growth rate
         drdxa=0.0D0
      endif
99    continue

      RHS(1)=(2.0D0*R*RA)*(ALPHA*DABS(USC-UA*U/USC)+
     1                     BETA*DABS(UA*DSIN(PHI)))
c
c --- Condition entrainment to be .GE. growth due to ambient turb.
      rhs1a =(2.0D0*R*RA)*UA*drdxa
      rhs(1)=MAX(rhs(1),rhs1a)
c
      RHS(2)=-(R*R*DEN*W)*DUDZ
      RHS(3)=GRAVI*R*R*(RA-DEN)
      RHS(4)=-(R*R*DEN*W)*DPDZ-RP*R*(TP**4-TA**4)
      RHS(5)=W/USC
      RHS(6)=U/USC
      rhs(7)=v/usc
      RETURN
      END

c----------------------------------------------------------------------
      subroutine lump(ua,ta,f)
c----------------------------------------------------------------------
c
c --- PRIME      Version:  1.0     Level:  970812                  LUMP
c                X.(J.) Zhang, J. Scire,  EARTH TECH
c
c --- PURPOSE:  Calculate the lumped variables
c
c --- INPUTS:
c         UA - real    - Current ambient wind speed (m/s)
c         TA - real    - Current ambient air temperature (K)
c
c --- OUTPUT:
c          F(7) - real     - lumped variables
c
c --- LUMP called by:  NUMRISE
c --- LUMP calls:      none
c----------------------------------------------------------------------
      USE PRIME_PLU

      IMPLICIT NONE

      DOUBLE PRECISION F(7), UA, TA
      F(1)=DEN*USC*R*R
      F(2)=F(1)*(U-UA)
      F(3)=F(1)*W
      F(4)=F(1)*(TP-TA)
      F(5)=Z
      F(6)=X
      F(7)=y
      RETURN

      END

c----------------------------------------------------------------------
      subroutine marching(fold,fnew,rhs,ds)
c----------------------------------------------------------------------
c
c --- PRIME      Version:  1.0     Level:  970812              MARCHING
c                X.(J.) Zhang, J. Scire,  EARTH TECH
c
c --- PURPOSE:  Marching S one step, either PREDICTOR or CORRECTOR
c
c --- INPUTS:
c       FOLD(7) - real     - Old values
c        RHS(7) - real     - Right hand terms
c            DS - real     - Distance (m) along plume axis
c
c
c --- OUTPUT:
c       FNEW(7) - real     - New values
c
c --- MARCHING called by:  NUMRISE
c --- MARCHING calls:      none
c----------------------------------------------------------------------
      IMPLICIT NONE

      DOUBLE PRECISION FNEW(7),FOLD(7),RHS(7)
      DOUBLE PRECISION DS
      INTEGER I

      DO I=1,7
         FNEW(I)=FOLD(I)+RHS(I)*DS
      END DO

      RETURN
      END

c----------------------------------------------------------------------
      subroutine unlump(ua,ta,ra,f)
c----------------------------------------------------------------------
c
c --- PRIME      Version:  1.0     Level:  970812                UNLUMP
c                X.(J.) Zhang, J. Scire,  EARTH TECH
c
c --- PURPOSE:  Calculate physical variables from lumped variables
c
c --- MODIFIED: To limit plume temperature (TP) to be .GE. ambient
c               temperature (TA) minus 10K to avoid potential math error.
c               R.W. Brode, PES/MACTEC, Inc. - 06/17/03
c
c --- INPUTS:
c         UA - real    - Current ambient wind speed (m/s)
c         TA - real    - Current ambient air temperature (K)
c         RA - real    - Current ambient air density (kg/m^3)
c       F(7) - real    - Lumped variables
c
c --- OUTPUT:
c       common /PLU/:
c          U,V,W,USC,R,TP,PHI,Z,Y,X
c
c --- UNLUMP called by:  NUMRISE
c --- UNLUMP calls:      none
c----------------------------------------------------------------------
        USE PRIME_PLU

        IMPLICIT NONE

        DOUBLE PRECISION F(7), UA, TA, RA

        U=UA+F(2)/F(1)
        W=F(3)/F(1)
        USC=DSQRT(U*U+W*W)
        TP=TA+F(4)/F(1)
CPES    Limit plume temperature (TP) to be .GE. ambient (TA) - 10 K to
CPES    avoid potential math error.  R. Brode, PES/MACTEC, 6/21/03
        TP = MAX( TP, TA-10.0D0 )
        DEN=(RA*TA)/TP
        R=DSQRT(F(1)/(USC*DEN))
        PHI=DATAN(W/U)
        Z=F(5)
        X=F(6)
        Y=F(7)

        RETURN
        END

c>>>c   This is the old ZMET, replace with new ZMET that uses AERMOD profiles
c>>>c----------------------------------------------------------------------
c>>>      subroutine zmet(z,ua,ra,ta,dudz,dpdz)
c>>>c----------------------------------------------------------------------
c>>>c
c>>>c --- PRIME      Version:  1.0     Level:  970812                  ZMET
c>>>c                X.(J.) Zhang, J. Scire,  EARTH TECH
c>>>c
c>>>c --- PURPOSE:  Obtain ambient met parameters at height z
c>>>c               by interpolation of gridded values
c>>>c
c>>>c --- INPUTS:
c>>>c          Z - real    - Height (m)
c>>>c
c>>>c     Common block /AMBIENT/ variables:
c>>>c           NZA,UAMB(mxnz),RAMB(mxnz),TAMB(mxnz),ZFACEA(mxnzp1),
c>>>c           ZGPTA(mxnz),TAMB0,RAMB0,ADIA,PTGRAD0
c>>>c     Parameters:
c>>>c           MXNZ, MXNZP1
c>>>c
c>>>c --- OUTPUT:
c>>>c         UA - real    - Current ambient wind speed (m/s)
c>>>c         RA - real    - Current ambient air density (kg/m**3)
c>>>c         TA - real    - Current ambient air temperature (deg. K)
c>>>c       DUDZ - real    - Current wind shear (1/s)
c>>>c       DPDZ - real    - Current ambient potential temperature gradient
c>>>c                        (deg. K/m)
c>>>c
c>>>c --- ZMET called by:  NUMRISE
c>>>c --- ZMET calls:      none
c>>>c----------------------------------------------------------------------
c>>>c     Defined at grid center: uamb,tamb,ramb
c>>>c     Defined at zface:       dedz
c>>>c----------------------------------------------------------------------
c>>>c --- Include files
c>>>      include 'params
c>>>      include 'ambient
c>>>
c>>>c --- Interpolate variables defined at grid cell center
c>>>      if(z.lt.zgpta(1))then
c>>>
c>>>c ---    Height is below first grid point
c>>>         zfact=(zgpta(1)-z)/zgpta(1)
c>>>         ta=tamb(1)-(tamb(1)-tamb0)*zfact
c>>>         ra=ramb(1)-(ramb(1)-ramb0)*zfact
c>>>c ---    Wind speed at z=0 is assumed to be zero
c>>>         ua=uamb(1)*(1.0-zfact)
c>>>         dudz=uamb(1)/zgpta(1)
c>>>         dpdz=adia+(tamb(1)-tamb0)/zgpta(1)
c>>>         dpdz=amax1(dpdz,ptgrad0)
c>>>
c>>>      else if(z.lt.zgpta(nza))then
c>>>
c>>>c ---    Find the layer containing height, Z
c>>>         do i=2,nza
c>>>            if(z.le.zgpta(i))then
c>>>               im1=i-1
c>>>               delz=zgpta(i)-zgpta(im1)
c>>>               zfact=(zgpta(i)-z)/delz
c>>>               ta=tamb(i)-(tamb(i)-tamb(im1))*zfact
c>>>               ra=ramb(i)-(ramb(i)-ramb(im1))*zfact
c>>>               ua=uamb(i)-(uamb(i)-uamb(im1))*zfact
c>>>c ---          Compute wind speed gradient & pot. temp. gradient
c>>>               dudz=(uamb(i)-uamb(im1))/delz
c>>>               dpdz=adia+(tamb(i)-tamb(im1))/delz
c>>>               dpdz=amax1(dpdz,ptgrad0)
c>>>               go to 101
c>>>            endif
c>>>         enddo
c>>>
c>>>      else
c>>>
c>>>c ---    Height is at or above the top grid point -- persist values
c>>>c ---    at the top grid cell
c>>>         ta=tamb(nza)
c>>>         ra=ramb(nza)
c>>>         ua=uamb(nza)
c>>>c ---    Hold wind speed and temperature constant above top layer at
c>>>c ---    values at top grid point
c>>>         dudz=0.0
c>>>         dpdz=adia
c>>>         dpdz=amax1(dpdz,ptgrad0)
c>>>      endif
c>>>
c>>>101   continue
c>>>
c>>>      return
c>>>      end

c----------------------------------------------------------------------
      subroutine zmet(z,ua,ra,tap,dudz,dpdz)
c----------------------------------------------------------------------
c
c --- PRIME      Version:  1.0     Level:  970812                  ZMET
c                X.(J.) Zhang, J. Scire,  EARTH TECH
c
c --- PURPOSE:  Obtain ambient met parameters at height z
c               by interpolation of gridded values
c
c --- MODIFIED: For use with the AERMOD model.  Uses AERMOD meteorological
c               profiles.
c               R.W. Brode, PES, Inc. - 07/05/01
c
c --- INPUTS:
c          Z - real    - Height (m)
c
c     Common block /AMBIENT/ variables:
c           NZA,UAMB(mxnz),RAMB(mxnz),TAMB(mxnz),ZFACEA(mxnzp1),
c           ZGPTA(mxnz),TAMB0,RAMB0,ADIA,PTGRAD0
c     Parameters:
c           MXNZ, MXNZP1
c
c --- OUTPUT:
c         UA - real    - Current ambient wind speed (m/s)
c         RA - real    - Current ambient air density (kg/m**3)
c         TA - real    - Current ambient air temperature (deg. K)
c       DUDZ - real    - Current wind shear (1/s)
c       DPDZ - real    - Current ambient potential temperature gradient
c                        (deg. K/m)
c
c --- ZMET called by:  NUMRISE
c --- ZMET calls:      none
c----------------------------------------------------------------------
c     Defined at grid center: uamb,tamb,ramb
c     Defined at zface:       dedz
c----------------------------------------------------------------------
c --- Include files
      USE MAIN1
      USE PRIME_ambient

      IMPLICIT NONE

      DOUBLE PRECISION Z, ua, ra, tap, dudz, dpdz
      DOUBLE PRECISION SVATZ, SWATZ, UATZ, PTZ, DELZ

      INTEGER NDXBLZ

C---- Compute the parameter values at height Z for PRIME
C---- Locate index below height Z

      CALL LOCATE(GRIDHT, 1, MXGLVL, Z, NDXBLZ)

      IF (NDXBLZ .GE. 1) THEN

C----    Sigma_V
         CALL GINTRP( GRIDHT(NDXBLZ), GRIDSV(NDXBLZ),
     &                GRIDHT(NDXBLZ+1), GRIDSV(NDXBLZ+1),
     &                Z, SVATZ )

C----    Sigma_W
         CALL GINTRP( GRIDHT(NDXBLZ), GRIDSW(NDXBLZ),
     &                GRIDHT(NDXBLZ+1), GRIDSW(NDXBLZ+1),
     &                Z, SWATZ )

C----    Wind speed
         CALL GINTRP( GRIDHT(NDXBLZ), GRIDWS(NDXBLZ),
     &                GRIDHT(NDXBLZ+1), GRIDWS(NDXBLZ+1),
     &                Z, UATZ )

C----    Potential temperature gradient
         CALL GINTRP( GRIDHT(NDXBLZ), GRIDTG(NDXBLZ),
     &                GRIDHT(NDXBLZ+1), GRIDTG(NDXBLZ+1),
     &                Z, DPDZ )

C----    Potential temperature
         CALL GINTRP( GRIDHT(NDXBLZ), GRIDPT(NDXBLZ),
     &                GRIDHT(NDXBLZ+1), GRIDPT(NDXBLZ+1),
     &                Z, PTZ )

C----    Ambient air density
         CALL GINTRP( GRIDHT(NDXBLZ), GRIDRHO(NDXBLZ),
     &                GRIDHT(NDXBLZ+1), GRIDRHO(NDXBLZ+1),
     &                Z, RA )

C----    Compute wind speed gradient
         DELZ = (GRIDHT(NDXBLZ+1) - GRIDHT(NDXBLZ))
         DUDZ = (GRIDWS(NDXBLZ+1) - GRIDWS(NDXBLZ)) / DELZ

      ELSE
C        Use GRID value for lowest level
         SVATZ  = GRIDSV(1)
         SWATZ  = GRIDSW(1)
         UATZ   = GRIDWS(1)
         DPDZ   = GRIDTG(1)
         PTZ    = GRIDPT(1)
         RA     = GRIDRHO(1)
         DUDZ   = GRIDWS(1) / GRIDHT(1)
      END IF

C---- Calculate ambient temperature from potential temperature profile
      TAP = PTZ - ADIA * (Z + ZBASE)

CRWB  Modify the treatment of low wind/low turbulence cases per 7/31/96
CRWB  write-up by Steve Perry.  R. Brode, PES, 8/15/96
      SWATZ = MAX( SWATZ, SWMIN )
      SVATZ = MAX( SVATZ, SVMIN, SVUMIN*UATZ )
      IF( L_VECTORWS )THEN
          UA = DSQRT( UATZ*UATZ + 2.0D0*SVATZ*SVATZ )
      ENDIF
      UA  = MAX( UATZ, WSMIN )

      return
      end

c----------------------------------------------------------------------
      subroutine zstream(H,W,L,R,LR,HR,x,y,z,dzdx)
c----------------------------------------------------------------------
c
c --- PRIME      Version:  1.0     Level:  970812               ZSTREAM
c                L. Schulman, J. Scire,   EARTH TECH
c                Prepared for EPRI under contract WO3527-01
c
c --- PURPOSE:  Estimates the local mean slope of streamlines in the
c               vicinity of buildings.  The local slope is equal to
c               w/u, where w is the local mean vertical velocity and
c               u the local mean longitudinal velocity.  For modeling
c               plume rise, the streamline deflection effect is modeled
c               as (w/u)(dx).
c
c --- Modification 2/24/2017
c               Added user control to include AWMADWNW extended diagnostics
c
c --- INPUTS:
c                H - real              - Building height above ground
c                W - real              - Projected building width
c                L - real              - Along-wind building length
c                R - real              - Scale length from H and W
c               LR - real              - Length of downwind cavity from
c                                         lee face
c               HR - real              - Maximum cavity height above
c                                         ground
c                x - real              - downwind distances
c                y - real              - crosswind distances
c                z - real              - heights above ground
c
c --- OUTPUT:
c             dzdx - real              - Vertical streamline slope
c
c --- ZSTREAM called by:  NUMRISE
c --- ZSTREAM calls:      none
c----------------------------------------------------------------------
c
      IMPLICIT NONE
C JAT 06/22/21 D065
C REMOVE EXPZ1,EXPZ2,EXPX AS UNUSED VARIABLES
C      DOUBLE PRECISION H,W,L,R,HR,LR,expz1,expz2,expzg,expx
      DOUBLE PRECISION H,W,L,R,HR,LR,expzg
      DOUBLE PRECISION x,y,z,dzdx2,zslope,dzdx,ypos,hbyw,onebyr,
     &                 wby2,rby2,zg,zslopeLR,yscale

C JAT 06/22/21 D065
C REMOVE EXPZ1,EXPZ2,EXPX AS UNUSED VARIABLES
C      data expx/1.0D0/, expz1/3.0D0/, expz2/1.0D0/
c
c --- Check for a building
      if(h.le.0.0D0)then
         dzdx=0.0D0
         go to 900
      endif

c --- Initialize
      dzdx2 = 0.0D0

c --- Set a few constants
      hbyw=H/W
      ypos=DABS(y)
      onebyr=1.0D0/R
      wby2=0.5D0*W
      rby2=0.5D0*R

c --- Power law exponent for slope approaching zero at ground
c --- Exponent modified for tall, narrow buidings
c --- zg is level below which slope is zero in far wake
      zg=0.0D0
      expzg=0.3D0
      if(hbyw .ge. 2.0D0) expzg=expzg*(0.5D0*hbyw)**2

c
c --- Local streamline slope (zslope) at z=H
c --- Local two-dimensional streamline slope (dzdx2)
c --- Local three-dimensional streamline slope (dzdx)
c --- (x,y,z)=(0,0,0) at ground of center of upwind building face
c
      if(x .lt. -R) then
c ---    Upwind of building influence
         zslope = 0.0D0
         dzdx2  = 0.0D0

      elseif(x .lt. 0.0D0) then
c ---    Ascent upwind of building:
c ---    parobolic fit to slope=0 at (-R,0) with increasing slope
c ---    to (0,(HR-H))
c ---    vertical decay above building using expz1
c ---    below building nonzero slope above 2/3 H for R<H reducing
c ---    to ground as R approaches 2H
         zslope = 2.0D0*(HR-H)*(x+R)*onebyr**2
         if(z .gt. H) then
c ---       dzdx2 = zslope/((z-H+R)*onebyr)**expz1
c ---               recode with explicit exponent, expz1=3
            dzdx2 = zslope/((z-H+R)*onebyr)**3
         elseif(R .le. H .and. z .le. 0.67D0*H) then
            dzdx2 = 0.0D0
         elseif(R .le. H .and. z .gt. 0.67D0*H) then
            dzdx2 = zslope
         elseif(R .gt. H .and. z .le. 0.67D0*(2.0D0*H-R)) then
            dzdx2 = 0.0D0
         elseif(R .gt. H .and. z .gt. 0.67D0*(2.0D0*H-R)) then
            dzdx2 = zslope
         else
            print *,'z out of bounds      ',x,z
         endif

      elseif(x .le. rby2) then
c ---    Ascent over building
c ---    parobolic fit from (0,0) with decreasing slope to
c ---    to (0.5R,(HR-H))
c ---    vertical decay above building using expz1
         zslope = (-(HR-H)*4.0D0*onebyr)*(2.0D0*x*onebyr-1.0D0)
         if(z .le. H) then
            dzdx2 = zslope
         else
c ---       dzdx2 = zslope/((z-H+R)*onebyr)**expz1
c ---               recode with explicit exponent, expz1=3
            dzdx2 = zslope/((z-H+R)*onebyr)**3
         endif

      elseif(x .le. L+LR) then
c ---    Descent over building to end of near wake
c ---    parobolic fit from (.5R,(HR-H)) with increasing slope to
c ---    to (L+LR,-H/2)
c ---    vertical decay above z=H using expz2
c ---    vertical decay below z=H using expzg
         zslope = (HR-H)*(R-2.0D0*x)/((L-rby2+LR)**2)
         if(z .gt. H) then
c ---       dzdx2 = zslope/((z-H+R)*onebyr)**expz2
c ---               recode without expz2 since expz2=1
            dzdx2 = zslope/((z-H+R)*onebyr)
         elseif(z .le. zg) then
            dzdx2 = 0.0D0
         else
            dzdx2 = zslope*((z-zg)/(H-zg))**expzg
         endif

      else
c ---    Descent beyond near wake (far wake)
c ---    horizontal decay beyond L+LR using expx
c ---    vertical decay above z=H using expz2
c ---    vertical decay below z=H using expzg
         zslopeLR  = -2.0D0*(HR-H)/(L-rby2+LR)
c ---    zslope = zslopeLR/((x-(L+LR-R))*onebyr)**expx
c ---             recode without expx since expx=1
         zslope = zslopeLR/((x-(L+LR-R))*onebyr)
         if(z .gt. H) then
c ---       dzdx2 = zslope/((z-H+R)*onebyr)**expz2
c ---               recode without expz2 since expz2=1
            dzdx2 = zslope/((z-H+R)*onebyr)
         elseif(z .le. zg) then
            dzdx2 = 0.0D0
         else
            dzdx2 = zslope*((z-zg)/(H-zg))**expzg
         endif

      endif

c --- Calculate 3-D slopes,: dzdx : from 2-d centerplane slope,: dzdx2
      if(ypos .gt. (wby2+R/3.0D0))then
         dzdx=0.0D0
      elseif(ypos .le. wby2)then
         dzdx=dzdx2
      else
         yscale=1.0D0+(3.0D0*onebyr)*(wby2-ypos)
         dzdx=dzdx2*yscale
      endif

900   continue

      return
      end

c-----------------------------------------------------------------------
      subroutine position(x,y,z,ipositn)
c-----------------------------------------------------------------------
c
c --- PRIME      Version:  1.0     Level:  970812              POSITION
c                L. Schulman, J. Scire, D. Strimaitis,   EARTH TECH
c                Prepared for EPRI under contract WO3527-01
c
c --- PURPOSE:  Identifies if (x,y,z) location is in the building,
c               in the near wake, in the far wake, or outside.
c               IPOSITN is set to: 1 if within the bldg.
c                                  2 if within the near wake
c                                  3 if within the far wake
c                                  4 if outside wake region
c
c --- INPUTS:
c                x - real              - downwind distance from center
c                                        of upwind face of bldg
c                y - real              - crosswind distance from center
c                                        of upwind face of bldg
c                z - real              - height above ground
c
c     Common block /WAKEDAT/ variables:
c           Hb,Wb,xLb,Rb,HR,xLR,xLC
c
c --- OUTPUT:
c
c          ipositn - integer           - 1 if (x,y,z) within building
c                                        2 if location within near wake
c                                        3 if location within far wake
c                                        4 if location outside
c
c --- POSITION called by:  NUMRISE, PCALC (HOST subroutine)
c --- POSITION calls:      WAKE_DIM
c----------------------------------------------------------------------
c
c --- Include commons
      USE PRIME_wakedat

      IMPLICIT NONE

      DOUBLE PRECISION x, y, z, ypos, rby2, rby3, wby2, xtest, ytest,
     &                 ztest, zcav, ycav, zwake, ywake
      DOUBLE PRECISION, PARAMETER :: zero = 0.0D0,
     &                               half = 0.5D0
      DOUBLE PRECISION, PARAMETER :: skin = 0.99998D0    ! fractional boundary just inside building

      INTEGER IPOSY, IPOSZ, IPOSITN

c --- Initialize
      iposy=4
      iposz=4
      ipositn=4

c --- Screen out any cases without building
      if(Hb.le.zero) return

c --- Screen out positions upwind of building (and on windward face)
      if(x.le.zero) return

c --- Set y positive for calculations
      ypos=DABS(y)

c --- Set selected length scale products
      rby2=half*Rb
      rby3=third*Rb
      wby2=half*Wb

c --- Set ipositn to 1 if location within building
      xtest=x/xLB
      ytest=ypos/wby2
      ztest=z/Hb
      if(xtest.lt.skin .and. ztest.lt.skin .and. ytest.lt.skin) then
         ipositn=1
         return
      endif

c --- Calculate if location below height of near wake boundary
      if(xLC .lt. xLb)then
c ---    Reattachment
         if(x.lt.xLb) then
c ---       Cavity height equal to building height
            zcav=Hb
            if(z .le. zcav) iposz=2
         elseif(x.lt.(xLb+xLR)) then
c ---       Cavity height is ellipse with a=LR and b=H
            zcav=Hb*DSQRT(1.0D0-((x-xLb)/xLR)**2)
            if(z .le. zcav) iposz=2
         endif
      else
c ---    No reattachment
         if(x.le.rby2) then
c ---       Cavity height is parabola with vertex at height MAX(0.5R,HR)
c ---       and passing thru upwind building edge (0,H)
            zcav=HR+4.0D0*(x-rby2)**2*(Hb-HR)/(Rb**2)
            if(z .le. zcav) iposz=2
         elseif(x.lt.(xLb+xLR)) then
c ---       Cavity height is ellipse with a=LR+L-0.5R and b=HR
            zcav=HR*DSQRT(1.0D0-((x-rby2)/(xLb+xLR-rby2))**2)
            if(z .le. zcav) iposz=2
         endif
      endif

c --- Calculate x-y near wake boundary
      if(x.le.Rb) then
c ---    Cavity width is parabola with vertex @ width MAX(R,W/2+R/3)
c ---    and passing thru upwind building edge (0,W/2)
         ycav=(wby2+rby3)-(x-Rb)**2/(3.0D0*Rb)
         if(ypos .le. ycav) iposy=2
      elseif(x.lt.(xLb+xLR)) then
c ---    Cavity width is ellipse with a=W/2+R/3 and b=LR+L-R
         ycav=(wby2+rby3)*DSQRT(1.0D0-((x-Rb)/(xLb+xLR-Rb))**2)
         if(ypos .le. ycav) iposy=2
      endif

c --- Set ipositn to 2 if (x,y,z) location within near wake
      if( iposz .eq. 2 .and. iposy .eq. 2) ipositn=2

c --- Test for position in far wake if still 4
      if(ipositn.EQ.4) then
         call WAKE_DIM(x,Hb,Wb,Rb,zwake,ywake)
         if(z.le.zwake .AND. ypos.le.ywake) ipositn=3
      endif

      return
      end

c----------------------------------------------------------------------
      subroutine numgrad(x,xtr,ztr,ntr,zeff)
c----------------------------------------------------------------------
c
c --- PRIME      Version:  1.0     Level:  970812               NUMGRAD
c                J. Scire,  EARTH TECH
c                Prepared for EPRI under contract WO3527-01
c
c --- PURPOSE:  Compute the effective gradual plume height by
c               interpolation of the stored values.  Effective
c               plume height is the stack height + plume rise.
c
c --- INPUTS:
c                X - real       - Downwind distance (m)
c         XTR(ntr) - real array - Array of stored downwind distances (m)
c         ZTR(ntr) - real array - Array of stored effective plume height
c                                 at each downwind distance
c              NTR - integer    - Number of stored values in XTR, ZTR
c
c --- OUTPUT:
c             ZEFF - real       - Effective plume height (m) at
c                                 downwind distance X
c
c --- NUMGRAD called by:  PHEFF
c --- NUMGRAD calls:      none
c----------------------------------------------------------------------
c
      IMPLICIT NONE

      INTEGER I, NTR, NTRM1, IP1
      DOUBLE PRECISION x, xtr(ntr), ztr(ntr), zeff
c
      if(x.ge.xtr(ntr))then
         zeff=ztr(ntr)
      else
         ntrm1=ntr-1
         zeff=ztr(1)
         do i=ntrm1,1,-1
            if(x.ge.xtr(i))then
               ip1=i+1
               zeff=ztr(ip1)-(ztr(ip1)-ztr(i))*(xtr(ip1)-x)/
     1          (xtr(ip1)-xtr(i))
               return
            endif
         enddo
      endif

      return
      end

c----------------------------------------------------------------------
      subroutine wake_drdx(x,drdx)
c----------------------------------------------------------------------
c
c --- PRIME      Version:  1.0     Level:  970812             WAKE_DRDX
c                J. Scire, D. Strimaitis,  EARTH TECH
c                Prepared for EPRI under contract WO3527-01
c
c --- PURPOSE:  Compute the plume radius growth rate in the wake
c               by interpolating among the stored values.
c
c --- INPUTS:
c                X - real       - Downwind distance (m) from source
c
c     Common block /PARAMS/ variables:
c           MXNTR
c     Common block /WAKEDAT/ variables:
c           NWAK, XWAK(mxntr), DRWAK(mxntr)
c
c --- OUTPUT:
c             DRDX - real       - Rate of growth of plume radius at
c                                 downwind distance X from source
c
c --- WAKE_DRDX called by:  NUMRISE
c --- WAKE_DRDX calls:      none
c----------------------------------------------------------------------
c
      USE PRIME_wakedat

      IMPLICIT NONE

      DOUBLE PRECISION x, drdx
      INTEGER I, NWKM1, IP1
c
c --- Set growth rate to zero outside interpolation region
c --- (all x outside wake)
      if(x.gt.xwak(nwak) .OR. x.lt.xwak(1))then
         drdx=0.0D0
      elseif(nwak.le.1) then
c ---    Wake turbulence does not alter this plume
         drdx=0.0D0
      else
         nwkm1=nwak-1
         drdx=drwak(1)
         do i=nwkm1,1,-1
            if(x.ge.xwak(i))then
               ip1=i+1
               drdx=drwak(ip1)-(drwak(ip1)-drwak(i))*(xwak(ip1)-x)/
     1              (xwak(ip1)-xwak(i))
               return
            endif
         enddo
      endif

      return
      end

c----------------------------------------------------------------------
      subroutine wake_ini(kurdat,ldbhr,DBGUNT,rural,dsbh,dsbw,dsbl,
     &                    xadj,yadj,ubldg,ustack)
c----------------------------------------------------------------------
c
c --- PRIME      Version:  1.0     Level:  970812              WAKE_INI
c                D. Strimaitis, EARTH TECH
c                Prepared for EPRI under contract WO3527-01
c
c --- PURPOSE:  Refreshes variables in /wakedat/ common
c
c --- INPUTS:
c
c      LDBHR - logical - Debug output written when .TRUE.
c      RURAL - logical - Denotes rural dispersion when .TRUE.
c       DSBH - real    - Effective building height (m)
c       DSBW - real    - Effective building width (m) across flow
c       DSBL - real    - Effective building length (m) along flow
c       XADJ - real    - Distance (m) from source to upwind face of bldg
c                        along flow
c       YADJ - real    - Distance (m) from source to center of upwind
c                        face of bldg across flow
c      UBLDG - real    - Wind speed (m/s) at top of building
c     USTACK - real    - Wind speed (m/s) at release height
c
c     Parameters:
c           MXNTR
c
c --- OUTPUT:
c
c     Common block /WAKEDAT/ variables:
c           HB,WB,XLB,RSCALE,HR,XLR,XLC,XBADJ,YBADJ,
c           NWAK, XWAK(mxntr), SZWAK(mxntr), SYWAK(mxntr),
c           DRWAK(mxntr), XZVWAK, XYVWAK, UB, URH,
c           LRURL, ISTAB
c
c --- WAKE_INI called by:  PCALC (HOST subroutine)
c --- WAKE_INI calls:      WAKE_SCALES
c----------------------------------------------------------------------

c --- Include common blocks
      USE PRIME_wakedat

      IMPLICIT NONE

      DOUBLE PRECISION :: dsbh,dsbw,dsbl,xadj,yadj,ubldg,ustack
      DOUBLE PRECISION, PARAMETER :: zero = 0.0D0

      integer kurdat, DBGUNT

      logical rural,ldbhr

c --- Calculate global variable, third=1/3, used in various places
      third = 1.0D0/3.0D0

c --- Transfer arguments to /wakedat/ variables
      lrurl = rural
      Hb    = dsbh
      Wb    = dsbw
      xLb   = dsbl
      xbadj = xadj
      ybadj = yadj
      Ub    = ubldg
      Urh   = ustack

c --- Compute wake dimensions and related parameters
      call WAKE_SCALES(kurdat,ldbhr,DBGUNT)

c --- Reset contents of sigma arrays for wake region
      nwak=1
      xwak(1)=zero
      szwak(1)=zero
      sywak(1)=zero
      drwak(1)=zero

caerc --- Reset virtual distances for sigmas beyond wake
caer      xzvwak=zero
caer      xyvwak=zero

      return
      end

c-----------------------------------------------------------------------
      subroutine wake_scales(kurdat,ldbhr,DBGUNT)
c-----------------------------------------------------------------------
c
c --- PRIME      Version:  1.0     Level:  970812           WAKE_SCALES
c                L. Schulman, D. Strimaitis,  J. Scire,   EARTH TECH
c                Prepared for EPRI under contract WO3527-01
c
c --- PURPOSE:  Calculates length scale and wake dimensions
c
c --- INPUTS:
c            LDBHR - logical           - Control variable for debug
c                                        write statements
c
c     Common block /WAKEDAT/ variables:
c           Hb,Wb,xLb
c     Parameters:  IO6
c
c --- OUTPUT:
c
c     Common block /WAKEDAT/ variables:
c           Rb,HR,xLR,xLC
c
c --- WAKE_SCALES called by:  WAKE_INI
c --- WAKE_SCALES calls:      none
c----------------------------------------------------------------------
c
c --- Include commons
      USE PRIME_params
      USE PRIME_wakedat

      IMPLICIT NONE

      DOUBLE PRECISION twoby3, rw, rl, hh, ww, explh

      integer kurdat, DBGUNT

      logical ldbhr

c --- Set misc. constants, third=1/3 defined in sub. wake_ini
      twoby3 = 2.0D0*third

      if(HB.le.0.0D0) then
c ---    No building
         Rb=0.0D0
         Hb=0.0D0
         xLR=0.0D0
         xLC=0.0D0
      else
c
c ---    Set ratios
         rw = Wb/Hb
         rl = xLb/Hb
c ---    Fackrell limits on aspect ratio L/H
         if(rl .lt. 0.3D0) rl=0.3D0
         if(rl .gt. 3.0D0) rl=3.0D0
c
c ---    Length scale R --- Wilson
c ---    Wilson limits for length scale R
c ---    H/W or W/H not greater than 8 --  already behaves as 2-D
         HH=Hb                    ! only modify H to calculate R
         WW=Wb                    ! only modify W to calculate R
         if(HH.gt.8.0D0*WW)HH=8.0D0*WW
         if(WW.gt.8.0D0*HH)WW=8.0D0*HH
         Rb= (MIN(HH,WW)**twoby3) * (MAX(HH,WW)**third)
c
c ---    Reattachment for LC < L
         xLC = 0.9D0*Rb
c
c ---    Recirculation cavity length---Fackrell
c ---    Modify Fackrell for W/H less than 1 by weakening dependence
c ---    on L/H.  Snyder found that cavity did not increase in length
c ---    as W/H = L/H decreased from 1 to 0.33.
c ---    Let L/H dependence decrease from Fackrell dependence at W/H=1
c ---    to no dependence at W/H=0.33.
         explh = 0.3D0
         if(rw .lt. 1.0D0) explh=MAX(0.0D0,0.3D0*(rw-0.33D0)/0.67D0)
         xLR = 1.8D0*Wb/(rl**explh*(1.0D0+0.24D0*rw))
c
c ---    Maximum cavity height  (Wilson,ASHRAE):
         HR = Hb+0.22D0*Rb

      endif

c --- Write the results
      if(ldbhr)then
         write(DBGUNT,*)
         write(DBGUNT,*)'YR/MN/DY/HR:      ', kurdat
         write(DBGUNT,*)
         write(DBGUNT,*)'WAKE_SCALES inputs: '
         write(DBGUNT,*)'   HB    = ',Hb,' (m)'
         write(DBGUNT,*)'   WB    = ',Wb,' (m)'
         write(DBGUNT,*)'   LB    = ',xLb,' (m)'
         write(DBGUNT,*)
         write(DBGUNT,*)'WAKE_SCALES output: '
         write(DBGUNT,*)'   Scale length (R)               = ',Rb
         write(DBGUNT,*)'   Max. cavity height (HR)        = ',HR
         write(DBGUNT,*)'   Length of downwind cavity (LR) = ',xLR
         write(DBGUNT,*)'   Length of roof cavity (LC)     = ',xLC
      endif
c
      return
      end

c-----------------------------------------------------------------------
      subroutine wake_dfsn(ldbhr,xi,szi,syi,z,DBGUNT)
c-----------------------------------------------------------------------
c
c --- PRIME      Version:  1.0     Level:  970812             WAKE_DFSN
c                L. Schulman, D. Strimaitis,   EARTH TECH
c                Prepared for EPRI under contract WO3527-01
c
c --- PURPOSE: Tabulates sigmas and rate of plume growth as function
c              of location within the wake from modified Weil (1996)
c              analytical expressions
c
c --- MODIFIED: To initialize dummy variables zkdum and ykdum to 0.0.
c               Otherwise these variables may be undefined in subrountine
c               WAKE_SIG under some circumstances.
c               R.W. Brode, MACTEC (f/k/a PES), Inc. - 09/01/05
c
c --- MODIFIED: To modify calling arguments in call to subroutine
c               WAKE_SIG from turby and turbz to wakiy and wakiz.
c               R.W. Brode, PES, Inc. - 07/05/01
c
c --- MODIFIED: For use with the AERMOD model. Use virtual source sigmas
c               added in quadrature to ambient sigmas instead of
c               virtual source distances.
c               R.W. Brode, PES, Inc. - 07/05/01
c
c --- INPUTS:
c            ldbhr - logical     - Flag for debug write statements
c                                  to upwind bldg wall
c               xi - real        - distance (m) from upwind bldg wall
c                                  to point where plume intersects wake
c              szi - real        - sigma-z (m) at xi
c              syi - real        - sigma-y (m) at xi
c                z - real        - plume height (m) at xi
c
c     Common block /PARAMS/ variables:
c           MXNTR, MXNW
c     Common block /WAKEDAT/ variables:
c           XBADJ, Hb, Wb, xLb, Rb, xLR
c
c --- OUTPUT:
c
c     Common block /WAKEDAT/ variables:
c           NWAK, XWAK(mxntr), SZWAK(mxntr), SYWAK(mxntr),
c           DRWAK(mxntr), XZVWAK, XYVWAK,
c           NCAV, XCAV(mxntr), SZCAV(mxntr), SYCAV(mxntr),
c           XZVCAV, XYVCAV, LRURL, ISTAB
c
c --- WAKE_DFSN called by:  NUMRISE
c --- WAKE_DFSN calls    :  SIGZPR, SIGYPR,
c                           WAKE_XA, WAKE_CAV0, WAKE_TURB, WAKE_SIG
c----------------------------------------------------------------------
c
      USE PRIME_params
      USE PRIME_numparm
      USE PRIME_wakedat
      USE MAIN1, ONLY: AWMADWDBUNT, AWMADWDBG

      IMPLICIT NONE

c --- Define local variable arrays for fine-steps
      DOUBLE PRECISION dist(mxnw),asigz(mxnw),asigy(mxnw),dsz(mxnw)
      DOUBLE PRECISION csigz(mxnw),csigy(mxnw)
      DOUBLE PRECISION xi,szi,syi,z
      DOUBLE PRECISION xamx,xamn,xay,zkdum,ykdum,xcave,distc,xbc
      DOUBLE PRECISION xaz,xdc,szcav0,sycav0,xd,dx,xlow,xhi,xrange
      DOUBLE PRECISION dxi,x,xold,sycav0r,xmid,wakiz,wakiy,zk,yk,zkc,
     &                 ykc,dzrate,xnew,sigzxa,sydum,sz,szdum,sigyxa,sy,
     &                 deln,rn
      DOUBLE PRECISION, PARAMETER :: zero=0.0D0, half=0.5D0, one=1.0D0,
     &                               rtpiby2 = 1.25331413731550D0

      INTEGER I,NP,NWS,NCS,N,IR,NPW,IN,JN,INP,NPC,DBGUNT
      logical ldbhr,lcav,lwak,lrevcav

c --- Compute xa, where turbulent growth in the wake transitions
c --- to ambient growth rate, measured from upwind face of bldg
      call WAKE_XA(xLb,Rb,xaz,xay)
      xamx=MAX(xaz,xay)
      xamn=MIN(xaz,xay)

c --- Initialize virtual source sigma terms
      vsigy  = 0.0D0
      vsigz  = 0.0D0
      vsigyc = 0.0D0
      vsigzc = 0.0D0

c --- Initialize dummy variables for zk and yk
      zkdum = 0.0D0
      ykdum = 0.0D0

c --- Initialize CAVITY parameters
c --------------------------------
c --- Set distance from upwind face of bldg to END of cavity
      xcave=xLb+xLR
c --- Set distance from source to start of cavity
      distc=xLb+xbadj
c --- Set downwind distance to effective cavity source (when present),
c --- from the upwind face of bldg
      xbc=MAX(xi,xLb)
      xbc=MIN(xbc,xcave)
c --- Location of downwind edge of PDF region from effective cavity
c --- source
      xdc=xbc+xLR
c --- Set initial sigmas for cavity source using sigma-y at xi
      call WAKE_CAV0(syi,szcav0,sycav0)
c --- The cavity sigma-y will need to be revised if xi lies upwind of
c --- the downwind face of the bldg.
      if(xi.LT.xLb) then
         lrevcav=.TRUE.
      else
         lrevcav=.FALSE.
      endif

c --- Determine if any plume material in cavity may be modeled
c ------------------------------------------------------------
c --- Initialize output arrays
      ncav=1
      xcav(1)=xbc+xbadj
      szcav(1)=szcav0
      sycav(1)=sycav0
      if(xi.GE.xcave) then
         lcav=.FALSE.
         lrevcav=.FALSE.
      else
         lcav=.TRUE.
      endif

c --- Is plume affected by wake turbulence?
c -------------------------------------------
c --- Initialize output arrays
      nwak=1
      xwak(1)=xi+xbadj
      szwak(1)=szi
      sywak(1)=syi
      drwak(1)=zero
      if(xi.GE.xamx) then
         lwak=.FALSE.
         if(ldbhr) then
            write(DBGUNT,*) ' '
            write(DBGUNT,*)'----- WAKE_DFSN:        NWAK = ',nwak
            write(DBGUNT,*)'Z-dispersion reaches ambient at: ',xaz+xbadj
            write(DBGUNT,*)'Y-dispersion reaches ambient at: ',xay+xbadj
caer            write(DBGUNT,*)'z,y virtual distances (m)    = ',xzvwak,xyvwak
            write(DBGUNT,'(1x,a,2x,3(f14.5,2x))')
     &                     'xadj, yadj, xi  (m) = ',xbadj,ybadj,xi
            write(DBGUNT,*)'Plume NOT altered by wake turbulence!'
            write(DBGUNT,*)
         endif
         if (AWMADWDBG) then
            write(AWMADWDBUNT,*) ' '
            write(AWMADWDBUNT,*)'----- WAKE_DFSN:        NWAK = ',nwak
            write(AWMADWDBUNT,*)'Z-dispersion reaches ambient at: ',
     &                                                         xaz+xbadj
            write(AWMADWDBUNT,*)'Y-dispersion reaches ambient at: ',
     &                                                         xay+xbadj
            write(AWMADWDBUNT,'(1x,a,2x,3(f14.5,2x))')
     &                     'xadj, yadj, xi  (m) = ',xbadj,ybadj,xi
            write(AWMADWDBUNT,*)'Plume NOT altered by wake turbulence!'
            write(AWMADWDBUNT,*)
         endif
      else
         lwak=.TRUE.
      endif

c --- Return now if sigmas in wake do not need to be tabulated
      if(.NOT.lwak .AND. .NOT.lcav) return

c --- Compute location of downwind edge of PDF region from xi
      xd=xi+xLR

c --- Set stepping parameters
      dx=2.0D0
c --- Range of table is from point of entry into wake (xi), to the point
c --- at which ambient growth rate resumes (xa), plus one "ds" so that
c --- both sigmas reach ambient, and virtual distances are computed.
c --- When cavity sigmas are also computed, range may start at the
c --- downwind bldg face, and extend to end of cavity.
      xlow=xi
      xhi=xamx
      if(lcav) then
         xlow=MIN(xi,xbc)
         xhi=MAX(xamx,xcave)
      endif
      xrange=xhi-xlow+dx
      np=IDNINT(xrange/dx)+1
      np=MIN(np,mxnw-1)
      dx=xrange/(DBLE(np)-one)
      dxi=one/dx
      nws=0
      ncs=0

c --- Fill first element of marching arrays using values at xlow
      dist(1)=xlow+xbadj
      if(lwak) then
         asigz(1)=szi
         asigy(1)=syi
c ---    Set inital plume growth rate in wake to zero
         dsz(1)=zero
      endif
      if(lcav) then
         csigz(1)=szcav0
         csigy(1)=sycav0
      endif

c --- Initialize distance (from upwind face of bldg)
      x=xlow

c --- Loop over steps in wake region
c -----------------------------------
      do n=2,np
         xold=x
         x=x+dx
         dist(n)=dist(n-1)+dx

c ---    Check to see if cavity data should be revised based on
c ---    data from previous step
         if(lrevcav .AND. xold.GE.xLb) then
            call WAKE_CAV0(asigy(n-1),szcav0,sycav0r)
            if(sycav0r.GT.sycav0) then
               sycav0=sycav0r
               sycav(1)=sycav0
c ---          Replace sigma-y values in stepping arrays
               do ir=1,n-1
                  csigy(ir)=MAX(csigy(ir),sycav0)
               enddo
            endif
            lrevcav=.FALSE.
         endif

c ---    Obtain sigmas for this step

c ---    First, persist initial values if upwind of starting point
         if(lwak .AND. (xi.GE.x)) then
            asigz(n)=asigz(n-1)
            asigy(n)=asigy(n-1)
            dsz(n)=dsz(n-1)
c ---       Set index for skipping entry when filling wake arrays
            nws=n
         endif
         if(lcav .AND. (xbc.GE.x)) then
            csigz(n)=szcav0
            csigy(n)=sycav0
c ---       Set index for skipping entry when filling cav arrays
            ncs=n
         endif

c ---    Now test again and apply full treatment when needed
         if(xold.GT.xamx) then
c ---       Ambient growth region in wake: use virtuals
            if(lwak .AND. (xi.LT.x)) then
               vsigz = max( vsigz, szi )
               vsigy = max( vsigy, syi )
               call SIGZPR(dist(n),z,asigz(n))
               call SIGYPR(dist(n),z,asigy(n))
               asigz(n) = DSQRT( asigz(n)**2 + vsigz**2 )
               asigy(n) = DSQRT( asigy(n)**2 + vsigy**2 )
               dsz(n)=(asigz(n)-asigz(n-1))*dxi
            endif
c ---       Cavity source ---
            if(lcav .AND. (xbc.LT.x)) then
               vsigzc = max( vsigzc, szcav0 )
               vsigyc = max( vsigyc, sycav0 )
               call SIGZPR(dist(n),0.0D0,csigz(n))
               call SIGYPR(dist(n),0.0D0,csigy(n))
               csigz(n) = DSQRT( csigz(n)**2 + vsigzc**2 )
               csigy(n) = DSQRT( csigy(n)**2 + vsigyc**2 )
            endif
         else
            if(x.LT.xamn) then
c ---          Wake growth for both sigz and sigy
c ---          Set x at mid-point of step
               xmid=half*(x+xold)
c ---          Compute turbulence intensities at midpoint
               call WAKE_TURB(xmid,xLb,Rb,wakiz,wakiy)
               if(lwak .AND. (xi.LE.x)) then
c ---             Compute sigmas in wake
                  call WAKE_SIG(x,xd,xold,wakiz,wakiy,asigz(n-1),
     &                          asigy(n-1),Hb,Wb,Rb,zk,yk,
     &                          asigz(n),asigy(n),dsz(n))
               endif
c ---          Cavity source ---
               if(lcav .AND. (xbc.LE.x)) then
                  call WAKE_SIG(x,xdc,xold,wakiz,wakiy,csigz(n-1),
     &                          csigy(n-1),Hb,Wb,Rb,zkc,ykc,
     &                          csigz(n),csigy(n),dzrate)
                     csigz(n) = MAX( csigz(n), szcav0 )       ! ctmp cavity bug fix
               endif
            else
c ---          At least one of the sigmas reaches ambient growth in wake
c ---          Process SIGMA-Z
               if(xold.GE.xaz) then
c ---             Ambient growth region in wake: use virtual x
                  if(lwak .AND. (xi.LE.x)) then
                     call SIGZPR(dist(n),z,asigz(n))
                     vsigz = max( vsigz, szi )
                     asigz(n) = DSQRT( asigz(n)**2 + vsigz**2 )
                     dsz(n)=(asigz(n)-asigz(n-1))*dxi
                  endif
c ---             Cavity source ---
                  if(lcav .AND. (xbc.LE.x)) then
                     call SIGZPR(dist(n),0.0D0,csigz(n))
                     csigz(n) = DSQRT( csigz(n)**2 + vsigzc**2 )
                     csigz(n) = MAX( csigz(n), szcav0 )       ! ctmp cavity bug fix
                  endif
               elseif(x.GE.xaz) then
c ---             Transition from wake to ambient
                  xnew=xaz
                  xmid=half*(xnew+xold)
c ---             Compute turbulence intensities at midpoint
                  call WAKE_TURB(xmid,xLb,Rb,wakiz,wakiy)
                  if(lwak .AND. (xi.LE.xnew)) then
c ---                Compute wake sigma at xaz
                     call WAKE_SIG(xnew,xd,xold,wakiz,wakiy,asigz(n-1),
     &                             asigy(n-1),Hb,Wb,Rb,zk,ykdum,
     &                             sigzxa,sydum,dzrate)
c ---                Get virtual source term as difference in quadrature between
c ---                wake and ambient sigmas at transition distance
                     call SIGZPR(xaz+xbadj,z,sz)
                     if (sigzxa .gt. sz) then
                        vsigz = DSQRT( sigzxa**2 - sz**2 )
                     else
                        vsigz = 0.0D0
                     end if
c ---                Now compute sigma at dist(n) with virtual source
                     call SIGZPR(dist(n),z,asigz(n))
                     asigz(n) = DSQRT( asigz(n)**2 + vsigz**2 )
                     dsz(n)=(asigz(n)-asigz(n-1))*dxi
                  endif
c ---             Cavity source ---
                  if(lcav .AND. (xbc.LE.xnew)) then
c ---                Compute wake sigma at xaz
                     call WAKE_SIG(xnew,xdc,xold,wakiz,wakiy,csigz(n-1),
     &                             csigy(n-1),Hb,Wb,Rb,zkc,ykdum,
     &                             sigzxa,sydum,dzrate)
c ---                Get virtual source term as difference in quadrature between
c ---                wake and ambient sigmas at transition distance
                     call SIGZPR(xaz+xbadj,0.0D0,sz)
                     if (sigzxa .gt. sz) then
                        vsigzc = DSQRT( sigzxa**2 - sz**2 )
                     else
                        vsigzc = 0.0D0
                     end if
                     vsigzc = MAX( vsigzc, szcav0 )           ! ctmp cavity bug fix
c ---                Now compute sigma at dist(n) with virtual source
                     call SIGZPR(dist(n),0.0D0,csigz(n))
                     csigz(n) = DSQRT( csigz(n)**2 + vsigzc**2 )
                  endif
               else
c ---             Wake growth for sigz
c ---             Set x at mid-point of step
                  xmid=half*(x+xold)
c ---             Compute turbulence intensities at midpoint
                  call WAKE_TURB(xmid,xLb,Rb,wakiz,wakiy)
                  if(lwak .AND. (xi.LE.x)) then
c ---                Compute sigmaz
                     call WAKE_SIG(x,xd,xold,wakiz,wakiy,asigz(n-1),
     &                             asigy(n-1),Hb,Wb,Rb,zk,ykdum,
     &                             asigz(n),sydum,dsz(n))
                  endif
c ---             Cavity source ---
                  if(lcav .AND. (xbc.LE.x)) then
                     call WAKE_SIG(x,xdc,xold,wakiz,wakiy,csigz(n-1),
     &                             csigy(n-1),Hb,Wb,Rb,zkc,ykdum,
     &                             csigz(n),sydum,dzrate)
                     csigz(n) = MAX( csigz(n), szcav0 )       ! ctmp cavity bug fix
                  endif
               endif
c ---          Process SIGMA-Y
               if(xold.GE.xay) then
c ---             Ambient growth region in wake: use virtual x
                  if(lwak .AND. (xi.LE.x)) then
                     call SIGYPR(dist(n),z,asigy(n))
                     vsigy = max( vsigy, syi )
                     asigy(n) = DSQRT( asigy(n)**2 + vsigy**2 )
                  endif
c ---             Cavity source ---
                  if(lcav .AND. (xbc.LE.x)) then
                     call SIGYPR(dist(n),0.0D0,csigy(n))
                     vsigyc = max( vsigyc, sycav0 )
                     csigy(n) = DSQRT( csigy(n)**2 + vsigyc**2 )
                  endif
               elseif(x.GE.xay) then
c ---             Transition from wake to ambient
                  xnew=xay
                  xmid=half*(xnew+xold)
c ---             Compute turbulence intensities at midpoint
                  call WAKE_TURB(xmid,xLb,Rb,wakiz,wakiy)
                  if(lwak .AND. (xi.LE.xnew)) then
c ---                Compute sigma at xay
CRWB                     call WAKE_SIG(xnew,xd,xold,turbz,turby,asigz(n-1),
CRWB                 turbz and turby appear to be the wrong variables for this
CRWB                 call to WAKE_SIG, try wakiz and wakiy
                     call WAKE_SIG(xnew,xd,xold,wakiz,wakiy,asigz(n-1),
     &                             asigy(n-1),Hb,Wb,Rb,zkdum,yk,
     &                             szdum,sigyxa,dzrate)
c ---                Get virtual source term as difference in quadrature between
c ---                wake and ambient sigmas at transition distance
                     call SIGYPR(xay+xbadj,z,sy)
                     if (sigyxa .gt. sy) then
                        vsigy = DSQRT( sigyxa**2 - sy**2 )
                     else
                        vsigy = 0.0D0
                     end if
c ---                Now compute sigma at dist(n) with virtual source
                     call SIGYPR(dist(n),z,asigy(n))
                     asigy(n) = DSQRT( asigy(n)**2 + vsigy**2 )
                  endif
c ---             Cavity source ---
                  if(lcav .AND. (xbc.LE.xnew)) then
                     call WAKE_SIG(xnew,xdc,xold,wakiz,wakiy,csigz(n-1),
     &                             csigy(n-1),Hb,Wb,Rb,zkdum,ykc,
     &                             szdum,sigyxa,dzrate)
                     call SIGYPR(xay+xbadj,0.0D0,sy)
                     if (sigyxa .gt. sy) then
                        vsigyc = DSQRT( sigyxa**2 - sy**2 )
                     else
                        vsigyc = 0.0D0
                     end if
                     call SIGYPR(dist(n),0.0D0,csigy(n))
                     csigy(n) = DSQRT( csigy(n)**2 + vsigyc**2 )
                  endif
               else
c ---             Wake growth for sigy
c ---             Set x at mid-point of step
                  xmid=half*(x+xold)
c ---             Compute turbulence intensities at midpoint
                  call WAKE_TURB(xmid,xLb,Rb,wakiz,wakiy)
                  if(lwak .AND. (xi.LE.x)) then
c ---                Compute sigmay
                     call WAKE_SIG(x,xd,xold,wakiz,wakiy,asigz(n-1),
     &                             asigy(n-1),Hb,Wb,Rb,zkdum,yk,
     &                             szdum,asigy(n),dzrate)
                  endif
c ---             Cavity source
                  if(lcav .AND. (xbc.LE.x)) then
                     call WAKE_SIG(x,xdc,xold,wakiz,wakiy,csigz(n-1),
     &                             csigy(n-1),Hb,Wb,Rb,zkdum,ykc,
     &                             szdum,csigy(n),dzrate)
                  endif
               endif
            endif
         endif

c --- Next distance
      enddo

c --- Construct arrays for /WAKEDAT/
c ----------------------------------

      if(lwak) then
c ---    WAK arrays:
         npw=np-nws

c ---    Place initial values into first element
         xwak(1)=xi+xbadj
         szwak(1)=szi
         sywak(1)=syi
         drwak(1)=zero
         if(npw.GE.mxntr) then
c ---       Sample a subset of the npw points
            nwak=mxntr
            xwak(nwak)=dist(np)
            szwak(nwak)=asigz(np)
            sywak(nwak)=asigy(np)
            drwak(nwak)=rtpiby2*dsz(np)
            if(npw.LE.2*mxntr) then
c ---          Fill elements with nearest values
               deln=DBLE(npw)/DBLE(nwak)
               do in=2,nwak-1
                  jn=IDINT(DBLE(in)*deln)+nws
                  xwak(in)=dist(jn)
                  szwak(in)=asigz(jn)
                  sywak(in)=asigy(jn)
                  drwak(in)=rtpiby2*dsz(jn)
               enddo
            else
c ---          Use sliding step-size to sample nearfield more frequently
               deln=2.0D0*DBLE(npw-mxntr)/DBLE(mxntr*(mxntr-1))
               rn=one
               do in=2,nwak-1
                  rn=rn+one+DBLE(in-1)*deln
                  jn=IDINT(rn)+nws
                  xwak(in)=dist(jn)
                  szwak(in)=asigz(jn)
                  sywak(in)=asigy(jn)
                  drwak(in)=rtpiby2*dsz(jn)
               enddo
            endif
         else
c ---       Fill only those elements used
            nwak=npw
            do in=2,npw
               inp=in+nws
               xwak(in)=dist(inp)
               szwak(in)=asigz(inp)
               sywak(in)=asigy(inp)
               drwak(in)=rtpiby2*dsz(inp)
            enddo
         endif
      endif

      if(lcav) then
c ---    CAV arrays:
         npc=np-ncs

c ---    Place initial values into first element
         xcav(1)=xbc+xbadj
         szcav(1)=szcav0
         sycav(1)=sycav0
         if(npc.GE.mxntr) then
c ---       Sample a subset of the npc points
            ncav=mxntr
            xcav(ncav)=dist(np)
            szcav(ncav)=csigz(np)
            sycav(ncav)=csigy(np)
            if(npc.LE.2*mxntr) then
c ---          Fill elements with nearest values
               deln=DBLE(npc)/DBLE(ncav)
               do in=2,ncav-1
                  jn=IDINT(DBLE(in)*deln)+ncs
                  xcav(in)=dist(jn)
                  szcav(in)=csigz(jn)
                  sycav(in)=csigy(jn)
               enddo
            else
c ---          Use sliding step-size to sample nearfield more frequently
               deln=2.0D0*DBLE(npc-mxntr)/DBLE(mxntr*(mxntr-1))
               rn=one
               do in=2,ncav-1
                  rn=rn+one+DBLE(in-1)*deln
                  jn=IDINT(rn)+ncs
                  xcav(in)=dist(jn)
                  szcav(in)=csigz(jn)
                  sycav(in)=csigy(jn)
               enddo
            endif
         else
c ---       Fill only those elements used
            ncav=npc
            do in=2,npc
               inp=in+ncs
               xcav(in)=dist(inp)
               szcav(in)=csigz(inp)
               sycav(in)=csigy(inp)
            enddo
         endif
      endif

      if(ldbhr) then

         write(DBGUNT,*)
         write(DBGUNT,*)'----- WAKE_DFSN:        NWAK = ',nwak
         write(DBGUNT,*)'Z-dispersion reaches ambient at: ', xaz+xbadj
         write(DBGUNT,*)'Y-dispersion reaches ambient at: ', xay+xbadj
caer         write(DBGUNT,*)'z,y virtual dist (m) - WAKE  = ',xzvwak,xyvwak
caer         write(DBGUNT,*)'z,y virtual dist (m) - CAV   = ',xzvcav,xyvcav
         write(DBGUNT,'(1x,a,2x,3(f14.5,2x))')
     &                   'xadj, yadj, xi  (m) = ', xbadj,ybadj,xi
         write(DBGUNT,'(1x,a,2x,3(f14.5,2x))')
     &                   'xbc, distc, xdc (m) = ', xbc,distc,xdc
         write(DBGUNT,*) 'lwak,  nws,  npw    = ', lwak,nws,npw
         write(DBGUNT,*) 'lcav,  ncs,  npc    = ', lcav,ncs,npc
         write(DBGUNT,*)
c
c ---    Write the arrays passed back to the calling routine
         write(DBGUNT,28)
28       format(/4x,'I',9x,'XWAK',6x,'SZWAK',6x,'SYWAK',6x,'DRWAK',/)
         do i=1,nwak
            write(DBGUNT,32)i,xwak(i),szwak(i),sywak(i),drwak(i)
32          format(i5,3x,4(f10.4,1x))
         enddo
         write(DBGUNT,*)

         write(DBGUNT,29)
29       format(/4x,'I',9x,'XCAV',6x,'SZCAV',6x,'SYCAV',/)
         do i=1,ncav
            write(DBGUNT,33)i,xcav(i),szcav(i),sycav(i)
33          format(i5,3x,3(f10.4,1x))
         enddo
         write(DBGUNT,*)
      endif

      if (AWMADWDBG) then
         write(AWMADWDBUNT,*)
         write(AWMADWDBUNT,*)'----- WAKE_DFSN:        NWAK = ',nwak
         write(AWMADWDBUNT,*)'Z-disp reaches ambient at xaz+xbadj: ',
     &                                                         xaz+xbadj
         write(AWMADWDBUNT,*)'Y-disp reaches ambient at xay+xbadj: ',
     &                                                         xay+xbadj
         write(AWMADWDBUNT,'(1x,a,2x,3(f14.5,2x))')
     &                   'xadj, yadj, xi  (m) = ', xbadj,ybadj,xi
         write(AWMADWDBUNT,'(1x,a,2x,3(f14.5,2x))')
     &                   'xbc, distc, xdc (m) = ', xbc,distc,xdc
         write(AWMADWDBUNT,*) 'lwak,  nws,  npw    = ', lwak,nws,npw
         write(AWMADWDBUNT,*) 'lcav,  ncs,  npc    = ', lcav,ncs,npc
         write(AWMADWDBUNT,*)
      end if
      return
      end

c-----------------------------------------------------------------------
      subroutine wake_dfsn2(ldbhr,xi,szi,syi,xtr,ztr,ntr,DBGUNT)
c-----------------------------------------------------------------------
c
c --- PRIME      Version:  1.0     Level:  010706             WAKE_DFSN2
c                R.W. Brode, PES, Inc.
c
c --- PURPOSE: This is a modified version of WAKE_DFSN that is called
c              after plume height array is completed in order to use
c              actual plume heights in calls to SIGYPR and SIGZPR to
c              obtain virtual source sigma-y and sigma-z terms and for
c              portions of sigma tables that include ambient dispersion.
c              NOTE: Much of this code is redundant with WAKE_DFSN, and
c              may be modified later to be more efficient.
c
c --- MODIFIED: To initialize dummy variables zkdum and ykdum to 0.0.
c               Otherwise these variables may be undefined in subrountine
c               WAKE_SIG under some circumstances.
c               R.W. Brode, MACTEC (f/k/a PES), Inc. - 09/01/05
c
c --- MODIFIED: To modify calling arguments in call to subroutine
c               WAKE_SIG from turby and turbz to wakiy and wakiz.
c               R.W. Brode, MACTEC (PES), Inc. - 07/23/04
c
c --- INPUTS:
c            ldbhr - logical     - Flag for debug write statements
c                                  to upwind bldg wall
c               xi - real        - distance (m) from upwind bldg wall
c                                  to point where plume intersects wake
c              szi - real        - sigma-z (m) at xi
c              syi - real        - sigma-y (m) at xi
c         XTR(ntr) - real        - Downwind distance (m)
c         ZTR(ntr) - real        - Plume centerline height (m)
c              ntr - integer     - Array size for XTR and ZTR
c
c     Common block /PARAMS/ variables:
c           MXNTR, MXNW
c     Common block /WAKEDAT/ variables:
c           XBADJ, Hb, Wb, xLb, Rb, xLR
c
c --- OUTPUT:
c
c     Common block /WAKEDAT/ variables:
c           NWAK, XWAK(mxntr), SZWAK(mxntr), SYWAK(mxntr),
c           DRWAK(mxntr), XZVWAK, XYVWAK,
c           NCAV, XCAV(mxntr), SZCAV(mxntr), SYCAV(mxntr),
c           VSIGY, VSIGZ, VSIGYC, VSIGZC
c
c --- WAKE_DFSN called by:  NUMRISE
c --- WAKE_DFSN calls    :  SIGZPR, SIGYPR,
c                           WAKE_XA, WAKE_CAV0, WAKE_TURB, WAKE_SIG
c----------------------------------------------------------------------
c
      USE PRIME_params
      USE PRIME_numparm
      USE PRIME_wakedat
      USE MAIN1, only : L_ORD_Cav, AWMADWDBUNT, AWMADWDBG

      IMPLICIT NONE

c --- Define local variable arrays for fine-steps
      INTEGER NTR,NP,NWS,NCS,N,IR,NPW,IN,JN,INP,NPC,DBGUNT

      DOUBLE PRECISION dist(mxnw),asigz(mxnw),asigy(mxnw),dsz(mxnw)
      DOUBLE PRECISION csigz(mxnw),csigy(mxnw)
      DOUBLE PRECISION xtr(ntr), ztr(ntr), zxay, zxaz
      DOUBLE PRECISION xi,szi,syi
C     JAT D065 8/9/21 DISTC SET BUT NEVER USED
C      DOUBLE PRECISION xamx,xamn,xay,zkdum,ykdum,xcave,distc,xbc
      DOUBLE PRECISION xamx,xamn,xay,zkdum,ykdum,xcave,xbc
      DOUBLE PRECISION xaz,xdc,szcav0,sycav0,xd,dx,xlow,xhi,xrange
      DOUBLE PRECISION dxi,x,xold,sycav0r,xmid,wakiz,wakiy,zk,yk,zkc,
     &                 ykc,dzrate,xnew,sigzxa,sydum,sz,szdum,sigyxa,sy,
     &                 deln,rn,zdist
      DOUBLE PRECISION, PARAMETER :: zero=0.0D0, half=0.5D0, one=1.0D0,
     &                               rtpiby2 = 1.25331413731550D0

      logical ldbhr,lcav,lwak,lrevcav

c --- Compute xa, where turbulent growth in the wake transitions
c --- to ambient growth rate, measured from upwind face of bldg
      call WAKE_XA(xLb,Rb,xaz,xay)
      xamx=MAX(xaz,xay)
      xamn=MIN(xaz,xay)

c --- Retrieve plume height at transition points from table
      call numgrad(xay+xbadj,xtr,ztr,ntr,zxay)
      call numgrad(xaz+xbadj,xtr,ztr,ntr,zxaz)

c --- Reinitialize virtual source sigma terms
      vsigy  = 0.0D0
      vsigz  = 0.0D0
      vsigyc = 0.0D0
      vsigzc = 0.0D0

c --- Initialize dummy variables for zk and yk
      zkdum = 0.0D0
      ykdum = 0.0D0

c --- Initialize CAVITY parameters
c --------------------------------
c --- Set distance from upwind face of bldg to END of cavity
      xcave=xLb+xLR
c --- Set distance from source to start of cavity
C     JAT D065 8/9/21 DISTC SET BUT NEVER USED
C      distc=xLb+xbadj
c --- Set downwind distance to effective cavity source (when present),
c --- from the upwind face of bldg
      xbc=MAX(xi,xLb)
      xbc=MIN(xbc,xcave)
c --- Location of downwind edge of PDF region from effective cavity
c --- source
      xdc=xbc+xLR
c --- Set initial sigmas for cavity source using sigma-y at xi
      call WAKE_CAV0(syi,szcav0,sycav0)
c --- The cavity sigma-y will need to be revised if xi lies upwind of
c --- the downwind face of the bldg.
      if(xi.LT.xLb) then
         lrevcav=.TRUE.
      else
         lrevcav=.FALSE.
      endif

c --- Determine if any plume material in cavity may be modeled
c ------------------------------------------------------------
c --- Initialize output arrays
      ncav=1
      xcav(1)=xbc+xbadj
      szcav(1)=szcav0
      sycav(1)=sycav0
      if(xi.GE.xcave) then
         lcav=.FALSE.
         lrevcav=.FALSE.
      else
         lcav=.TRUE.
      endif

c --- Is plume affected by wake turbulence?
c -------------------------------------------
c --- Initialize output arrays
      nwak=1
      xwak(1)=xi+xbadj
      szwak(1)=szi
      sywak(1)=syi
      drwak(1)=zero
      if(xi.GE.xamx) then
         lwak=.FALSE.
         if(ldbhr) then
            write(DBGUNT,*)' '
            write(DBGUNT,*)'----- WAKE_DFSN2:       NWAK = ',nwak
            write(DBGUNT,*)'Z-dispersion reaches ambient at: ',xaz+xbadj
            write(DBGUNT,*)'Y-dispersion reaches ambient at: ',xay+xbadj
caer            write(DBGUNT,*)'z,y virtual distances (m)    = ',xzvwak,xyvwak
            write(DBGUNT,*)'xadj, yadj, xi        (m)    = ',
     &                      xbadj,ybadj,xi
            write(DBGUNT,*)'Plume NOT altered by wake turbulence!'
            write(DBGUNT,*)
         endif

         if(AWMADWDBG) then
            write(AWMADWDBUNT,*)' '
            write(AWMADWDBUNT,*)'----- WAKE_DFSN2:       NWAK = ',nwak
            write(AWMADWDBUNT,*)'Z-dispersion reaches ambient at: '
     &                                                        ,xaz+xbadj
            write(AWMADWDBUNT,*)'Y-dispersion reaches ambient at: ',
     &                                                         xay+xbadj
            write(AWMADWDBUNT,*)'xadj, yadj, xi        (m)    = ',
     &                      xbadj,ybadj,xi
            write(AWMADWDBUNT,*)'Plume NOT altered by wake turbulence!'
            write(AWMADWDBUNT,*)
         endif
      else
         lwak=.TRUE.
      endif

c --- Return now if sigmas in wake do not need to be tabulated
      if(.NOT.lwak .AND. .NOT.lcav) return

c --- Compute location of downwind edge of PDF region from xi
      xd=xi+xLR

c --- Set stepping parameters
      dx=2.0D0
c --- Range of table is from point of entry into wake (xi), to the point
c --- at which ambient growth rate resumes (xa), plus one "ds" so that
c --- both sigmas reach ambient, and virtual distances are computed.
c --- When cavity sigmas are also computed, range may start at the
c --- downwind bldg face, and extend to end of cavity.
      xlow=xi
      xhi=xamx
      if(lcav) then
         xlow=MIN(xi,xbc)
         xhi=MAX(xamx,xcave)
      endif
      xrange=xhi-xlow+dx
      np=IDNINT(xrange/dx)+1
      np=MIN(np,mxnw-1)
      dx=xrange/(DBLE(np)-one)
      dxi=one/dx
      nws=0
      ncs=0

c --- Fill first element of marching arrays using values at xlow
      dist(1)=xlow+xbadj
      if(lwak) then
         asigz(1)=szi
         asigy(1)=syi
c ---    Set inital plume growth rate in wake to zero
         dsz(1)=zero
      endif
      if(lcav) then
         csigz(1)=szcav0
         csigy(1)=sycav0
      endif

c --- Initialize distance (from upwind face of bldg)
      x=xlow

      if(AWMADWDBG) then
         write(AWMADWDBUNT,*)' '
         write(AWMADWDBUNT,*)'----- WAKE_DFSN2:'
         write(AWMADWDBUNT,*)'       xaz  = distance (m) from upwind '
         write(AWMADWDBUNT,*)'              bldg wall at which wake '
         write(AWMADWDBUNT,*)'              turbulence Iz = ambient'
         write(AWMADWDBUNT,*)'       zxaz = plume height at transition'
         write(AWMADWDBUNT,*)'              points from plume ht array'

c         write (AWMADWDBUNT, 6000) n,np, xaz+xbadj,zxaz
c 6000    format(' ', / ,'  dfsn2(0): wake growth for sigz, sigy -',
c     &              ' step ',i4,' of ',i4,'; xaz+xbadj, zxaz =', 2F10.4)
      end if

c --- Loop over steps in wake region
c -----------------------------------
      do n=2,np
         xold=x
         x=x+dx
         dist(n)=dist(n-1)+dx

c ---    Check to see if cavity data should be revised based on
c ---    data from previous step
         if(lrevcav .AND. xold.GE.xLb) then
            call WAKE_CAV0(asigy(n-1),szcav0,sycav0r)
            if(sycav0r.GT.sycav0) then
               sycav0=sycav0r
               sycav(1)=sycav0
c ---          Replace sigma-y values in stepping arrays
               do ir=1,n-1
                  csigy(ir)=MAX(csigy(ir),sycav0)
               enddo
            endif
            lrevcav=.FALSE.
         endif

c ---    Obtain sigmas for this step

c ---    First, persist initial values if upwind of starting point
         if(lwak .AND. (xi.GE.x)) then
            asigz(n)=asigz(n-1)
            asigy(n)=asigy(n-1)
            dsz(n)=dsz(n-1)
c ---       Set index for skipping entry when filling wake arrays
            nws=n
         endif
         if(lcav .AND. (xbc.GE.x)) then
            csigz(n)=szcav0
            csigy(n)=sycav0
c ---       Set index for skipping entry when filling cav arrays
            ncs=n
         endif

c ---    Now test again and apply full treatment when needed
         if(xold.GT.xamx) then
c ---       Ambient growth region in wake: use virtuals
            if(lwak .AND. (xi.LT.x)) then
               vsigz = max( vsigz, szi )
               vsigy = max( vsigy, syi )
               call numgrad(dist(n),xtr,ztr,ntr,zdist)
               call SIGZPR(dist(n),zdist,asigz(n))
               call SIGYPR(dist(n),zdist,asigy(n))
               asigz(n) = DSQRT( asigz(n)**2 + vsigz**2 )
               asigy(n) = DSQRT( asigy(n)**2 + vsigy**2 )
               dsz(n)=(asigz(n)-asigz(n-1))*dxi
            endif
c ---       Cavity source ---
            if(lcav .AND. (xbc.LT.x)) then
               vsigzc = max( vsigzc, szcav0 )
               vsigyc = max( vsigyc, sycav0 )
               call SIGZPR(dist(n),0.0D0,csigz(n))
               call SIGYPR(dist(n),0.0D0,csigy(n))
               csigz(n) = DSQRT( csigz(n)**2 + vsigzc**2 )
               csigy(n) = DSQRT( csigy(n)**2 + vsigyc**2 )
            endif
         else
            if(x.LT.xamn) then
c ---          Wake growth for both sigz and sigy
c ---          Set x at mid-point of step
               xmid=half*(x+xold)
c ---          Compute turbulence intensities at midpoint
               call WAKE_TURB(xmid,xLb,Rb,wakiz,wakiy)
               if(lwak .AND. (xi.LE.x)) then
c ---             Compute sigmas in wake
                  call WAKE_SIG(x,xd,xold,wakiz,wakiy,asigz(n-1),
     &                          asigy(n-1),Hb,Wb,Rb,zk,yk,
     &                          asigz(n),asigy(n),dsz(n))
               endif

c ---          Cavity source ---
              IF( L_ORD_Cav )THEN                                 ! ORD (EMM) change
C ---             ORD Downwash Modification
                 if (lcav.AND.(xbc.LE.x).AND.(x.GE.xcave)) then        ! ORD (EMM) change
                    call WAKE_SIG(x,xdc,xold,wakiz,wakiy,csigz(n-1),   ! ORD (EMM) change
     &                             csigy(n-1),Hb,Wb,Rb,zkc,ykc,
     &                             csigz(n),csigy(n),dzrate)
                    csigz(n) = MAX( csigz(n), szcav0 )                 ! ctmp cavity bug fix
                 elseif(lcav.AND.(xbc.LE.x).AND.(x.LT.xcave)) then     ! ORD (EMM) change
                    call WAKE_SIG(x,xdc,xold,wakiz,wakiy,csigz(n-1),   ! ORD (EMM) change
     &                             csigy(n-1),Hb,Wb,Rb,zkc,ykc,
     &                             csigz(n),csigy(n),dzrate)

                    csigz(n) = 1.00D0*csigz(1)                         ! ORD (EMM) change ** added to write over above
                    csigy(n) = 1.00D0*csigy(1)                         ! ORD (EMM) change ** added to write over above
                 endif

              ELSE
C ---             Regulatory AERMOD code
                  if(lcav .AND. (xbc.LE.x)) then
                     call WAKE_SIG(x,xdc,xold,wakiz,wakiy,csigz(n-1),
     &                             csigy(n-1),Hb,Wb,Rb,zkc,ykc,
     &                             csigz(n),csigy(n),dzrate)
                     csigz(n) = MAX( csigz(n), szcav0 )         !ctmp cavity bug fix
                  endif
              END IF

            else
c ---          At least one of the sigmas reaches ambient growth in wake
c ---          Process SIGMA-Z
               if(xold.GE.xaz) then
c ---             Ambient growth region in wake: use virtual x
                  if(lwak .AND. (xi.LE.x)) then
                     call numgrad(dist(n),xtr,ztr,ntr,zdist)
                     call SIGZPR(dist(n),zdist,asigz(n))
                     vsigz = max( vsigz, szi )
                     asigz(n) = DSQRT( asigz(n)**2 + vsigz**2 )
                     dsz(n)=(asigz(n)-asigz(n-1))*dxi
                  endif
c ---             Cavity source ---
                  if(lcav .AND. (xbc.LE.x)) then
                     vsigzc = MAX( vsigzc, szcav0 )           ! ctmp cavity bug fix
                     call SIGZPR(dist(n),0.0D0,csigz(n))
                     csigz(n) = DSQRT( csigz(n)**2 + vsigzc**2 )
                  endif
               elseif(x.GE.xaz) then
c ---             Transition from wake to ambient
                  xnew=xaz
                  xmid=half*(xnew+xold)
c ---             Compute turbulence intensities at midpoint
                  call WAKE_TURB(xmid,xLb,Rb,wakiz,wakiy)
                  if(lwak .AND. (xi.LE.xnew)) then
c ---                Compute wake sigma at xaz
                     call WAKE_SIG(xnew,xd,xold,wakiz,wakiy,asigz(n-1),
     &                             asigy(n-1),Hb,Wb,Rb,zk,ykdum,
     &                             sigzxa,sydum,dzrate)
c ---                Get virtual source term as difference in quadrature between
c ---                wake and ambient sigmas at transition distance
                     call SIGZPR(xaz+xbadj,zxaz,sz)
                     if (sigzxa .gt. sz) then
                        vsigz = DSQRT( sigzxa**2 - sz**2 )
                     else
                        vsigz = 0.0D0
                     end if
c ---                Now compute sigma at dist(n) with virtual source
                     call numgrad(dist(n),xtr,ztr,ntr,zdist)
                     call SIGZPR(dist(n),zdist,asigz(n))
                     asigz(n) = DSQRT( asigz(n)**2 + vsigz**2 )
                     dsz(n)=(asigz(n)-asigz(n-1))*dxi
                  endif
c ---             Cavity source ---
                  if(lcav .AND. (xbc.LE.xnew)) then
c ---                Compute wake sigma at xaz
                     call WAKE_SIG(xnew,xdc,xold,wakiz,wakiy,csigz(n-1),
     &                             csigy(n-1),Hb,Wb,Rb,zkc,ykdum,
     &                             sigzxa,sydum,dzrate)
c ---                Get virtual source term as difference in quadrature between
c ---                wake and ambient sigmas at transition distance
                     call SIGZPR(xaz+xbadj,0.0D0,sz)
                     if (sigzxa .gt. sz) then
                        vsigzc = DSQRT( sigzxa**2 - sz**2 )
                     else
                        vsigzc = 0.0D0
                     end if
                     vsigzc = MAX( vsigzc, szcav0 )           ! ctmp cavity bug fix
c ---                Now compute sigma at dist(n) with virtual source
                     call SIGZPR(dist(n),0.0D0,csigz(n))
                     csigz(n) = DSQRT( csigz(n)**2 + vsigzc**2 )
                  endif
               else
c ---             Wake growth for sigz
c ---             Set x at mid-point of step
                  xmid=half*(x+xold)
c ---             Compute turbulence intensities at midpoint
                  call WAKE_TURB(xmid,xLb,Rb,wakiz,wakiy)
                  if(lwak .AND. (xi.LE.x)) then
c ---                Compute sigmaz
                     call WAKE_SIG(x,xd,xold,wakiz,wakiy,asigz(n-1),
     &                             asigy(n-1),Hb,Wb,Rb,zk,ykdum,
     &                             asigz(n),sydum,dsz(n))
                  endif
c ---             Cavity source ---
                  if(lcav .AND. (xbc.LE.x)) then
                     call WAKE_SIG(x,xdc,xold,wakiz,wakiy,csigz(n-1),
     &                             csigy(n-1),Hb,Wb,Rb,zkc,ykdum,
     &                             csigz(n),sydum,dzrate)
                     csigz(n) = MAX( csigz(n), szcav0 )       ! ctmp cavity bug fix
                  endif
               endif
c ---          Process SIGMA-Y
               if(xold.GE.xay) then
c ---             Ambient growth region in wake: use virtual x
                  if(lwak .AND. (xi.LE.x)) then
                     call numgrad(dist(n),xtr,ztr,ntr,zdist)
                     call SIGYPR(dist(n),zdist,asigy(n))
                     vsigy = max( vsigy, syi )
                     asigy(n) = DSQRT( asigy(n)**2 + vsigy**2 )
                  endif

c ---             Cavity source ---
                  IF( L_ORD_Cav )THEN                              ! ORD (EMM) change
C ---                Modifications for AWMADWNW
                     if(lcav.AND.(x.GE.xcave).AND.(xbc.LE.x)) then      ! ORD (EMM) change
                        call SIGYPR(dist(n),0.0D0,csigy(n))
                        vsigyc = max( vsigyc, sycav0 )
                        csigy(n) = DSQRT( csigy(n)**2 + vsigyc**2 )
                     elseif(lcav.AND.(x.LT.xcave).AND.(xbc.LE.x)) then  ! ORD (EMM) change
                        call SIGYPR(dist(n),0.0D0,csigy(n))             ! ORD (EMM) change
                        vsigyc = max( vsigyc, sycav0 )                  ! ORD (EMM) change
C                        csigy(n) = DSQRT( csigy(n)**2 + vsigyc**2 )     ! ORD (EMM) change
                        csigy(n) = 1.00D0*csigy(1)                      ! ORD (EMM) change
                    endif

                  ELSE
C ---                Regulatory AERMOD code
                     if(lcav .AND. (xbc.LE.x)) then
                        call SIGYPR(dist(n),0.0D0,csigy(n))
                        vsigyc = max( vsigyc, sycav0 )
                        csigy(n) = DSQRT( csigy(n)**2 + vsigyc**2 )
                     endif
                  END IF

               elseif(x.GE.xay) then
c ---             Transition from wake to ambient
                  xnew=xay
                  xmid=half*(xnew+xold)
c ---             Compute turbulence intensities at midpoint
                  call WAKE_TURB(xmid,xLb,Rb,wakiz,wakiy)
                  if(lwak .AND. (xi.LE.xnew)) then
c ---                Compute sigma at xay
CRWB                 Modify call to WAKE_SIG to include wakiz and wakiy
CRWB                 instead of turbz and turby.
CRWB                     call WAKE_SIG(xnew,xd,xold,turbz,turby,asigz(n-1),
                     call WAKE_SIG(xnew,xd,xold,wakiz,wakiy,asigz(n-1),
     &                             asigy(n-1),Hb,Wb,Rb,zkdum,yk,
     &                             szdum,sigyxa,dzrate)
c ---                Get virtual source term as difference in quadrature between
c ---                wake and ambient sigmas at transition distance
                     call SIGYPR(xay+xbadj,zxay,sy)
                     if (sigyxa .gt. sy) then
                        vsigy = DSQRT( sigyxa**2 - sy**2 )
                     else
                        vsigy = 0.0D0
                     end if
c ---                Now compute sigma at dist(n) with virtual source
                     call numgrad(dist(n),xtr,ztr,ntr,zdist)
                     call SIGYPR(dist(n),zdist,asigy(n))
                     asigy(n) = DSQRT( asigy(n)**2 + vsigy**2 )
                  endif
c ---             Cavity source ---
                  if(lcav .AND. (xbc.LE.xnew)) then
                     call WAKE_SIG(xnew,xdc,xold,wakiz,wakiy,csigz(n-1),
     &                             csigy(n-1),Hb,Wb,Rb,zkdum,ykc,
     &                             szdum,sigyxa,dzrate)
                     call SIGYPR(xay+xbadj,0.0D0,sy)
                     if (sigyxa .gt. sy) then
                        vsigyc = DSQRT( sigyxa**2 - sy**2 )
                     else
                        vsigyc = 0.0D0
                     end if
                     call SIGYPR(dist(n),0.0D0,csigy(n))
                     csigy(n) = DSQRT( csigy(n)**2 + vsigyc**2 )
                  endif
               else
c ---             Wake growth for sigy
c ---             Set x at mid-point of step
                  xmid=half*(x+xold)
c ---             Compute turbulence intensities at midpoint
                  call WAKE_TURB(xmid,xLb,Rb,wakiz,wakiy)
                  if(lwak .AND. (xi.LE.x)) then
c ---                Compute sigmay
                     call WAKE_SIG(x,xd,xold,wakiz,wakiy,asigz(n-1),
     &                             asigy(n-1),Hb,Wb,Rb,zkdum,yk,
     &                             szdum,asigy(n),dzrate)
                  endif
c ---             Cavity source
                  if(lcav .AND. (xbc.LE.x)) then
                     call WAKE_SIG(x,xdc,xold,wakiz,wakiy,csigz(n-1),
     &                             csigy(n-1),Hb,Wb,Rb,zkdum,ykc,
     &                             szdum,csigy(n),dzrate)
                  endif
               endif
            endif
         endif

c --- Next distance
      enddo

c --- Construct arrays for /WAKEDAT/
c ----------------------------------

      if(lwak) then
c ---    WAK arrays:
         npw=np-nws

c ---    Place initial values into first element
         xwak(1)=xi+xbadj
         szwak(1)=szi
         sywak(1)=syi
         drwak(1)=zero
         if(npw.GE.mxntr) then
c ---       Sample a subset of the npw points
            nwak=mxntr
            xwak(nwak)=dist(np)
            szwak(nwak)=asigz(np)
            sywak(nwak)=asigy(np)
            drwak(nwak)=rtpiby2*dsz(np)
            if(npw.LE.2*mxntr) then
c ---          Fill elements with nearest values
               deln=DBLE(npw)/DBLE(nwak)
               do in=2,nwak-1
                  jn=IDINT(DBLE(in)*deln)+nws
                  xwak(in)=dist(jn)
                  szwak(in)=asigz(jn)
                  sywak(in)=asigy(jn)
                  drwak(in)=rtpiby2*dsz(jn)
               enddo
            else
c ---          Use sliding step-size to sample nearfield more frequently
               deln=2.0D0*DBLE(npw-mxntr)/DBLE(mxntr*(mxntr-1))
               rn=one
               do in=2,nwak-1
                  rn=rn+one+DBLE(in-1)*deln
                  jn=IDINT(rn)+nws
                  xwak(in)=dist(jn)
                  szwak(in)=asigz(jn)
                  sywak(in)=asigy(jn)
                  drwak(in)=rtpiby2*dsz(jn)
               enddo
            endif
         else
c ---       Fill only those elements used
            nwak=npw
            do in=2,npw
               inp=in+nws
               xwak(in)=dist(inp)
               szwak(in)=asigz(inp)
               sywak(in)=asigy(inp)
               drwak(in)=rtpiby2*dsz(inp)
            enddo
         endif
      endif

      if(lcav) then
c ---    CAV arrays:
         npc=np-ncs

c ---    Place initial values into first element
         xcav(1)=xbc+xbadj
         szcav(1)=szcav0
         sycav(1)=sycav0
         if(npc.GE.mxntr) then
c ---       Sample a subset of the npc points
            ncav=mxntr
            xcav(ncav)=dist(np)
            szcav(ncav)=csigz(np)
            sycav(ncav)=csigy(np)
            if(npc.LE.2*mxntr) then
c ---          Fill elements with nearest values
               deln=DBLE(npc)/DBLE(ncav)
               do in=2,ncav-1
                  jn=IDINT(DBLE(in)*deln)+ncs
                  xcav(in)=dist(jn)
                  szcav(in)=csigz(jn)
                  sycav(in)=csigy(jn)
               enddo
            else
c ---          Use sliding step-size to sample nearfield more frequently
               deln=2.0D0*DBLE(npc-mxntr)/DBLE(mxntr*(mxntr-1))
               rn=one
               do in=2,ncav-1
                  rn=rn+one+DBLE(in-1)*deln
                  jn=IDINT(rn)+ncs
                  xcav(in)=dist(jn)
                  szcav(in)=csigz(jn)
                  sycav(in)=csigy(jn)
               enddo
            endif
         else
c ---       Fill only those elements used
            ncav=npc
            do in=2,npc
               inp=in+ncs
               xcav(in)=dist(inp)
               szcav(in)=csigz(inp)
               sycav(in)=csigy(inp)
            enddo
         endif
      endif

      if(ldbhr) then

         write(DBGUNT,*)
         write(DBGUNT,*)'----- WAKE_DFSN2:'
         write(DBGUNT,*)'PRIMARY SOURCE:'
         write(DBGUNT,*)' Lateral virtual source sigma,  VSIGY (m)  = ',
     &                   vsigy
         write(DBGUNT,*)' Vertical virtual source sigma, VSIGZ (m)  = ',
     &                   vsigz
         write(DBGUNT,*)'CAVITY SOURCE:'
         write(DBGUNT,*)' Lateral virtual source sigma,  VSIGYC (m) = ',
     &                   vsigyc
         write(DBGUNT,*)' Vertical virtual source sigma, VSIGZC (m) = ',
     &                   vsigzc
         write(DBGUNT,*)

      endif

      if (AWMADWDBG) then
         write(AWMADWDBUNT,*)
         write(AWMADWDBUNT,*)'----- WAKE_DFSN2:'
         write(AWMADWDBUNT,*)'PRIMARY SOURCE:'
         write(AWMADWDBUNT,*)' Lateral virtual source sigma,  ',
     &                       'VSIGY (m)  = ', vsigy
         write(AWMADWDBUNT,*)' Vertical virtual source sigma, ',
     &                       'VSIGZ (m)  = ', vsigz
         write(AWMADWDBUNT,*)'CAVITY SOURCE:'
         write(AWMADWDBUNT,*)' Lateral virtual source sigma,  ',
     &                       'VSIGYC (m) = ', vsigyc
         write(AWMADWDBUNT,*)' Vertical virtual source sigma, ',
     &                       'VSIGZC (m) = ', vsigzc
         write(AWMADWDBUNT,*)
      end if

      return
      end

c-----------------------------------------------------------------------
      subroutine wake_turb(xinp,L,Rinp,tiz,tiy)
c-----------------------------------------------------------------------
c
c --- PRIME      Version:  1.0     Level:  970812             WAKE_TURB
c                L. Schulman, D. Strimaitis,   EARTH TECH
c                Prepared for EPRI under contract WO3527-01
c
c --- PURPOSE: Calculates turbulence intensity as a function of
c              location within the wake from modified Weil (1996)
c              analytical expressions
c
c --- MODIFIED: For use with the AERMOD model.  Added MAIN1 module to
c               access global AERMOD data.  Modified ambiy and ambiz to
c               use AERMOD turbulence intensities.
c               R.W. Brode, PES, Inc. - 07/05/01
c
c               Modified for AWMADWNW 5/25/18,
c               for enhanced calculation of tiz, tiy using sub wake_u_turb.
c
c --- INPUTS:
c              kst - integer     - PG stability class (1-6)
c            lrurl - logical     - Rural flag (T=Rural, F=Urban)
c             xinp - real        - distance (m) from upwind bldg wall
c                L - real        - dist (m) of downwind bldg wall from
c                                  upwind bldg wall
c                R - real        - wake scaling length (m)
c
c     Common block /DFSN/ variables:
c           wiz0,wiy0,wfz,wfy,
c           dua_ua,xdecay,xdecayi,
c           rurliz,rurliy,urbniz,urbniy
c
c --- OUTPUT:
c
c              tiz - real        - turbulence intensity sigw/u
c              tiy - real        - turbulence intensity sigv/u
c
c --- WAKE_TURB called by:  WAKE_DFSN
c --- WAKE_TURB calls    :  none
c----------------------------------------------------------------------
c
      USE MAIN1
      USE PRIME_dfsn
      USE PRM2_WAKEDAT

      IMPLICIT NONE

      DOUBLE PRECISION xinp, L, Rinp, tiz, tiy
      DOUBLE PRECISION ambiz, ambiy, fariz, fariy, xmL, xfac, xfrac
      DOUBLE PRECISION :: ZHI, ZLO
      INTEGER          :: NDXBHI, NDXBLO, NDXALO

      DOUBLE PRECISION, PARAMETER :: one = 1.0D0, zero = 0.0D0
      DOUBLE PRECISION zdum ! forces wake_u_turb to use z from wake_u
      DOUBLE PRECISION dum  ! for consistent wake_u_turb argument list

c --- Specify ambient turbulence intensities from AERMOD effective parameters
      ambiz = sweff/ueff
      ambiy = sveff/ueff

c --- Compute asymptotic turbulence intensity in far wake
      fariz=MIN(wiz0,ambiz)
      fariy=MIN(wiy0,ambiy)

C --- AWMA version D20350
C     If AWMAUTurbHX option set and WAKE_DFSN2 is called in WAKE_TURB,
C       recompute effective parameters, turbulence intensities 

      IF (L_AWMA_UTurbHX .and. DFSN2CALL) THEN

c ---    Determine the plume rise at the current position, xinp
         IF (xinp .LT. xtr_sav(p2mxntr)) THEN
c ---          Interpolate in rise table to get gradual rise   ---   CALL NUMGRAD
            call NUMGRAD(xinp,xtr_sav,ztr_sav,p2mxntr,Zeff_prm2)
         ELSE
            Zeff_prm2 = ztr_sav(p2mxntr)
      END IF

         ZHI = Zeff_PRM2 + 5.0D0
         ZLO = Zeff_PRM2 - 5.0D0

C ---    Recompute average values between ZLO and ZHI
         CALL LOCATE(GRIDHT, 1, MXGLVL, ZHI, NDXBHI)
         CALL LOCATE(GRIDHT, 1, MXGLVL, ZLO, NDXBLO)

         NDXALO = NDXBLO + 1
         CALL ANYAVG ( MXGLVL, GRIDHT, GRIDWS, ZLO,NDXALO,
     &      ZHI,NDXBHI,UEFF )

         CALL ANYAVG ( MXGLVL, GRIDHT, GRIDSV, ZLO,NDXALO,
     &      ZHI,NDXBHI,SVEFF )

         CALL ANYAVG ( MXGLVL, GRIDHT, GRIDSW, ZLO,NDXALO,
     &      ZHI,NDXBHI,SWEFF )

         CALL ANYAVG ( MXGLVL, GRIDHT, GRIDTG, ZLO,NDXALO,
     &      ZHI, NDXBHI, TGEFF )

c ---    Specify ambient turbulence intensities from AWMA effective parameters
         ambiz = sweff/ueff
         ambiy = sveff/ueff

c --- Compute asymptotic turbulence intensity in far wake
C         fariz and fariy are not used in further calculaitons
c         fariz=MIN(wiz0,ambiz)
c         fariy=MIN(wiy0,ambiy)

      END IF

C --- AWMA version D20350
      IF (L_AWMA_UTurb .OR. L_AWMA_UTurbHX) THEN
C ---    Modifications for AWMADWNW
C        ZINP is passed from WAKE_DFSN (xmid,...)
         zdum = -1.d0 ! ensure that prior z from wake_u is used
         call wake_u_turb ('turb', xinp, zdum, ambiz, ambiy, dum,
     &                     tiz,tiy)
      ELSE
C ---    Original AERMOD code
c ---     Compute turbulence intensity at position downwind of bldg
         xmL=MAX(zero,xinp-L)
         xfac=one/(((xmL+Rinp)/Rinp)**xdecay-dua_ua)
         tiz=fariz*(one+((wfz*wiz0/fariz-one)+dua_ua)*xfac)
         tiy=fariy*(one+((wfy*wiy0/fariy-one)+dua_ua)*xfac)
      END IF

c --- Interpolate turbulence intensity if over roof of bldg
      if (xinp.LT.L) then
         xfrac=xinp/L
         tiz=ambiz+xfrac*(tiz-ambiz)
         tiy=ambiy+xfrac*(tiy-ambiy)
      endif

C --- Reset the effective parameters to the original values computed in
C       PRMCALC
C --- AWMA version D20350
       IF (L_AWMA_UTurbHX) THEN
          Ueff  = Ueff_save
          SWeff = SWeff_save
          SVeff = SVeff_save
       END IF

      return
      end

c-----------------------------------------------------------------------
      subroutine wake_u(ldb,x,y,z,ubyua,dufac,DBGUNT)
c-----------------------------------------------------------------------
c
c --- PRIME      Version:  1.0     Level:  990726 (99207)           WAKE_U
c                D. Strimaitis,   EARTH TECH
c                Prepared for EPRI under contract WO3527-01
c
c --- PURPOSE: Calculates speed ratio u(wake)/u(ambient) as a function
c              of location within the wake
c
c              Modified by B. de Foy, 26th July 1999,
c              To set fmin as a minimum value for ubyua
c
c              Modified for AWMA PRIME2 Subcommitee Downwash 5/25/2018,
c              Enhanced calculation of du_ua using subroutine wake_u_turb
c
c --- INPUTS:
c              ldb - logical     - flag for debug output
c                x - real        - downwind distance (m) from upwind
c                                  bldg wall
c                y - real        - crosswind distance (m) from center of
c                                  upwind bldg wall
c                z - real        - height (m) above ground
c
c     Common block /PARAMS/ variables:
c           MXNTR, MXNW
c     Common block /WAKEDAT/ variables:
c           Hb, Wb, xLb, Rb, xLR
c     Common block /DFSN/ variables:
c           dua_ua,xdecay,xdecayi
c
c --- OUTPUT:
c
c            ubyua - real        - U(x,z)/Ua speed in wake scaled by
c                                  ambient speed
c            dufac - real        - Gradient in speed factor above
c                                  Zcav
c
c --- WAKE_U called by:  NUMRISE, WAKE_DBG
c --- WAKE_U calls    :  CAVITY_HT, WAKE_DIM
c----------------------------------------------------------------------
c
      USE MAIN1, only : L_AWMA_UTurb, L_AWMA_UTurbHX
      USE PRIME_params
      USE PRIME_dfsn
      USE PRIME_wakedat

      IMPLICIT NONE
      INTEGER DBGUNT
      DOUBLE PRECISION x,y,z,zcav,ycav,hwake,wwake,yabs,
     &                 ubyua,dufac,ymin,du_ua,ydiff,xml,xfrac,ucbyua,
     &                 zz

      DOUBLE PRECISION, PARAMETER :: zero=0.0D0, one=1.0D0, two=2.0D0,
     &                               fmin=0.01D0

      DOUBLE PRECISION dum    ! used for for consistency in wake_u_turb arg list
      logical ldb

c --- Compute cavity height above ground, and width
      call CAVITY_HT(Hb,Wb,xLb,Rb,xLC,xLR,HR,x,zcav,ycav)

c --- Compute far wake height above ground, and width
      call WAKE_DIM(x,Hb,Wb,Rb,hwake,wwake)

c --- Return "null" values if point is outside wake
      yabs  = DABS(y)
      ubyua = one
      dufac = zero
      if(z.GE.hwake .OR. yabs.GE.wwake) return

C --- AWMA version D20350
      IF (L_AWMA_UTurb .OR. L_AWMA_UTurbHX) THEN
C ---    Modifications for AWMADWNW
c ---    Get new value of velocity deficit
         call wake_u_turb ('vel',x, z, dum, dum, du_ua, dum, dum)

c ---    Adjust "base" speed deficit dua_ua if lateral position is
c ---    beyond bldg width projection, but within the wake
         ymin  = MAX(0.5D0*Wb,wwake-Rb/3.0D0)
         ydiff = wwake-ymin
         if(yabs.GT.ymin .AND. ydiff.GT.zero) then
            du_ua = du_ua * (one-(yabs-ymin)/ydiff)
         endif

      ELSE
C ---    Regulatory AERMOD
c ---    Adjust "base" speed deficit dua_ua if lateral position is
c ---    beyond bldg width projection, but within the wake
         ymin  = MAX(0.5D0*Wb,wwake-Rb/3.0D0)
         du_ua = dua_ua
         ydiff = wwake-ymin
         if(yabs.GT.ymin .AND. ydiff.GT.zero) then
            du_ua = dua_ua*(one-(yabs-ymin)/ydiff)
         endif

c ---    Scale speed deficit (Ua-U)/Ua =  du_ua in wake for
c ---    position x downwind of bldg face
         xmL   = MAX(zero,x-xLb)
         du_ua = du_ua*((xmL+Rb)/Rb)**(-xdecay)

      END IF

c --- Interpolate factor if over roof of bldg (linear)
      if(x.LT.xLb) then
         xfrac=x/xLb
         du_ua=xfrac*du_ua
      endif

c --- Compute speed factor Ucav/Ua at top of cavity
c --- Assume that speed is constant below ZCAV, and increases linearly
c --- with height to ambient speed at HWAKE
      ucbyua=MAX(zero,(one-two*hwake*du_ua/(hwake+zcav)))

c --- Compute gradient in speed factor (zero below Zcav)
      dufac=zero
      if(z.GT.zcav) then
         dufac=(one-ucbyua)/(hwake-zcav)
      endif

c --- Compute speed factor U/Ua at height z
      zz=MIN(z,hwake)
c --- Ensure fmin as lower bound for ubyua
      ubyua=MAX(fmin,(ucbyua+dufac*(zz-zcav)))

      if(ldb) then
        write(DBGUNT,*)'WAKE_U         '
        write(DBGUNT,*)'       x,y,z = ',x,y,z
        write(DBGUNT,*)'hwake, zcav  = ',hwake, zcav
        write(DBGUNT,*)'wwake, ymin  = ',wwake, ymin
        write(DBGUNT,*)'du_ua, ucbyua= ',du_ua, ucbyua
        write(DBGUNT,*)'ubyua, dufac = ',ubyua,dufac
        write(DBGUNT,*)
      endif

      return
      end

c-----------------------------------------------------------------------
      SUBROUTINE wake_u_turb
     &   (scope, xinp, znew, ambiz, ambiy, du_ua, tiz, tiy)
                                       !-----  ---  ---
c-----------------------------------------------------------------------
c --- New subroutine for AWMADWNW 5/25/2018
c
c --- PURPOSE: Calculate all new equations for enhanced wake flow as
c              needed by subroutines wake_u, wake_turb.
c
c --- INPUTS:
c        scope
c        xinp
c        znew   -  When called from wake_u, will be > 0 and used for z.
c                    When called from wake_turb, will be < 0 and previous
c                    value of z will be used.
c        ambiz -  Only needed when scope = 'turb'. Otherwise, a dummy
c        ambiy -     "
c        du_ua_new - [similar to znew]
c        zeff_PRM2 - Effective height (from module prm2_wakedat)
c
c --- OUTPUT:
c        du_ua -  Needed when called from wake_u. Otherwise for internal use
c        tiz   -  Needed when called from wake_turb. Otherwise ignored
c        tiy   -     "
c
c --- CALLED BY:
c        wake_u
c        wake_turb
c
c --- CALLS:      None
c-----------------------------------------------------------------------
      USE MAIN1, ONLY: L_STRMLN_BLDG, L_RECT_BLDG, L_AWMA_UTurbHX,
     &                 AWMADWDBUNT, AWMADWDBG
      USE prime_wakedat, ONLY: HB, WB, HR, xLb, mxntr
      USE PRM2_WAKEDAT, ONLY:  DFSN2CALL, xtr_sav, ztr_sav

      use prm2_wakedat

      IMPLICIT NONE

c    ! args
      CHARACTER(*), INTENT (IN)        :: scope
      DOUBLE PRECISION, INTENT (IN)    :: xinp,znew, ambiz, ambiy
      DOUBLE PRECISION, INTENT (OUT)   :: du_ua
      DOUBLE PRECISION, INTENT (OUT)   :: tiz, tiy
c    ! constants
      DOUBLE PRECISION ::
     &      VUFAC,               SVFAC,               SWFAC,
     &      Au,                  Asv,                 Asw,
     &      nu,                  nsv,                 nsw,
     &      Bu,                  Bsv,                 Bsw,
     &      ru,                  rsv,                 rsw,
     &      LrUDfac,             LrSVfac,             LrSWfac,
     &      HWminU,              HWminSV,             HWminSW,
     &      HLminU,              HLminSV,             HLminSW,
     &      HWmaxU,              HWmaxSV,             HWmaxSW,
     &      HLmaxU,              HLmaxSV,             HLmaxSW

C Unused:      double precision iyo, izo

c     Additional "constants" needed for Bsv, Bsw, rsv, rsw

      DOUBLE PRECISION ::
     &   iy0,  iz0,
     &   ABsv, BBsv, CBsv,    ABsw, BBsw, CBsw,
     &   Arsv, Brsv, Crsv,    Arsw, Brsw, Crsw
C Unused:       DOUBLE PRECISION :: BsvMax, BsvMin, BswMax, BswMin, rsvMax, rsvMin, rswMax, rswMin

c     local vars
      DOUBLE PRECISION :: ZUDMAX_HB, UDMAX !, dummy, dummyy, dummyyy
      DOUBLE PRECISION :: A1, A2           ! Parts of larger expressions
      DOUBLE PRECISION :: HWlimU, HWlimSW, HWlimSV
      DOUBLE PRECISION :: HLlimU, HLlimSW, HLlimSV
      DOUBLE PRECISION :: xLRUD, xLRSW, xLRSV
      DOUBLE PRECISION :: XiUD_HB          !! 1-17-2017
      DOUBLE PRECISION :: xml, eps
      DOUBLE PRECISION :: XiSW_HB, XiSV_HB
      DOUBLE PRECISION :: SW, SWmaxx, Zsw_HB, Zsv_HB
      DOUBLE PRECISION :: SV, SVmaxx
      DOUBLE PRECISION :: z, zvel_sav, zplm
C Unused:      DOUBLE PRECISION :: PorFacSW = 1.d0, PorFacUD = 1.d0

      SAVE z, zvel_sav


c---- Set constants according to the building type (rect. vs. streamlined)

      IF (L_RECT_BLDG ) THEN                         ! Rectangular bldgs
         VUFAC =  0.294d0;    SVFAC =  0.320d0;    SWFAC =  0.290d0
         Au =     0.500d0;    Asv =    0.700d0;    Asw =    0.650d0
         nu =     0.000d0;    nsv =    0.356d0;    nsw =    0.336d0
         Bu =     3.494d0   ! Bsv             ;    Bsw
         ru =     1.667d0   ! rsv             ;    rsw
         LrUDfac= 1.135d0;    LrSVfac= 1.349d0;    LrSWfac= 1.794d0
         HWminU = 0.449d0;    HWminSV= 0.200d0;    HWminSW= 0.375d0
         HLminU = 0.753d0;    HLminSV= 0.321d0;    HLminSW= 0.308d0
         HWmaxU = 3.00d0;     HWmaxSV= 3.00d0;     HWmaxSW= 3.00d0
         HLmaxU = 3.00d0;     HLmaxSV= 3.00d0;     HLmaxSW= 3.00d0
      ELSE IF (L_STRMLN_BLDG) THEN                   ! Streamlined bldgs
         VUFAC =  0.294d0;    SVFAC =  0.225d0;    SWFAC =  0.225d0
         Au =     0.400d0;    Asv =    0.550d0;    Asw =    0.450d0
         nu =     0.631d0;    nsv =    0.477d0;    nsw =    0.570d0
         Bu =     2.113d0   ! Bsv             ;    Bsw
         ru =     1.796d0   ! rsv             ;    rsw
         LrUDfac= 0.563d0;    LrSVfac= 0.971d0;    LrSWfac= 0.640d0
         HWminU = 0.449d0;    HWminSV= 0.375d0;    HWminSW= 0.375d0
         HLminU = 0.753d0;    HLminSV= 0.308d0;    HLminSW= 0.308d0
         HWmaxU = 3.00d0;     HWmaxSV= 3.00d0;     HWmaxSW= 3.00d0
         HLmaxU = 3.00d0;     HLmaxSV= 3.00d0;     HLmaxSW= 3.00d0
      END IF
c---- Get additional "constants" Bsv, Vsw, rsv, rsw

      IF (L_RECT_BLDG ) THEN                         ! Rectangular bldgs
         ABsv = 0.018571d0;   BBsv = -1.10574d0;   CBsv = 18.30949d0
         ABsw = 0.045142d0;   BBsw = -2.1585d0;    CBsw = 29.33697d0
         Arsv = -0.00124d0;   Brsv = 0.033975d0;   Crsv = 1.431187d0
         Arsw = 0.000155d0;   Brsw = -0.00626d0;   Crsw = 1.562781d0
         !!!BsvMax = 9.036d0;    BsvMin = 1.856d0
         !!!BswMax = 16.593d0;   BswMin = 4.806d0
         !!!rsvMax = 1.648d0;    rsvMin = 1.369d0
         !!!rswMax = 1.527d0;    rswMin = 1.500d0
         iy0 = SV30 / U30 * 100.d0
         IF (iy0 > 29.2d0) iy0 = 29.2d0
         IF (iy0 < 5.0d0) iy0 = 5.0d0
         iz0 = SW30 / U30 * 100.d0
         IF (iz0 > 18.6d0) iz0 = 18.6d0
         IF (iz0 < 2.0d0) iz0 = 2.0d0
         Bsv = ABsv * iy0**2 + BBsv * iy0 + CBsv
         Bsw = ABsw * iz0**2 + BBsw * iz0 + CBsw
         rsv = Arsv * iy0**2 + Brsv * iy0 + Crsv
         rsw = Arsw * iz0**2 + Brsw * iz0 + Crsw

      ELSE IF (L_STRMLN_BLDG) THEN                   ! Streamlined bldgs
         ABsv = 0.008223d0;   BBsv = -0.42265d0;   CBsv = 9.330299d0
         ABsw = 0.092864d0;   BBsw = -3.68931d0;   CBsw = 39.21321d0
         Arsv = 0.000861d0;   Brsv = 0.004194d0;   Crsv = 1.385942d0
         Arsw = 0.004627d0;   Brsw = -0.15270d0;   Crsw = 2.939305d0
         !!!BsvMax = 5.900d0;    BsvMin = 4.000d0
         !!!BswMax = 18.180d0;   BswMin = 2.720d0
         !!!rsvMax = 2.243d0;    rsvMin = 1.516d0
         !!!rswMax = 2.110d0;    rswMin = 1.700d0
         iy0 = SV30 / U30 * 100.d0
         iz0 = SW30 / U30 * 100.d0
         Bsv = ABsv * iy0**2 + BBsv * iy0 + CBsv
         Bsw = ABsw * iz0**2 + BBsw * iz0 + CBsw
         rsv = Arsv * iy0**2 + Brsv * iy0 + Crsv
         rsw = Arsw * iz0**2 + Brsw * iz0 + Crsw
      END IF

      !!!IF (Bsv > BsvMax) Bsv = BsvMax
      !!!IF (Bsv < BsvMin) Bsv = BsvMin
      !!!IF (Bsw > BswMax) Bsw = BswMax
      !!!IF (Bsw < BswMin) Bsw = BswMin
      !!!IF (rsv > rsvMax) rsv = rsvMax
      !!!IF (rsv < rsvMin) rsv = rsvMin
      !!!IF (rsw > rswMax) rsw = rswMax
      !!!IF (rsw < rswMin) rsw = rswMin

c-----Get the value of z.
c       If scope = 'vel', znew is the value to use
c       If scope = 'turb', then use previous value or zeff_PRM2.

c     If < 0, then use previous value.
      if (scope == 'vel') then
         z = znew
         zvel_sav = znew
      elseif (scope == 'turb') then
         z = MIN(zvel_sav,zeff_PRM2)

C ---    AWMA version D20350
         if (L_AWMA_UTurbHX .and. DFSN2CALL) THEN
C ---       Conditional with call to WAKE_DFSN2
C           This is a repeat of what is found in wake_turb to get 'z'; 
C             with further thought the value of 'z' computed there could
C             be passed to wake_u_turb and used here such that the 
C             following code could be removed
            IF (xinp .LT. xtr_sav(p2mxntr)) THEN
C              xinp is the input argument, which is xinp in CALL WAKE_U_TURB
c ---          Interpolate in rise table to get gradual rise ---   CALL NUMGRAD
               call NUMGRAD(xinp,xtr_sav,ztr_sav,p2mxntr,Zplm)
               z = zplm
            ELSE
               Zplm = ztr_sav(p2mxntr)
               z = zplm
            END IF
         end if
      else
C ---    Coding error - should not get here in an AERMOD mode run
         print *, ' Improper u/turb scope in wake_u_turb'
         print *, '  - must be "vel" or "turb" (lower case)'
         stop
      endif

c---- Compute du_ua as needed by wake_u and wake_turb

      ! Eq (11)
      xml = MAX (0.d0, xinp-xLb)
      eps = xml + HB

      ! Eq (8a)
      HWlimU = HB / WB
      if (HWlimU < HWminU) HWlimU = HWminU
      if (HWlimU > HWmaxU) HWlimU = HWmaxU
      ! Eq (8b)
      HLlimU = HB / xLB
      if (HLlimU < HLminU) HLlimU = HLminU
      if (HLlimU > HLmaxU) HLlimU = HLmaxU

      ! Eq (7a) (dependent on 8a,8b)
      A1 = 1.d0 / HWlimU
      A2 = (1.d0 / HLlimU**.3d0) * (1.d0 + .24d0 / HWlimU)
      xLRUD = 1.8d0 * A1/A2

      ! Eq (10a) (dependent on 7a)
      XiUD_HB = MAX ((LRUDfac*xLRUD+1), eps/HB)

      ! Eq (5) (dependent on 8a, 8b, 10) (rev 1/17/2017)
      A1 = XiUD_HB * HWlimU**(2.d0/3.d0)
      A2 = HLlimU**(-nu)
      UDmax = Bu * (A1*A2)**(-ru)

      ! Eq (4a)
      ZUDmax_HB = AU * (HR/HB)**1.5

      ! Eq (2) (dependent on 5,4a,z)
      !if (zc/HB >= ZUDMAX_HB) then
      if (z/HB >= ZUDMAX_HB) then
      !if (zeff_PRM2/HB >= ZUDMAX_HB) then
         !A1 = (ZUDmax_HB - zc/HB)**2
         !A1 = (ZUDmax_HB - z/HB)**2
         A1 = (ZUDmax_HB - z/HB)**2
         A2 = (VUFAC**2) * XiUD_HB ! New eq 1-17-2017
         du_ua = UDmax * exp(-A1/A2)
      else
         du_ua = UDmax
      endif
      if (scope == 'vel') then
         if (AWMADWDBG) write (AWMADWDBUNT, '(3A, 8(A, f9.3))')
     &                  '    scope=', scope, ':',
     &                  '  x=',xinp,
     &                  '  xml=', xml,
     &                  '  znew(inarg)=', znew,
     &                  '  z=', z,
     &                  '  eps/HB=', eps/HB,
     &                  '  XiUD_HB=', XiuD_HB,
     &                  '  UDmax=', UDmax,
     &                  '  du_ua=', du_ua
      end if

c---- Can now bail if call was for wake_u.
c      Otherwise, continue to compute tiz, tiy as needed by wake_turb.

      if (scope == 'vel') RETURN

      ! Eq (9a1) 
      HWlimSW = HB / WB
      if (HWlimSW < HWminSW) HWlimSW = HWminSW
      if (HWlimSW > HWmaxSW) HWlimSW = HWmaxSW
      ! Eq (9a2)
      HLlimSW = HB / xLB
      if (HLlimSW < HLminSW) HLlimSW = HLminSW
      if (HLlimSW > HLmaxSW) HLlimSW = HLmaxSW

      ! Eq (9b1) 
      HWlimSV = HB / WB
      if (HWlimSV < HWminSV) HWlimSV = HWminSV
      if (HWlimSV > HWmaxSV) HWlimSV = HWmaxSV
      ! Eq (9b2)
      HLlimSV = HB / xLB
      if (HLlimSV < HLminSW) HLlimSV = HLminSV
      if (HLlimSV > HLmaxSW) HLlimSV = HLmaxSV

      ! Eq (7b) (dependent on 9)
      A1 = 1.d0 / HWlimSW
      A2 = (1.d0 / HLlimSW**.3d0) * (1.d0 + .24d0 / HWlimSW)
      xLRSW = 1.8d0 * A1/A2
      ! Eq (7c) (dependent on 9b)
      A1 = 1.d0 / HWlimSV
      A2 = (1.d0 / HLlimSV**.3d0) * (1.d0 + .24d0 / HWlimSV)
      xLRSV = 1.8d0 * A1/A2

      ! Eq (10b) (dependent on 7b)
      XiSW_HB = MAX ((LrSWfac*xLRSW+1), eps/HB)
      ! Eq (10c) (dependent on 7c) (added 6/12/2017)
      XiSV_HB = MAX ((LrSVfac*xLRSV+1), eps/HB)

      ! Eq (6a) (dependent on 7,9) (Rev 1/7/2017)
      A1 = XiSW_HB * HWlimSW**(2.d0/3.d0)
      A2 = HLlimSW**(-nsw)
      SWmaxx = Bsw * (A1*A2)**(-rsw)
      ! Eq (6b) (dependent on 7,9) (added 6/12/2017)
      A1 = XiSV_HB * HWlimSV**(2.d0/3.d0)
      A2 = HLlimSV**(-nsv)
      SVmaxx = Bsv * (A1*A2)**(-rsv)

      ! Eq (4b)
      Zsw_HB = Asw * (HR/HB)**1.5d0
      ! Eq (4b) (added 6/12/2017)
      Zsv_HB = Asv * (HR/HB)**1.5d0

      ! Eq (3) (dependent on 4b)
      ! Eq (3a) 
      !A1 = (Zsw_HB - zc/HB)**2
      !A1 = (Zsw_HB - z/HB)**2
      A1 = (Zsw_HB - z/HB)**2
      A2 = (SWFAC**2) * XiSW_HB !! New equation 1-17-2017
      !IF (zc/HB >= Zsw_HB) THEN
      !IF (z/HB >= Zsw_HB) THEN
      IF (z/HB >= Zsw_HB) THEN
         SW = SWmaxx * exp(-A1/A2)
      ELSE
         SW = SWmaxx
      END IF
      ! Eq (3b) (dependent on 4c) (added 6/12/2017)
      A1 = (Zsv_HB - z/HB)**2
      A2 = (SVFAC**2) * XiSV_HB !! New equation 1-17-2017
      IF (z/HB >= Zsv_HB) THEN
         SV = SVmaxx * exp(-A1/A2)
      ELSE
         SV = SVmaxx
      END IF

      ! Constraint on UD per RLP7/6/2017
C     D116 Update to AWMA Turbulence Enhancement Limit, 11/2/21
C     New constraints per RPetersen
      du_ua = MIN (du_ua, 0.9d0)
      ! Eq (1a)
      A1 = (1.d0 + SW) / (1.d0 - du_ua)
C      D116 Comment original constraint (50), update value to 18
C      tiz = ambiz * MIN (50.0d0, A1)
      tiz = ambiz * MIN ( 18.0d0, A1)
      tiz = MIN (0.9d0, tiz)
      ! Eq (1b)
      A1 = (1.d0 + SV) / (1.d0 - du_ua)
C      D116 Comment original constraint (50), update value to 6
C      tiz = ambiz * MIN (50.0d0, A1)
      tiy = ambiy * MIN ( 6.0d0, A1)
      tiy = MIN (0.9d0, tiy)

      IF( AWMADWDBG ) write (AWMADWDBUNT, '(3A, 19(A, f9.3))')
     &   '  scope=', scope, ':',
     &   '  x=', xinp,
     &   '  znew(inarg)=' ,znew,
     &   '  z=', z,
     &   '  eps/HB=', eps/HB,
     &   '  XiUD/HB=', XiuD_HB,
     &   '  XiSW/HB=', XiSW_HB,
     &   '  SW=', SW,
     &   '  SWmaxx=', SWmaxx,
     &   '  SV=', SV,
     &   '  SVmaxx=', SVmaxx,
     &   '  SW30=', SW30,
     &   '  SV30=', SV30,
     &   '  U30=', U30,
     &   '  tiz=', tiz,
     &   '  tiy=', tiy,
     &   '  Bsv=', Bsv,
     &   '  Bsw=', Bsw,
     &   '  rsv=', rsv,
     &   '  rsw=', rsw

      END SUBROUTINE wake_u_turb

c-----------------------------------------------------------------------
      subroutine wake_xa(L,Rinp,xaz,xay)
c-----------------------------------------------------------------------
c
c --- PRIME      Version:  1.0     Level:  980310               WAKE_XA
c                D. Strimaitis,   EARTH TECH
c                Prepared for EPRI under contract WO3527-01
c
c --- PURPOSE: Calculates the distance from the upwind face of the
c              building to the point at which the turbulence intensity
c              in the wake approaches that in the ambient flow.
c
c              Final distances are limited to "xbyrmax" scale-lengths
c              (R) set in prime1, measured from downwind bldg face
c
c --- MODIFIED: For use with the AERMOD model.  Added MAIN1 module to
c               access global AERMOD data.  Modified ambiy and ambiz to
c               use AERMOD turbulence intensities.
c               R.W. Brode, PES, Inc. - 07/05/01
c
c --- INPUTS:
c              kst - integer     - PG stability class (1-6)
c            lrurl - logical     - Rural flag (T=Rural, F=Urban)
c                L - real        - dist (m) of downwind bldg wall from
c                                  upwind bldg wall
c                R - real        - wake scaling length (m)
c
c     Common block /DFSN/ variables:
c           afac,xbyrmax,wiz0,wiy0,wfz,wfy,
c           dua_ua,xdecay,xdecayi,
c           rurliz,rurliy,urbniz,urbniy
c
c --- OUTPUT:
c
c              xaz - real        - distance (m) from upwind bldg wall
c                                  at which wake turbulence Iz = ambient
c              xay - real        - distance (m) from upwind bldg wall
c                                  at which wake turbulence Iy = ambient
c
c --- WAKE_XA called by:  WAKE_DFSN
c --- WAKE_XA calls    :  none
c----------------------------------------------------------------------
c

      USE main1

      USE PRIME_dfsn

      IMPLICIT NONE

      DOUBLE PRECISION L, Rinp, xaz, xay
      DOUBLE PRECISION ambiz, ambiy, fariz, fariy, farizt, fariyt,
     &                 x0byr, xbyr
      DOUBLE PRECISION, PARAMETER :: one=1.0D0

c --- Specify ambient turbulence intensities from AERMOD effective parameters
      ambiz = sweff/ueff
      ambiy = sveff/ueff

c --- Compute asymptotic turbulence intensity in far wake
      fariz=MIN(wiz0,ambiz)
      fariy=MIN(wiy0,ambiy)

c --- Define the turbulence intensity at the transition point
      farizt=MAX(ambiz,afac*fariz)
      fariyt=MAX(ambiy,afac*fariy)

c --- Compute leading term
      x0byr=L/Rinp-one

c --- Compute scaled distance at which Iz equals transition Iz
      xaz=x0byr+(dua_ua+(wfz*wiz0-fariz*(one-dua_ua))/
     &       (farizt-fariz))**xdecayi

c --- Compute distance at which Iy equals transition Iy
      xay=x0byr+(dua_ua+(wfy*wiy0-fariy*(one-dua_ua))/
     &       (fariyt-fariy))**xdecayi

c --- Cap distances
      xbyr=L/Rinp+xbyrmax
      xaz=Rinp*MIN(xbyr,xaz)
      xay=Rinp*MIN(xbyr,xay)

      if (PRIMEDBG) then
         write(PRMDBUNT,*) ' '
         write(PRMDBUNT,*) 'WAKE_XA Calculations:'
         write(PRMDBUNT,*) 'ambiz,  ambiy  = ', ambiz, ambiy
         write(PRMDBUNT,*) 'farizt, fariyt = ', farizt, fariyt
         write(PRMDBUNT,*) 'xaz,    xay    = ', xaz, xay
      end if

      return
      end

      subroutine wake_xa2(L,Rinp,xaz,xay)
c-----------------------------------------------------------------------
c
c --- PRIME      Version:  1.0     Level:  980310               WAKE_XA2
c                D. Strimaitis,   EARTH TECH
c                Prepared for EPRI under contract WO3527-01
c                Modified version of wake_xa for use with AERMOD-PRIME
c                R. Brode, PES, Inc. - 8/9/01
c
c --- PURPOSE: Calculates the distance from the upwind face of the
c              building to the point at which the turbulence intensity
c              in the wake approaches that in the ambient flow.
c
c              Final distances are NOT limited to "xbyrmax" scale-lengths
c              (R) set in prime1.
c
c --- INPUTS:
c                L - real        - dist (m) of downwind bldg wall from
c                                  upwind bldg wall
c                Rinp - real     - wake scaling length (m)
c
c     Common block /DFSN/ variables:
c           afac,xbyrmax,wiz0,wiy0,wfz,wfy,
c           dua_ua,xdecay,xdecayi,
c           rurliz,rurliy,urbniz,urbniy
c
c --- OUTPUT:
c
c              xaz - real        - distance (m) from upwind bldg wall
c                                  at which wake turbulence Iz = ambient
c              xay - real        - distance (m) from upwind bldg wall
c                                  at which wake turbulence Iy = ambient
c
c --- WAKE_XA called by:  GAMCALC
c --- WAKE_XA calls    :  none
c----------------------------------------------------------------------
c

      use main1

      USE PRIME_dfsn

      IMPLICIT NONE

      DOUBLE PRECISION L, Rinp, xaz, xay
      DOUBLE PRECISION ambiz, ambiy, fariz, fariy, farizt, fariyt,
     &                 x0byr
      DOUBLE PRECISION, PARAMETER :: one=1.0D0

c --- Specify ambient turbulence intensities from AERMOD effective parameters
      ambiz = sweff/ueff
      ambiy = sveff/ueff

c --- Compute asymptotic turbulence intensity in far wake
      fariz=MIN(wiz0,ambiz)
      fariy=MIN(wiy0,ambiy)

c --- Define the turbulence intensity at the transition point
      farizt=MAX(ambiz,afac*fariz)
      fariyt=MAX(ambiy,afac*fariy)

c --- Compute leading term
      x0byr=L/Rinp-one

c --- Compute scaled distance at which Iz equals transition Iz
      xaz=x0byr+(dua_ua+(wfz*wiz0-fariz*(one-dua_ua))/
     &       (farizt-fariz))**xdecayi

c --- Compute distance at which Iy equals transition Iy
      xay=x0byr+(dua_ua+(wfy*wiy0-fariy*(one-dua_ua))/
     &       (fariyt-fariy))**xdecayi

c --- Apply scaling factor (without capping at 15R)
      xaz=Rinp*xaz
      xay=Rinp*xay

      return
      end

c-----------------------------------------------------------------------
      subroutine wake_dim(x,H,W,R,hwake,wwake)
c-----------------------------------------------------------------------
c
c --- PRIME      Version:  1.0     Level:  970812              WAKE_DIM
c                D. Strimaitis,   EARTH TECH
c                Prepared for EPRI under contract WO3527-01
c
c --- PURPOSE: Calculates the vertical height and lateral half-width
c              of a building wake at a distance x from the upwind
c              face of the bldg
c
c --- INPUTS:
c                x - real        - dist (m) from upwind bldg face
c                H - real        - building height (m)
c                W - real        - building width (m)
c                R - real        - wake scaling length (m)
c
c --- OUTPUT:
c
c            hwake - real        - wake height (m)
c            wwake - real        - wake half-width (m)
c
c --- WAKE_DIM called by:  POSITION, WAKE_SIG
c --- WAKE_DIM calls    :  none
c----------------------------------------------------------------------
c --- Wake height from combination of Wilson (1979) and Weil (1996)
c --- limits for uniform approach wind

      IMPLICIT NONE

      DOUBLE PRECISION x,H,W,R,hwake,wwake
      DOUBLE PRECISION xbyr,xpbyr,dxbyr,xbyr3rd
      DOUBLE PRECISION third
      DOUBLE PRECISION, PARAMETER :: cwkht = 1.2D0, half=0.5D0,
     &                               zero=0.0D0

c --- Misc. local constants
      third = 1.0D0/3.0D0

c --- Scale distance by R
      xbyr=x/R
      xbyr3rd=xbyr**third

c --- Compute match to bdlg height at x=0
      xpbyr=-(H/(cwkht*R))**3
      dxbyr=xbyr-xpbyr

c --- Wake height
      hwake=zero
      if(xbyr.GT.zero) hwake =cwkht*R*dxbyr**third

c --- Wake half-width from empirical fit to Snyder wind tunnel data
      wwake=zero
      if(xbyr.GT.zero) wwake=half*W+(third*R)*xbyR3rd

      return
      end

c-----------------------------------------------------------------------
      subroutine wake_sig(x,xd,xold,turbz,turby,szold,syold,
     &                    H,W,R,zk,yk,sz,sy,dsz)
c-----------------------------------------------------------------------
c
c --- PRIME      Version:  1.0     Level:  970812              WAKE_SIG
c                D. Strimaitis,   EARTH TECH
c                Prepared for EPRI under contract WO3527-01
c
c --- PURPOSE: Calculates sigmas and d(sigma)/dx within the wake
c              at a distance x from the upwind face of the bldg,
c              prior to the ambient growth regime, for a "small"
c              distance increment
c
c --- INPUTS:
c                x - real        - dist (m) from upwind bldg face
c               xd - real        - dist (m) at which PDF growth ends
c             xold - real        - starting x of this step (m)
c     turbz, turby - real        - current turbulence intensities
c     szold, syold - real        - sigmas (m) at start of step
c   htwake, hwwake - real        - height and half-width (m) of wake
c           zk, yk - real        - matching constants for PDF transition
c
c --- OUTPUT:
c
c           zk, yk - real        - matching constants for PDF transition
c           sz, sy - real        - sigmas (m) at end of step
c              dsz - real        - d(sigmaz)/dx over step
c
c --- WAKE_SIG called by:  WAKE_DFSN
c --- WAKE_SIG calls    :  WAKE_DIM
c----------------------------------------------------------------------
c --- Wake height from combination of Wilson (1979) and Weil (1996)
c --- limits for uniform approach wind

      IMPLICIT NONE

      DOUBLE PRECISION x,xd,xold,turbz,turby,szold,syold,
     &                 H,W,R,zk,yk,sz,sy,dsz
      DOUBLE PRECISION htwake,hwwake,fwwake,delx,xstepi,dsz2,dsy2,
     &                 sigzd,sigyd
      DOUBLE PRECISION, PARAMETER :: two = 2.0D0

c --- Get wake dimensions
      call WAKE_DIM(x,H,W,R,htwake,hwwake)

c --- Use full width of the wake to scale lateral diffusivity
      fwwake=two*hwwake

      delx=x-xold
      xstepi=1.0D0/delx
      if(x.LT.xd) then
c ---    Pure PDF Form
         dsz=turbz
         sz=szold + delx*turbz
         sy=syold + delx*turby
      elseif(xold.GT.xd) then
c ---    Pure Wake Diffusivity Form
         dsz2=zk*turbz*htwake
         dsy2=yk*turby*fwwake
         sz=DSQRT(szold**2+delx*dsz2)
         sy=DSQRT(syold**2+delx*dsy2)
         dsz=(sz-szold)*xstepi
      else
c ---    Transition from PDF to Diffusivity Form
c ---    To end of PDF:
         delx=xd-xold
         sigzd=szold + delx*turbz
         sigyd=syold + delx*turby
         zk=two*sigzd/htwake
         yk=two*sigyd/fwwake
c ---    Beyond end of PDF:
         delx=x-xd
         dsz2=zk*turbz*htwake
         dsy2=yk*turby*fwwake
         sz=DSQRT(sigzd**2+delx*dsz2)
         sy=DSQRT(sigyd**2+delx*dsy2)
         dsz=(sz-szold)*xstepi
      endif

      return
      end

c-----------------------------------------------------------------------
      subroutine wake_dbg(io,ntr,xtr,ytr,ztr,rtr,nobid,hstack)
c-----------------------------------------------------------------------
c
c --- PRIME      Version:  1.0     Level:  970812              WAKE_DBG
c                D. Strimaitis,   EARTH TECH
c                Prepared for EPRI under contract WO3527-01
c
c --- PURPOSE: Reports salient features of PRIME results to
c              file for DEBUG purposes
c
c --- MODIFIED: For use with the AERMOD model.  Added hstack to calling
c               arguments for WAKE_XSIG.
c               R.W. Brode, PES, Inc. - 07/05/01
c
c --- INPUTS:
c               io - integer     - unit for output file
c         XTR(ntr) - real        - Downwind distance (m)
c         YTR(ntr) - real        - Crosswind distance (m)
c         ZTR(ntr) - real        - Plume centerline height (m)
c         RTR(ntr) - real        - Plume radius (m)
c            NOBID - logical     - flag for BID
c           HSTACK - real        - height (m) of release
c
c     Common block /PARAMS/ variables:
c           MXNTR, MXNW
c     Common block /WAKEDAT/ variables:
c           XBADJ, Hb, Wb, xLb, Rb, xLR, xLC, HR,
c           XCAV, SZCAV, SYCAV
c
c --- OUTPUT: (written to file)
c
c          DBXB - real    - Distance (m) from upwind bldg face
c           DBX - real    - Distance (m) from source along wind
c           DBZ - real    - Plume centerline height above ground (m)
c          DBHC - real    - Cavity height above ground (m)
c          DBHW - real    - Wake height above ground (m)
c          DBSZ - real    - Sigma-z (m)
c          DBSY - real    - Sigma-y (m)
c          DBUW - real    - Wind speed factor at DBZ  (u/Ua)
c         DBRSZ - real    - Sigma-y (m) inferred from plume radius
c       IPOSITN - integer - 1: in bldg
c                           2: in cavity
c                           3: in far wake
c                           4: outside bldg influence
c         DBSZC - real    - Sigma-z (m) for cavity source
c         DBSYC - real    - Sigma-y (m) for cavity source
c
c --- WAKE_DBG called by:  PHEFF
c --- WAKE_DBG calls    :  WAKE_XSIG, WAKE_DIM, CAVITY_HT,
c                          POSITION, WAKE_U
c----------------------------------------------------------------------
c
      USE PRIME_wakedat

      IMPLICIT NONE

      INTEGER IO, NTR, IPOSITN, IT

      DOUBLE PRECISION XTR(ntr),YTR(ntr),ZTR(ntr),RTR(ntr),HSTACK
      DOUBLE PRECISION DBX,DBY,DBZ,DBSZ,DBSY,DBHW,DBHC,DBRSZ,DBUW,
     &                 RISE,XB,YB,ZB,DBXB,DBSZC,DBSYC,DBDRDX,DBWW,
     &                 DBWC,DBDUW,XZERO

      DOUBLE PRECISION, PARAMETER :: rt2bypi = 0.797884560802866D0

      logical nobid,ldb

      ldb=.FALSE.

c --- Write section header to file
      write(io,*)
      write(io,*)'------------------------------------------------'
      write(io,*)'PRIME Module Results for Current Source and Hour'
      write(io,*)'          (all lengths in meters)'
      write(io,*)'------------------------------------------------'
      write(io,*)
      write(io,100)
      write(io,*)

c --- Report start of cavity as first point if it lies upwind of source
      if(xcav(1).LT.0.0D0) then
c ---    Set plume coordinates
         dbx=xcav(1)
         dby=ytr(1)
         dbz=0.0D0

c ---    Set initial values
         dbsz=0.0D0
         dbsy=0.0D0
         dbhw=0.0D0
         dbhc=0.0D0
         dbrsz=0.0D0
         dbuw=1.0D0
         ipositn=4

c ---    Compute related data
         rise=0.0D0
         xb=dbx-xbadj
         yb=dby-ybadj
         zb=dbz
         dbxb=xb

c ---    Set sigmas
         dbsz=0.0D0
         dbsy=0.0D0
         dbszc=szcav(1)
         dbsyc=sycav(1)

c ---    Set dr/dx of plume radius within wake region
         dbdrdx=0.0D0

         if(xb.GE.0.0D0) then
c ---       Set wake dimension along center plane from bldg
            call WAKE_DIM(xb,Hb,Wb,Rb,dbhw,dbww)

c ---       Set cavity dimension along centerplane from bldg
            call CAVITY_HT(Hb,Wb,xLb,Rb,xLC,xLR,HR,xb,dbhc,dbwc)

c ---       Set speed factor
            call POSITION(xb,yb,zb,ipositn)
            dbuw=1.0D0
            if(ipositn.LT.4)then
               call WAKE_U(ldb,xb,yb,zb,dbuw,dbduw,io)
            else
               dbduw = 0.0D0
            endif
         endif

c ---    Report values
         write(io,101) dbxb,dbx,dbz,dbhw,dbhc,dbsz,dbsy,dbuw,dbduw,
     &                 dbrsz,dbdrdx,ipositn,dbszc,dbsyc
      endif

c --- Process point of release
c --- Set plume coordinates
      dbx=0.0D0
      dby=ytr(1)
      dbz=hstack

c --- Set initial values
      dbsz=0.0D0
      dbsy=0.0D0
      dbhw=0.0D0
      dbhc=0.0D0
      dbrsz=0.0D0
      dbuw=1.0D0
      ipositn=4

c --- Compute related data
      rise=dbz-hstack
      xb=dbx-xbadj
      yb=dby-ybadj
      zb=dbz
      dbxb=xb

c --- Set sigmas just downwind of source
      xzero=0.001D0
      call WAKE_XSIG(xzero,hstack,rise,nobid,dbsz,dbsy,dbszc,dbsyc)

c --- Set dr/dx of plume radius within wake region
      call WAKE_DRDX(dbx,dbdrdx)

      if(xb.GE.0.0D0) then
c ---    Set wake dimension along center plane from bldg
         call WAKE_DIM(xb,Hb,Wb,Rb,dbhw,dbww)

c ---    Set cavity dimension along centerplane from bldg
         call CAVITY_HT(Hb,Wb,xLb,Rb,xLC,xLR,HR,xb,dbhc,dbwc)

c ---    Set speed factor
         call POSITION(xb,yb,zb,ipositn)
         dbuw=1.0D0
         if(ipositn.LT.4)then
            call WAKE_U(ldb,xb,yb,zb,dbuw,dbduw,io)
         else
            dbduw = 0.0D0
         endif
      else
         dbduw  = 0.0D0
      endif

c --- Report values
      write(io,101) dbxb,dbx,dbz,dbhw,dbhc,dbsz,dbsy,dbuw,dbduw,dbrsz,
     &              dbdrdx,ipositn,dbszc,dbsyc

c --- Now loop over entries in plume rise array
      do it=1,ntr

c ---    Set plume coordinates
         dbx=xtr(it)
         dby=ytr(it)
         dbz=ztr(it)
         dbrsz=rtr(it)*rt2bypi

c ---    Set initial values
         dbhw=0.0D0
         dbhc=0.0D0
         dbuw=1.0D0
         ipositn=4

c ---    Compute related data
         rise=dbz-hstack
         xb=dbx-xbadj
         yb=dby-ybadj
         zb=dbz
         dbxb=xb

c ---    Set sigmas
         call WAKE_XSIG(dbx,hstack,rise,nobid,dbsz,dbsy,dbszc,dbsyc)

c ---    Set dr/dx of plume radius within wake region
         call WAKE_DRDX(dbx,dbdrdx)

         if(xb.GE.0.0D0) then
c ---       Set wake dimension along center plane from bldg
            call WAKE_DIM(xb,Hb,Wb,Rb,dbhw,dbww)

c ---       Set cavity dimension along centerplane from bldg
            call CAVITY_HT(Hb,Wb,xLb,Rb,xLC,xLR,HR,xb,dbhc,dbwc)

c ---       Set speed factor
            call POSITION(xb,yb,zb,ipositn)
            dbuw=1.0D0
            if(ipositn.LT.4)then
               call WAKE_U(ldb,xb,yb,zb,dbuw,dbduw,io)
            else
               dbduw = 0.0D0
            endif
         else
            dbduw  = 0.0D0
         endif

c ---    Report values
         write(io,101) dbxb,dbx,dbz,dbhw,dbhc,dbsz,dbsy,dbuw,dbduw,
     &                 dbrsz,dbdrdx,ipositn,dbszc,dbsyc

      enddo
      write(io,*)

100   format('     XB      X      Z   Hwake   Hcav    Sz     S',
     &       'y    Ufac  dUfac  R->Sz  dRdx  Pos  Szcav  Sycav')
101   format(1x,7f7.1,2f7.3,f7.1,f7.3,i4,2f7.1)

      return
      end

c-----------------------------------------------------------------------
      subroutine wake_cav0(sycapt,szcav0,sycav0)
c-----------------------------------------------------------------------
c
c --- PRIME      Version:  1.0     Level:  970812              WAKE_CAV0
c                D. Strimaitis, L. Schulman,   EARTH TECH
c                Prepared for EPRI under contract WO3527-01
c
c --- PURPOSE:  Compute the sigmas for a source placed on the floor
c               of the cavity, which produce the target cavity
c               concentration
c
c --- INPUTS:
c
c        SYCAPT - real    - Sigma-y (m) of plume at point where
c                           mass is captured in cavity
c
c     Common block /WAKEDAT/ variables:
c           Hb, Wb, xLR, xLC, HR, Ub, Urh
c
c --- OUTPUT:
c
c        SZCAV0 - real    - Initial sigma-z (m) of cavity source
c        SYCAV0 - real    - Initial sigma-y (m) of cavity source
c
c                 Note    - These sigmas reproduce the cavity
c                           concentration when they are used in:
c                           C = Qc / (pi * us * szcav0 * sycav0)
c                           where us is the wind speed for the primary
c                           source, and Qc is the mass flux captured by
c                           and released from the cavity.
c
c --- WAKE_CAV0 called by:  WAKE_DFSN
c --- WAKE_CAV0 calls    :  none
c----------------------------------------------------------------------
c
      USE PRIME_wakedat

      IMPLICIT NONE

      DOUBLE PRECISION sycapt,szcav0,sycav0

      DOUBLE PRECISION wcapt, wscale, hcav, uratio

      DOUBLE PRECISION, PARAMETER :: rt2pi   = 2.50662827463100D0,
     &                               rt2bypi = 0.797884560802866D0


c --- Interpret plume sigma-y at cavity entry point as equivalent
c --- top-hat;  limit to full width of bldg
      wcapt=MIN(rt2pi*sycapt,Wb)

c --- Set width scale for lateral distribution in cavity
      wscale=MIN(Wb,3.0D0*Hb)
      wscale=MAX(wscale,third*Hb)
      wscale=MAX(wscale,wcapt)

c --- Sigma-y for equivalent top-hat distribution
      sycav0=wscale/rt2pi

c --- Set height of cavity behind the bldg
      if(xLC .LT. xLb) then
c ---    Reattachment
         hcav=Hb
      else
c ---    No Reattachment
         hcav=HR
      endif

c --- Set sigma-z that results in centerline concentration equal to
c --- cavity concentration
c --- Wilson & Britter 1982 approach to cavity concentration
      uratio=Ub/Urh
      szcav0=rt2bypi*uratio*hcav*third

      return
      end

c-----------------------------------------------------------------------
      subroutine cav_src(xr,yr,zr,fqcav0,qc,hc,yrc,zrc,szc,syc,n1,n2)
c-----------------------------------------------------------------------
c
c --- PRIME      Version:  1.0     Level:  970812               CAV_SRC
c                D. Strimaitis,   EARTH TECH
c                Prepared for EPRI under contract WO3527-01
c
c --- PURPOSE:  Select plume data for computing concentration at a
c               receptor due to mass contained in / released from cavity
c
c --- MODIFIED:
c               Replaced undefined variable lbd with .TRUE. in calls to
c               WAKE_XSIG to ignore BID for "outside" cavity source.
c               R.W. Brode, MACTEC (f/k/a PES), Inc. - 08/02/05
c
c               For use with the AERMOD model.  Added hc to calling
c               arguments for WAKE_XSIG.  Also modified case where only
c               primary source and "outside" cavity source contribute
c               to keep "outside" cavity source in slot 3 by setting
c               emission rate for "inside" cavity source to 0.0.
c               R.W. Brode, PES, Inc. - 07/05/01
c
c --- INPUTS:
c            XR - real    - Downwind distance (m) from stack to receptor
c            YR - real    - Crosswind distance (m) from stack to receptor
c            ZR - real    - Receptor height (m) above ground
c
c     Common block /WAKEDAT/ variables:
c           Hb, Wb, xLb, Rb, xLC, xLR, HR,
c           XBADJ, YBADJ, SZCAV, SYCAV, FQCAV
c
c --- OUTPUT:
c
c        FQCAV0 - real    - Fraction of plume mass rate captured
c                           and released by cavity
c         QC(3) - real    - Normalized emission rate (q/s) for cavity
c                           sources --- QC(1)+QC(2)=1.0
c         HC(3) - real    - Height (m) of cavity sources
c        YRC(3) - real    - Crosswind distance (m) from centerline
c                           of cavity sources to receptor
c        ZRC(3) - real    - Receptor height (m) above cavity
c        SZC(3) - real    - Sigma-z (m) for cavity sources
c        SYC(3) - real    - Sigma-y (m) for cavity sources
c         N1,N2 - integer - Index range for active sources
c                           1,1: Primary source ONLY (no cavity source
c                                contributions)
c                           1,2: Primary and "outside" cavity source
c                                contribution
c                           1,3: Primary and both "outside" and "inside"
c                                cavity source contributions
c                           2,2: "outside" cavity source ONLY
c                           2,3: Both "outside" and "inside" cavity
c                                sources
c                           3,3: "inside" cavity source ONLY
c
c ------------------------------------
c     NOTE:  3 sources are considered:
c                           (1)- the primary (point) source
c                           (2)- the cavity source that dominates
c                                "outside" of the cavity
c                           (3)- the cavity source that dominates
c                                "inside" of the cavity
c            For the 2 cavity sources, array data elements are ordered:
c                           (1)- RESERVED for primary source data
c                           (2)- "outside" cavity source
c                           (3)- "inside" cavity source
c
c --- CAV_SRC called by:  PSIMPL(HOST subroutine)
c --- CAV_SRC calls    :  POSITION, CAVITY_HT, WAKE_XSIG
c----------------------------------------------------------------------
c
      USE PRIME_wakedat

      IMPLICIT NONE

      INTEGER I, N1, N2, MODE, IPOSITN

      DOUBLE PRECISION xr,yr,zr,fqcav0,szcav0,sycav0,xrb,yrb,zrb,
     &                 x115b,x85b,wtop,ybmax,ysb,dumz,dumy,zcav,
     &                 wcav

      DOUBLE PRECISION qc(3),hc(3),yrc(3),zrc(3),szc(3),syc(3)

      DOUBLE PRECISION, PARAMETER :: rt2pi   = 2.50662827463100D0,
     &                               rt2bypi = 0.797884560802866D0

c --- Extract cavity sigmas from the first entry in the cavity arrays
      szcav0=szcav(1)
      sycav0=sycav(1)

c --- Pass mass fraction to calling program
      fqcav0=fqcav

c --- Set cavity source heights to zero
      hc(2)=0.0D0
      hc(3)=0.0D0

c --- Initialize cavity source mode
c --- (0: none, 1: "outside", 2: "inside", 3: both)
      mode=0

      if(fqcav.LE.0.0D0) then
c ---    No mass in cavity
         n1=1
         n2=1
         do i=2,3
            qc(i)=0.0D0
            yrc(i)=yr
            zrc(i)=zr
            szc(i)=szcav0
            syc(i)=sycav0
         enddo
      else
c ---    Find receptor location relative to center of upwind bldg face
         xrb=xr-xbadj
         yrb=yr-ybadj
         zrb=zr
         call POSITION(xrb,yrb,zrb,ipositn)

c ---    Set limits of transition zone at end of cavity
         x115b=xLb+1.15D0*xLR
         x85b=xLb+0.85D0*xLR
c ---    Adjust relative contribution of cavity sources near end
c ---    of cavity region
         if(xrb.GE.x115b) then
c ---       Receptor well outside cavity; use only "outside" source
            qc(2)=1.0D0
            qc(3)=0.0D0
            mode=1
         elseif(xrb.GT.x85b) then
c ---       Mix relative contribution so that they are equal at
c ---       end of cavity
            qc(2)=(xrb-x85b)/(x115b-x85b)
            qc(3)=1.0D0-qc(2)
            mode=3
         elseif(xrb.GT.xLb) then
c ---       Receptor well within cavity; use only "inside" source
            qc(2)=0.0D0
            qc(3)=1.0D0
            mode=2
         else
c ---       Receptor upwind of trailing edge of projected bldg;
c ---       use "inside" source, but drop mass fraction linearly
c ---       to zero at windward face of projected bldg
            qc(2)=0.0D0
            qc(3)=MAX(0.0D0,xrb/xLb)
            mode=2
         endif

         if(ipositn.EQ.4) then
c ---       Not within wake, so drop cavity source contribution
            mode=0
            n1=1
            n2=1
            do i=2,3
               qc(i)=0.0D0
               yrc(i)=yr
               zrc(i)=zr
               szc(i)=szcav0
               syc(i)=sycav0
            enddo
         else
c ---       Set receptor offset from centerline of cavity plume
c ---       Top-hat equivalent width of cavity sigma-y
            wtop=sycav0*rt2pi
c ---       Max distance from bldg center to centerline of cavity plume
            ybmax=0.5D0*(Wb-wtop)
            if(ybmax.LE.0.0D0) then
c ---          Plume spread exceeds bldg width so cavity source is
c ---          centered on bldg
               yrc(2)=yrb
            else
c ---          Source location relative to center of bldg
               ysb=-ybadj
               if(ysb.LT.0.0D0) then
                  yrc(2)=yrb-MAX(ysb,-ybmax)
               else
                  yrc(2)=yrb-MIN(ysb,ybmax)
               endif
            endif
            yrc(3)=yrc(2)

            if(ipositn.LE.2) then
c ---          Within cavity/bldg, so drop primary source contribution,
c ---          and place receptor on ground
               if(mode.EQ.3) then
                  n1=2
                  n2=3
               elseif(mode.EQ.2) then
                  n1=3
                  n2=3
               elseif(mode.EQ.1) then
                  n1=2
                  n2=2
               endif
               do i=n1,n2
                  zrc(i)=0.0D0
                  szc(i)=szcav0
                  syc(i)=sycav0
               enddo
               if((mode.EQ.1 .OR. mode.EQ.3) .AND.
     &               xr.GT.0.0D0) call WAKE_XSIG(xr,hc(2),0.0D0,.TRUE.,
     &                                         dumz,dumy,szc(2),syc(2))
            else
c ---          Contributions from primary & possibly both cavity plumes
               n1=1
               n2=3
c ---          Set pole height to height above cavity boundary
               if(xrb.GE.(xLb+xLR)) then
                  zrc(2)=zr
               else
                  call CAVITY_HT(Hb,Wb,xLb,Rb,xLC,xLR,HR,xrb,zcav,wcav)
                  zrc(2)=MAX(0.0D0,zr-zcav)
               endif
               zrc(3)=zrc(2)
               if(mode.EQ.2) then
c ---             No contribution from "outside" cavity source, so
c ---             set emission rate for "outside" source to zero.
                  qc(2)=0.0D0
                  szc(2)=szcav0
                  syc(2)=sycav0
                  szc(3)=szcav0
                  syc(3)=sycav0
                  n2=3
               elseif(mode.EQ.1) then
c ---             No contribution from "inside" cavity source, so
c ---             reset n2=2
                  call WAKE_XSIG(xr,hc(2),0.0D0,.TRUE.,dumz,dumy,szc(2),
     &                           syc(2))
                  n2=2
               else
c ---             Both cavity sources are used
                  szc(2)=szcav0
                  syc(2)=sycav0
                  szc(3)=szcav0
                  syc(3)=sycav0
                  if(xr.GE.0.0D0) call WAKE_XSIG(xr,hc(2),0.0D0,.TRUE.,
     &                                        dumz,dumy,szc(2),syc(2))
               endif
            endif
         endif
      endif

c --- Final check: receptor upwind of primary source, or all mass in cav
c --- Do not allow n1=1 (primary source contribution)
      if(n1.EQ.1 .AND. (xr.LE.0.0D0 .OR. fqcav.EQ.1.0D0)) n1=2

      return
      end

c-----------------------------------------------------------------------
      subroutine wake_fqc(ldb,xbi,xtr,ztr,ntr,DBGUNT)
c-----------------------------------------------------------------------
c
c --- PRIME      Version:  1.0     Level:  970812              WAKE_FQC
c                D. Strimaitis, L. Schulman,   EARTH TECH
c                Prepared for EPRI under contract WO3527-01
c
c --- PURPOSE: Computes the maximum plume mass captured by cavity.
c ---          Plume centerline enters wake boundary before clearing
c ---          downwind end of cavity, so WAKE_FQC is used to find
c ---          point where mass in cavity is greatest.  Note that
c ---          distances are measured from center of upwind face of bldg
c
c --- MODIFIED: For use with the AERMOD model.  Added Hb or Zplm to calling
c               arguments for WAKE_XSIG.
c               R.W. Brode, PES, Inc. - 07/05/01
c
c --- INPUTS:
c              ldb - logical     - Debug output switch
c              xbi - real        - Downwind distance (m) from upwind
c                                  face of bldg to point where plume
c                                  centerline enters wake
c         XTR(ntr) - real        - Downwind distance from source (m)
c         ZTR(ntr) - real        - Plume centerline height (m)
c              NTR - integer     - Number of entries in arrays
c
c     Common block /PARAMS/ variables:
c           MXNTR, MXNW
c     Common block /WAKEDAT/ variables:
c           Hb, Wb, xLb, Rb, xLC, xLR, HR, XBADJ, YBADJ
c
c --- OUTPUT:
c
c     Common block /WAKEDAT/ variables:
c           FQCAV
c
c --- WAKE_FQC called by:  NUMRISE
c --- WAKE_FQC calls    :  NUMGRAD, WAKE_XSIG, CAVITY_HT, FRGAUSS
c----------------------------------------------------------------------
c
      USE PRIME_params
      USE PRIME_wakedat

      IMPLICIT NONE

      INTEGER NTR, nstep, is, DBGUNT

      DOUBLE PRECISION XBI, XTR(ntr), ZTR(ntr)
      DOUBLE PRECISION xbstart,xbend,xrange,yba,xend,xstep,x,xb,fz,
     &                 fract,xb85,fq85,zplm,sz,sy,szc,syc,fractz0,
     &                 zcav,ycav,fractz,fracty,xmax,fzmax

      logical ldb

      fqcav=0.0D0

c --- Define range of distances from upwind face of bldg at which to
c --- evaluate plume mass fraction within cavity
      xbstart=MAX(xbi,xLb)
      xbend=xLb+xLR
      xrange=0.99D0*(xbend-xbstart)
      yba=DABS(ybadj)
c --- Distance from source to end of cavity
      xend=xbend+xbadj

c --- Use at least 5 steps, with a maximum length of 10 m
      nstep=MAX(5,IDINT(1.0D0+xrange/10.D0))
      xstep=xrange/DBLE(nstep)

c --- For vertical plane, compute mass fraction below Hb at the
c --- downwind end of the cavity.  This allows the influence of plume
c --- rise to continue lifting plume mass out of the influence of
c --- of the cavity structure for strongly buoyant releases.
c --- Use this value as a cap to fractz.
      call NUMGRAD(xend,xtr,ztr,ntr,zplm)
      call WAKE_XSIG(xend,Hb,0.0D0,.TRUE.,sz,sy,szc,syc)
      call FRGAUSS(zplm,sz,Hb,-Hb,fractz0)

      do is=0,nstep
         xb=xbstart+DBLE(is)*xstep
         x=xb+xbadj
         call NUMGRAD(x,xtr,ztr,ntr,zplm)
         call CAVITY_HT(Hb,Wb,xLb,Rb,xLC,xLR,HR,xb,zcav,ycav)
         call WAKE_XSIG(x,zplm,0.0D0,.TRUE.,sz,sy,szc,syc)
         call FRGAUSS(zplm,sz,zcav,-zcav,fractz)
         call FRGAUSS(yba,sy,ycav,-ycav,fracty)
         fz=MIN(fractz,fractz0)
         fract=fz*fracty
         if(fract.GT.fqcav) then
            fqcav=fract
            xmax=x
            fzmax=fz
         endif
      enddo

c --- Additional constraint:  account for fluctuations in cavity
c --- boundary on capturing low-level plumes by imposing a maximum
c --- capture fraction that linearly drops from 1.0 at 85% of the
c --- cavity length, to 0% at the end of the cavity.
      xb85=xLb+0.85D0*xLR
      if(xbstart.GT.xb85) then
         fq85=MAX( 0.0D0, 1.0D0-(xbstart-xb85)/(xbend-xb85) )
         fqcav=MIN(fqcav,fq85)
      endif

      if(ldb) then
         write(DBGUNT,*)
         write(DBGUNT,*)'WAKE_FQC:'
         write(DBGUNT,'(a,2x,3(f12.5,2x))') 'xbi, xbstart, xbend   = ',
     &                                       xbi, xbstart, xbend
         write(DBGUNT,'(a,2x,f12.5,I14,2x,f12.5)')
     &                 'xstep, nstep, fractz0 = ', xstep, nstep, fractz0
         write(DBGUNT,'(a,2x,2(f12.5,2x))') 'xb85,  xmax           = ',
     &                                       xb85,  xmax
         write(DBGUNT,'(a,2x,2(f12.5,2x))') 'fqcav, fzmax          = ',
     &                                       fqcav, fzmax
         write(DBGUNT,*)
      endif

      return
      end

c----------------------------------------------------------------------
      subroutine FRGAUSS(hcntr,sigma,h1,h2,fract)
c----------------------------------------------------------------------
c
c --- PRIME      Version:  1.0     Level:  970812               FRGAUSS
c                J. Scire, D. Strimaitis,  EARTH TECH
c                Prepared for EPRI under contract WO3527-01
c
c --- PURPOSE:  Compute the fraction of a Gaussian distribution between
c               two limits
c
c --- INPUTS:
c            HCNTR - real    - Center of Gaussian distribution (m)
c            SIGMA - real    - Standard deviation (m) of the
c                              distribution
c           H1, H2 - real    - Limits between which the distribution
c                              is to be integrated
c
c --- OUTPUT:
c            FRACT - real    - Fraction of the Gaussian distribution
c                              between H1 and H2
c
c --- FRGAUSS called by: WAKE_FQC
c --- FRGAUSS calls:     ERFDIF
c----------------------------------------------------------------------
c
      IMPLICIT NONE

      DOUBLE PRECISION hcntr,sigma,h1,h2,fract,s,z1,z2,erfdif
      DOUBLE PRECISION, PARAMETER :: sqrt2=1.41421356237310D0,
     *                               small=1.0D-5
c
c --- Prevent numerical problems with very small sigmas
      s=sqrt2*MAX(sigma,small)
c
      z1=(h1-hcntr)/s
      z2=(h2-hcntr)/s
c
      fract=0.5D0*DABS(ERFDIF(z1,z2))
c
      return
      end

c----------------------------------------------------------------------
      DOUBLE PRECISION FUNCTION ERFDIF(X1,X2)
c----------------------------------------------------------------------
c
c --- PRIME      Version:  1.0     Level:  970812                ERFDIF
c
c     Taken from:
c --- CALPUFF    Version: 4.0       Level: 900228                ERFDIF
c                R. Yamartino, SRC
c
c --- PURPOSE:  Computes the difference: erfdif = erf(x1) - erf(x2).
c               Various methods are used to avoid roundoff errors
c               depending on the values of the two arguments.
c
c --- INPUTS:
c
c                X1 - real    - Argument 1 (no units)
c                X2 - real    - Argument 2 (no units)
c
c --- OUTPUTS:
c
c            ERFDIF - real    - erf(x1) - erf(x2)
c
c --- ERFDIF called by:  FRGAUSS
c --- ERFDIF calls:      ERF,ERFC
c----------------------------------------------------------------------
C *** V3.21
c
      IMPLICIT NONE

      INTEGER ISIGN
      DOUBLE PRECISION FNERF, FNERFC
      DOUBLE PRECISION X1,X2,XTEST,XX1,XX2,ERFCX1,ERFCX2

      ERFDIF=0.0D0
      IF(X1.EQ.X2) GO TO 40
      IF((X1*X2) .LE. 0.0D0) GO TO 50
      XTEST=DABS(X2)
      IF(DABS(X1).LT.XTEST) XTEST=DABS(X1)
c --- Some compilers cannot handle reals .LT. 1.18e-38, so reset cut
c     IF(XTEST.GE.13.306D0) GO TO 40
      if(xtest .GE. 9.15D0) GO TO 40
      IF(XTEST .LT. 0.47D0) GO TO 50
C     CAN ONLY REACH HERE WHEN X1 AND X2 HAVE SAME SIGN.
      ISIGN=1
      XX1=X1
      XX2=X2
      IF(X1.GT.0.0D0) GO TO 30
      ISIGN=-1
      XX1=-XX1
      XX2=-XX2
C  30 ERFDIF=ISIGN*(FNERFC(XX2)-FNERFC(XX1))
   30 ERFCX1=0.0D0
      ERFCX2=0.0D0
c --- Some compilers cannot handle reals .LT. 1.18e-38, so reset cut
c     IF(XX1.LT.13.306D0) ERFCX1=FNERFC(XX1)
c     IF(XX2.LT.13.306D0) ERFCX2=FNERFC(XX2)
      if(xx1 .LT. 9.15D0) erfcx1=FNERFC(xx1)
      if(xx2 .LT. 9.15D0) erfcx2=FNERFC(xx2)
      ERFDIF=DBLE(ISIGN)*(ERFCX2-ERFCX1)
c --- Protect against flakey LAHEY compiler 4/9/89
      if(erfcx2.eq.erfcx1) erfdif=0.0D0
   40 RETURN
   50 ERFDIF=FNERF(X1)-FNERF(X2)
      RETURN
      END

c-----------------------------------------------------------------------
      DOUBLE PRECISION FUNCTION FNERF(XX)
c-----------------------------------------------------------------------
c
c --- PRIME      Version:  1.0     Level:  970812                   ERF
c
c     Taken from:
c --- CALPUFF    Version: 4.0       Level: 941228                   ERF
c                R. Yamartino, SRC
c
c --- PURPOSE:  Computes the error function, erf(x).
c ---           This is the Quick medium accuracy ERROR FUNCTION from
c ---           NBS 55.  Using an approximation due to Hastings;
c ---           absolute error about 3e-7
c
c
c --- INPUTS:
c
c                XX - real    - Argument  (no units)
c
c --- OUTPUTS:
c
c               ERF - real    - error function of x
c
c --- ERF called by:  ERFDIF
c --- ERF calls:   no routines
c----------------------------------------------------------------------
c
      IMPLICIT NONE

      DOUBLE PRECISION x, xx ,t, t16, a(6), xcut

      data a/0.0000430638D0, 0.0002765672D0, 0.0001520143D0,
     *       0.0092705272D0, 0.0422820123D0, 0.0705230784D0/
      data xcut/ 3.919206D0/
c
      x = DABS(xx)
      if(x .gt. xcut) then
         t16 = 0.0D0
      else
c
         t = ((((((((( a(1)*x + a(2) ) * x ) + a(3) ) * x ) + a(4) ) *
     x                    x ) + a(5) ) * x ) + a(6) ) * x
c
         t = 1.0D0 / (t + 1.0D0)
c
         t16 = t * t * t * t
         t16 = t16 * t16 * t16 * t16
      endif
c
      if(xx .gt. 0.0D0) then
         fnerf =  1.0D0 - t16
      else
         fnerf =  t16 - 1.0D0
      endif
c
      return
      end

c-----------------------------------------------------------------------
      DOUBLE PRECISION FUNCTION FNERFC(XX)
c-----------------------------------------------------------------------
c
c --- PRIME      Version:  1.0     Level:  970812                  ERFC
c
c     Taken from:
c --- CALPUFF    Version: 4.0       Level: 941228                  ERFC
c                R. Yamartino, SRC
c
c --- PURPOSE:  Computes the complementary error function, 1-erf(x).
c ---           This is the Quick medium accuracy COMP. ERROR FUNCTION
c ---           from NBS 55.  Using an approximation due to Hastings;
c ---           absolute error about 3e-7.  Asymptotic expression added
c ---           for large xx to reduce percent error.
c
c
c --- INPUTS:
c
c                XX - real    - Argument  (no units)
c
c --- OUTPUTS:
c
c              ERFC - real    - complementary error function of x
c
c --- ERFC called by:  ERFDIF
c --- ERFC calls:   no routines
c-----------------------------------------------------------------------
c
      IMPLICIT NONE

      DOUBLE PRECISION x, xx ,t, t16, a(6)
      DOUBLE PRECISION xcutl, xcuth, rtpii, z

      data a/0.0000430638D0, 0.0002765672D0, 0.0001520143D0,
     *       0.0092705272D0, 0.0422820123D0, 0.0705230784D0/
      data xcutl/-3.919206D0/
      data xcuth/13.306D0   /
      data rtpii/0.564189583547756D0/
c
      if(xx .gt. xcuth) then
         fnerfc = 0.0D0
c
      elseif(xx .lt. xcutl) then
         fnerfc = 2.0D0
c
      elseif(xx .gt. 2.79D0) then
         x = DABS(xx)
         z = 1.0D0 / x
         fnerfc = rtpii*z*DEXP(-x*x)*(1.0D0-0.5D0*z*z*(1.0D0-1.5D0*z*z))
c
      else
         x = DABS(xx)
         t = ((((((((( a(1)*x + a(2) ) * x ) + a(3) ) * x ) + a(4) ) *
     x                    x ) + a(5) ) * x ) + a(6) ) * x
c
         t = 1.0D0 / (t + 1.0D0)
c
c        fnerfc = t**16   for x > 0
         t16 = t * t * t * t
         t16 = t16 * t16 * t16 * t16
c
         if(xx .gt. 0.0D0) then
            fnerfc =  t16
         else
            fnerfc =  2.0D0 - t16
         endif
c
      endif
c
      return
      end

c----------------------------------------------------------------------
      subroutine wake_xsig(x,hstk,rise,NOBID,sz,sy,szc,syc)
c----------------------------------------------------------------------
c
c --- PRIME      Version:  1.0     Level:  970812             WAKE_XSIG
c                D. Strimaitis,  EARTH TECH
c                Prepared for EPRI under contract WO3527-01
c
c --- PURPOSE:  Extract sigmas in the wake by interpolating among the
c               stored values; compute sigmas outside tabulated range
c               using HOST sigma curves with BID or virtual source
c               adjustments
c
c --- MODIFIED: For use with the AERMOD model.  Replaced calls to SIGY
c               and SIGZ with calls to SIGYPR and SIGZPR.  Added virtual
c               source sigmas to be added to HOST sigmas in quadrature.
c               Also added physical stack height to calling arguments.
c               R.W. Brode, PES, Inc. - 07/05/01
c
c --- INPUTS:
c                X - real       - Downwind distance (m) from source
c             HSTK - real       - Physical stack height (m)
c             RISE - real       - Gradual plume rise (m)
c            NOBID - logical    - Directs use of buoyancy enhancement
c
c     Common block /PARAMS/ variables:
c           MXNTR
c     Common block /WAKEDAT/ variables:
c           NWAK, XWAK(mxntr), SZWAK(mxntr), SYWAK(mxntr),
c           VSIGZ, VSIGY,
c           NCAV, XCAV(mxntr), SZCAV(mxntr), SYCAV(mxntr),
c           VSIGZC, VSIGYC
c
c --- OUTPUT:
c               SZ - real       - Sigma-z (m) at downwind distance X
c                                 due to primary source
c               SY - real       - Sigma-y (m) at downwind distance X
c                                 due to primary source
c              SZC - real       - Sigma-z (m) of cavity source at
c                                 downwind distance X from primary source
c              SYC - real       - Sigma-y (m) of cavity source at
c                                 downwind distance X from primary source
c
c --- WAKE_XSIG called by:  PDIS
c --- WAKE_XSIG calls:      SIGZ, SIGY
c----------------------------------------------------------------------
c
      USE PRIME_wakedat

      IMPLICIT NONE

      INTEGER I, nwkm1, ip1, ncvm1

      DOUBLE PRECISION x,hstk,rise,sz,sy,szc,syc
      DOUBLE PRECISION bidsq,fac
c
      logical NOBID

c --- Primary source:
c -------------------
      if(x.LE.0.0D0) then
c ---    Report null values (these should never get used!)
         sz=0.0D0
         sy=0.0D0
      elseif(nwak.LE.1) then
c ---    Plume never altered by wake turbulence; use HOST curves
         call SIGZPR(x,hstk+rise,sz)
         call SIGYPR(x,hstk+rise,sy)
         if(.not.NOBID) then
            bidsq=(rise/3.5D0)**2
            sz=DSQRT(sz**2+bidsq)
            sy=DSQRT(sy**2+bidsq)
         endif
      elseif(x.lt.xwak(1)) then
c ---    Point lies upwind of wake region; use HOST curves
         call SIGZPR(x,hstk+rise,sz)
         call SIGYPR(x,hstk+rise,sy)
         if(.not.NOBID) then
            bidsq=(rise/3.5D0)**2
            sz=DSQRT(sz**2+bidsq)
            sy=DSQRT(sy**2+bidsq)
         endif
      elseif(x.gt.xwak(nwak)) then
c ---    Point lies downwind of transition to ambient growth; use
c ---    HOST curves with virtual source term
         call SIGZPR(x,hstk+rise,sz)
         call SIGYPR(x,hstk+rise,sy)
         sz = DSQRT(sz*sz + vsigz*vsigz )
         sy = DSQRT(sy*sy + vsigy*vsigy )
      else
c ---    Point lies within range of tabulated values
         nwkm1=nwak-1
         sz=szwak(1)
         sy=sywak(1)
         do i=nwkm1,1,-1
            if(x.ge.xwak(i))then
               ip1=i+1
               fac=(xwak(ip1)-x)/(xwak(ip1)-xwak(i))
               sz=szwak(ip1)-(szwak(ip1)-szwak(i))*fac
               sy=sywak(ip1)-(sywak(ip1)-sywak(i))*fac
               goto 50
            endif
         enddo
      endif

c --- Cavity source:
c -------------------
50    if(ncav.LE.1) then
c ---    No contribution from cavity source (report initial values)
         szc=szcav(1)
         syc=sycav(1)
      elseif(x.lt.xcav(1)) then
c ---    Point lies upwind of cavity region (report initial values)
         szc=szcav(1)
         syc=sycav(1)
      elseif(x.gt.xcav(ncav)) then
c ---    Point lies downwind of transition to ambient growth; use
c ---    HOST curves with virtual source term
         call SIGZPR(x,0.0D0,szc)
         call SIGYPR(x,0.0D0,syc)
         szc = DSQRT(szc*szc + vsigzc*vsigzc)
         syc = DSQRT(syc*syc + vsigyc*vsigyc)
      else
c ---    Point lies within range of tabulated values
         ncvm1=ncav-1
         szc=szcav(1)
         syc=sycav(1)
         do i=ncvm1,1,-1
            if(x.ge.xcav(i))then
               ip1=i+1
               fac=(xcav(ip1)-x)/(xcav(ip1)-xcav(i))
               szc=szcav(ip1)-(szcav(ip1)-szcav(i))*fac
               syc=sycav(ip1)-(sycav(ip1)-sycav(i))*fac
               return
            endif
         enddo
      endif

      return
      end

c-----------------------------------------------------------------------
      subroutine cavity_ht(H,W,L,R,LC,LR,HR,x,zcav,ycav)
c-----------------------------------------------------------------------
c
c --- PRIME      Version:  1.0     Level:  970812             CAVITY_HT
c                L. Schulman, EARTH TECH
c                Prepared for EPRI under contract WO3527-01
c
c --- PURPOSE:  Calculates height of cavity envelope as function of x
c
c --- INPUTS:
c                H - real              - Building height above ground
c                W - real              - Projected building width
c                L - real              - Along-wind building length
c                                        building face
c                R - real              - Scale length from H and W
c               LC - real              - Length of roof cavity
c               LR - real              - Length of downwind cavity from
c                                         lee face
c               HR - real              - Maximum cavity height above
c                                         ground
c                x - real              - downwind distances
c
c --- OUTPUT:
c
c          zcav    - real              - cavity height as function of x
c
c          ycav    - real              - cavity half-width as f(x)

c --- CAVITY_HT called by:  PRIME
c --- CAVITY_HT calls:      none
c----------------------------------------------------------------------
c
c
c
      IMPLICIT NONE

      DOUBLE PRECISION H,W,L,R,HR,LR,LC,x,zcav,ycav

c --- Initialize
      zcav=0.0D0
      ycav=0.0D0

c --- Cavity is not present upwind of bldg or at/beyond L+LR
      if(x.GE.(L+LR)) then
         return
      elseif(x.LT.0.0D0) then
         return
      endif
c
c     calculate x-y near wake boundary
c
      if(x.ge.0.0D0 .and. x.le.R) then
        ycav=(W/2.D0+R/3.D0)-(x-R)**2/(3.D0*R)
      elseif(x.gt.R .and. x.le.(L+LR)) then
        ycav=(W/2.D0+R/3.D0)*DSQRT(1.D0-((x-R)/(L+LR-R))**2)
      endif

c     calculate x-z near wake boundary
c
      if(LC .lt. L)then       ! reattachment
c
        if(x.ge.0.D0 .and. x.le.L) then
          zcav=H
        elseif(x.ge.L .and. x.le.(L+LR)) then
          zcav=H*DSQRT(1.D0-((x-L)/LR)**2)
        endif
c
      else                    ! nonreattachment
        if(x.ge.0.D0 .and. x.le.0.5D0*R) then
          zcav=HR+4.D0*(x-0.5D0*R)**2*(H-HR)/(R**2)
        elseif(x.gt.0.5D0*R .and. x.le.(L+LR)) then
          zcav=HR*DSQRT(1.D0-((x-0.5D0*R)/(L+LR-0.5D0*R))**2)
        endif
      endif
c
      return
      end

      SUBROUTINE SIGYPR(XARG,ZARG,SYOUT)
C***********************************************************************
C                 SIGYPR Module of AERMOD Model
C
C        PURPOSE: Calculates Ambient Sigma-y Values From Dispersion Curves
C                 for use with the PRIME calculation
C
C        PROGRAMMER: Roger Brode
C
C        DATE:       July 5, 2001
C
C        INPUTS:  Downwind Distance
C                 Stability Class
C                 Rural or Urban Dispersion Option
C
C        OUTPUTS: Lateral Dispersion Coefficient, SYOUT
C
C        CALLED FROM:   NUMRISE, WAKE_DFSN, WAKE_XSIG
C***********************************************************************

C     Variable Declarations
      USE MAIN1
      IMPLICIT NONE
      CHARACTER (LEN=8) :: MODNAM
      DOUBLE PRECISION, PARAMETER :: EXPON = 1.0D0
      DOUBLE PRECISION :: XARG, SVOVRU, TYEFF,
     &                    XNODIM, DENOMI, BETALPH,
     &                    ZARG, SYOUT

C     Variable Initializations
      MODNAM = 'SIGYPR'

      IF( STABLE )THEN
C        The atmosphere is stable or the release is above the CBL mixing ht.
C        Calculate SV/U, but not less than PARAMETER, SVUMIN = 0.05
         SVOVRU = MAX (SVUMIN, SVEFF/UEFF)
         TYEFF  = (ZIMECH/(156.0D0*SVEFF)) * (MAX(ZARG, 0.46D0)/0.46D0)
         SYAMB  = (SVOVRU * XARG)/(1.0D0+XARG/(2.0D0*UEFF*TYEFF))**0.3D0

      ELSEIF( UNSTAB )THEN
C        The atmosphere is unstable and the release is below the CBL mixing ht.
C        Calculate SV/U, but not less than PARAMETER, SVUMIN = 0.05
         SVOVRU = MAX (SVUMIN, SVEFF/UEFF)
         XNODIM = SVOVRU * XARG / ZI
         DENOMI = MAX (ZARG, 0.46D0)
C ---    BETALPH= MAX( 78.0D0*(0.46D0/DENOMI)**EXPON , 0.7D0)
C ---             recode without EXPON, since EXPON=1
         BETALPH= MAX( 78.0D0*(0.46D0/DENOMI), 0.7D0)
         SYAMB  = (SVOVRU * XARG) / (1.0D0+BETALPH*XNODIM)**0.3D0

      ENDIF

      SYOUT = SYAMB

      RETURN
      END

      SUBROUTINE SIGZPR(XARG,ZARG,SZOUT)
C***********************************************************************
C                 SIGZPR Module of AERMOD Model
C
C        PURPOSE: Calculates Ambient Sigma-z Values From Dispersion Curves
C                 for use with the PRIME calculation
C
C        PROGRAMMER: Roger Brode
C
C        DATE:    July 5, 2001
C
C        INPUTS:  Downwind Distance
C                 Stability Class
C                 Rural or Urban Dispersion Option
C
C        OUTPUTS: Vertical Dispersion CoEFFicient, SZOUT
C
C        CALLED FROM:   NUMRISE, WAKE_DFSN, WAKE_XSIG
C***********************************************************************

C     Variable Declarations
      USE MAIN1
      IMPLICIT NONE
      CHARACTER (LEN=8) :: MODNAM
      INTEGER :: NDXZHE
      DOUBLE PRECISION :: XARG, TTRAVL, PTP, BVFRQ, ZTMP, SIGF, ALPHAB,
     &                    ZARG, SZAD, SZOUT

C     Variable Initializations
      MODNAM = 'SIGZPR'

C     TTRAVL  = Travel time

      IF( STABLE )THEN
C        The atmosphere is stable or the release was above the CBL mixing ht.
C        See Eq. 1 of the document by Venkatram referenced above.

         TTRAVL = XARG / UEFF

C----    Apply Sigma-Z formulation from CTDMPLUS
C----    Locate index below HE, and retrieve potential temperature at HE

         CALL LOCATE(GRIDHT, 1, MXGLVL, ZARG, NDXZHE)

         IF (NDXZHE .GE. 1) THEN
            CALL GINTRP( GRIDHT(NDXZHE), GRIDPT(NDXZHE),
     &         GRIDHT(NDXZHE+1), GRIDPT(NDXZHE+1), ZARG, PTP )
         ELSE
            PTP  = GRIDPT(1)
         END IF

         BVFRQ = DSQRT( G * TGEFF / PTP )
         IF(BVFRQ .LT. 1.0D-10) BVFRQ = 1.0D-10

C        Set height for calculating sigma-z, ZTMP
         ZTMP = MAX( ZARG, 1.0D-4 )

         IF (URBSTAB) THEN
C           Set BVF term to zero for urban stable boundary layer
            SZAMB = SWEFF * TTRAVL / DSQRT( 1.0D0 + SWEFF*TTRAVL *
     &                        ( 1.0D0/(0.72D0*ZTMP) ) )

         ELSE
            SZAMB = SWEFF * TTRAVL / DSQRT( 1.0D0 + SWEFF*TTRAVL *
     &                  ( 1.0D0/(0.72D0*ZTMP) + BVFRQ/(0.54D0*SWEFF) ) )
         END IF

         IF (ZARG .LT. ZI) THEN
            CALL SZSFCLPR (XARG,ZARG)

            SIGF = MIN ( ZARG/ZI, 1.0D0)
            SZAS = (1.0D0 - SIGF) * SZSURF + SIGF * SZAMB
         ELSE
            SZAS = SZAMB
         END IF

         SZOUT = SZAS


      ELSEIF( UNSTAB )THEN

         IF (ZARG .GE. 0.1D0*ZI) THEN
            ALPHAB = 1.0D0
         ELSE
            ALPHAB = 0.6D0 + 0.4D0*(ZARG/(0.1D0*ZI))
         END IF

         SZAD = ALPHAB * SWEFF * XARG / UEFF

         CALL SZSFCLPR (XARG,ZARG)

         SZOUT = DSQRT( SZAD*SZAD + SZSURF*SZSURF )

      ENDIF

      RETURN
      END

      SUBROUTINE SZSFCLPR (XARG, ZARG)
C***********************************************************************
C             SZSFCLPR Module of the AMS/EPA Regulatory Model - AERMOD
C
C        PURPOSE: Calculates the surface layer dispersion term for sigma_Z,
C                 for use with the PRIME calculation
C
C        PROGRAMMER: Jim Paumier and Roger Brode, PES, Inc.
C
C        DATE:    September 30, 1993
C
C        REVISIONS:  SZSURF formula revised according to P.D.F. Model
C                    for Dispersion in the Convective Boundary Layer,
C                    J.C. Weil.  Revised 7/13/94, R.F. Lee.
C
C                    Fixed stable SZSURF to match MFD.  R. Brode, PES, 12/5/94
C
C        INPUTS:  Stack height (HS)
C                 Mixing height (ZI)
C                 Friction velocity (USTAR)
C                 Downwind distance (XARG)
C                 Effective wind speed (UEFF)
C                 Monin-Obukhov length (OBULEN)
C
C        OUTPUTS: Surface layer dispersion contribution to sigma_Z
C
C        CALLED FROM:   SIGZPR
C
C        References:  "A Dispersion Model for the Convective Boundary
C                      Layer", J. Weil, 8/27/93
C
C***********************************************************************

C     Variable Declarations
      USE MAIN1
      IMPLICIT NONE
      CHARACTER MODNAM*12
      DOUBLE PRECISION :: XARG, ZARG

C     Variable Initializations
      MODNAM = 'SZSFCLPR'

C NOTE --->  BSUBC = 0.5 is set in a PARAMETER stmt in MODULE MAIN1

C---- Calculate the surface layer contribution.

      IF (UNSTAB .AND. ZARG .LT. 0.1D0*ZI) THEN
         SZSURF = BSUBC * ( 1.0D0 - 10.D0 * (ZARG / ZI)) *
     &            (USTAR / UEFF)*(USTAR / UEFF)  *
     &            (XARG * XARG / DABS( OBULEN ))

      ELSEIF (STABLE) THEN
         SZSURF = (RTOF2/RTOFPI) * USTAR * (XARG/UEFF) *
     &            (1.0D0 + 0.7D0*XARG/OBULEN)**(-THIRD)

      ELSE
         SZSURF = 0.0D0

      ENDIF

      RETURN
      END
