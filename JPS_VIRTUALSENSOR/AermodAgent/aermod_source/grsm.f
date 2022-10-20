      SUBROUTINE GRSM_CALC
C***********************************************************************
C        GRSM_CALC Module of the AMS/EPA Regulatory Model - AERMOD
C
C        PURPOSE: Processes Hourly Results for GRSM Option
C
C        PROGRAMMER: CERC
C
C        DATE:    November 2020
C
C        MODIFIED: 
C
C        INPUTS:
C
C
C        OUTPUTS:
C
C
C        CALLED FROM:   HRLOOP, EV_LOOP, MAXDCALC
C***********************************************************************
      USE MAIN1
      USE GRSMMOD
      IMPLICIT NONE
      
C --- Local variables
      INTEGER::NUMGRP_USE, IGRP_USE, nStat
      CHARACTER MODNAM*12, NightStr*3
      DOUBLE PRECISION, ALLOCATABLE::PLUMEBKGNO2(:), PLUMEBKGNO(:) !Plume background concentrations
      DOUBLE PRECISION, ALLOCATABLE:: NOXPERSOURCE(:), PLUMEBKGO3(:) !NOX concentration for each source
      DOUBLE PRECISION, ALLOCATABLE::SIGYSIGZ_I_X(:), SIGYSIGZ_E_X(:) !Instantaneous and ensemble plume size at X
      DOUBLE PRECISION::SIGYSIGZ_I_0  !Instantaneous plume size at origin
      DOUBLE PRECISION::MINBKGNO2, MINBKGNO, MINBKGO3 !Minimum background concentrations
      DOUBLE PRECISION::CONCMAXNOX, INSTCONCNOX !Maximum and total NOX concentrations
      DOUBLE PRECISION::CONCSOURCENO2, CONCSOURCENO !Concentrations for a single source
      DOUBLE PRECISION::ENSCONCNO2, ENSCONCNO, INSTCONCNO2, INSTCONCNO !Total ensemble and instantaneous concentrations
      DOUBLE PRECISION::NO2Frac !NO2 fraction
      DOUBLE PRECISION::BGCONC_IN, BGCONC_OUT, BGCONC_PPB
      LOGICAL::L_DoFullConv
      DOUBLE PRECISION::fDum
      
C --- Initialise
      MODNAM        = 'GRSM_CALC'
      L_DoFullConv  = .FALSE.
      HRVAL         = 0.0D0
      SIGYSIGZ_I_0  = 0.0D0
      CONCSOURCENO2 = 0.0
      CONCSOURCENO  = 0.0
      CONCMAXNOX    = 0.0
      MINBKGNO2     = 0.0
      MINBKGNO      = 0.0
      MINBKGO3      = 0.0
          
      IF(ALLOCATED(PLUMEBKGNO2)) DEALLOCATE(PLUMEBKGNO2,STAT=nStat)
      ALLOCATE(PLUMEBKGNO2(NUMSRC),STAT=nStat)
      IF(nStat/=0)THEN
        CALL ERRHDL(PATH, MODNAM, 'E','615','GRSM') 
      END IF
      IF(ALLOCATED(PLUMEBKGNO)) DEALLOCATE(PLUMEBKGNO,STAT=nStat)
      ALLOCATE(PLUMEBKGNO(NUMSRC),STAT=nStat)
      IF(nStat/=0)THEN
        CALL ERRHDL(PATH, MODNAM, 'E','615','GRSM') 
      END IF
      IF(ALLOCATED(PLUMEBKGO3)) DEALLOCATE(PLUMEBKGO3,STAT=nStat)
      ALLOCATE(PLUMEBKGO3(NUMSRC),STAT=nStat)
      IF(nStat/=0)THEN
        CALL ERRHDL(PATH, MODNAM, 'E','615','GRSM') 
      END IF
      IF(ALLOCATED(NOXPERSOURCE)) DEALLOCATE(NOXPERSOURCE,STAT=nStat)
      ALLOCATE(NOXPERSOURCE(NUMSRC),STAT=nStat)
      IF(nStat/=0)THEN
        CALL ERRHDL(PATH, MODNAM, 'E','615','GRSM') 
      END IF
      
      PLUMEBKGNO2   = 0.0
      PLUMEBKGNO    = 0.0
      PLUMEBKGO3    = 0.0
      NOXPERSOURCE  = 0.0
      
      !Calculate NOx reaction rates
      CALL RXNRATES

      !Determine if its a night-time hour
      IF(QSW>0.0D0)THEN  
        L_NightHour = .FALSE.
        NightStr    = 'NO'
      ELSE
        L_NightHour = .TRUE.
        NightStr    = 'YES'
      END IF
      
      IF(EVONLY)THEN
        BGCONC_IN = EV_BGCONC(IHOUR)
      ELSE
        BGCONC_IN = BGCONC
      END IF

C --- Initialise concs by converting from ug m-3 to ppb.  Don't want to alter input bgd concs
      NO2CONC_BG = BGCONC_IN*NO2_PPB     !This is NO2 bgd
      IF(NOXMISS.OR.L_CalcNOxFromNO2)THEN
        !No NOx bgd
      ELSE
        NOXCONC_BG = NOXBGCONC*NO2_PPB  !This is NOx bgd (NOx as NO2).         
      END IF
      
C --- Calculate the upwind concentrations using the given background
C     values. Assumes some type of equilibrium
      IF(.NOT.O3MISS)THEN
        !If O3 bgd is missing assume full conversion later
        O3CONC_BG  = O3CONC/O3_PPB      !This is O3 bgd
        CALL GETBGDEQUIL  
      ELSE
        O3CONC_BG  = -999.
        L_DoFullConv=.TRUE.    
      END IF  

      IF(EVONLY)THEN
        !Event only - 1 grp
        NUMGRP_USE=1
      ELSE
        !Normal calc - use actual number of groups
        NUMGRP_USE=NUMGRP
      END IF
      
C --- Begin Source Group LOOP to sum values
      DO IGRP = 1, NUMGRP_USE
        
        IF(EVONLY)THEN
          !Event only
          IGRP_USE=IDXEV(IEVENT)
        ELSE
          !Normal calculation use actual group index
          IGRP_USE=IGRP
        END IF
          

        DO IREC = 1, NUMREC            
          IF(.NOT.L_DoFullConv)THEN
            !Not doing full conversion, doing proper chemistry
          
            INSTCONCNO2 = 0.0
            INSTCONCNO  = 0.0         
            INSTCONCNOX = 0.0
            CONCMAXNOX  = 0.0
            
            !Allocate variables for dilution and entrainment
            IF(ALLOCATED(SIGYSIGZ_I_X)) DEALLOCATE(SIGYSIGZ_I_X,
     +                                                     STAT=nStat)
            IF(ALLOCATED(SIGYSIGZ_E_X)) DEALLOCATE(SIGYSIGZ_E_X,
     +                                                     STAT=nStat)
            ALLOCATE(SIGYSIGZ_I_X(NUMSRC),STAT=nStat)
            IF(nStat/=0)THEN
              CALL ERRHDL(PATH, MODNAM, 'E','615','GRSM') 
            END IF
            ALLOCATE(SIGYSIGZ_E_X(NUMSRC),STAT=nStat)
            IF(nStat/=0)THEN
              CALL ERRHDL(PATH, MODNAM, 'E','615','GRSM') 
            END IF
            SIGYSIGZ_I_X(:) = 0.0D0
            SIGYSIGZ_E_X(:) = 0.0D0

            DO ISRC = 1, NUMSRC
              NOXPERSOURCE(ISRC) = 0.0D0
              !Is src included in group?
              IF (IGROUP(ISRC,IGRP_USE) == 0) CYCLE  
              IF ( CHI(IREC,ISRC,1) == 0.0D0) CYCLE
C ---           Get concentrations
              CONCSOURCENO2 = 
     &              ANO2_RATIO(ISRC)*CHI(IREC,ISRC,1)*NO2_PPB         !This is the NO2 conc
              CONCSOURCENO  = 
     &             (1.0D0-ANO2_RATIO(ISRC))*CHI(IREC,ISRC,1)*NO2_PPB  !This is the NO conc   
                
              !Find downwind distance to current receptor
              CALL SETSRC
              WDSIN = AWDSIN(ISRC)
              WDCOS = AWDCOS(ISRC)
              IF (EVONLY) THEN
                CALL XYDIST(IEVENT)
              ELSE
                CALL XYDIST(IREC)
              END IF

              !Calculate travel time for this source
              TTRAVCHM(IREC)=CHI_TTRAVCHM(IREC,ISRC)/CHI(IREC,ISRC,1)
      
              !Implement dilution and entrainment for downstream receptors
              IF(X>0.0)THEN
                
                !Find the sizes of the instantaneous plume at origin (source)
                CALL PLUMESIZES(0.0D0,0.0D0,SIGYSIGZ_I_0,fDum)
              
                !Find the sizes of the plume at X
                CALL PLUMESIZES(X,TTRAVCHM(IREC),
     +                          SIGYSIGZ_I_X(ISRC),SIGYSIGZ_E_X(ISRC))
                
                !Ensure I_0 <= I_X <= E_X
                IF(SIGYSIGZ_I_X(ISRC) > SIGYSIGZ_E_X(ISRC))
     +                           SIGYSIGZ_I_X(ISRC) = SIGYSIGZ_E_X(ISRC)
                IF(SIGYSIGZ_I_0 > SIGYSIGZ_I_X(ISRC))
     +                           SIGYSIGZ_I_0 = SIGYSIGZ_I_X(ISRC)
         
                !Get the instantaneous plume concentration
                CONCSOURCENO2= CONCSOURCENO2*
     +                         SIGYSIGZ_E_X(ISRC)/(SIGYSIGZ_I_X(ISRC))
                CONCSOURCENO = CONCSOURCENO*
     +                         SIGYSIGZ_E_X(ISRC)/(SIGYSIGZ_I_X(ISRC))

                !Calculate the entrained concentration
                IF(GRP_BACK(IGRP_USE).AND..NOT.L_NIGHTHOUR)THEN
                  PLUMEBKGNO2(ISRC)= (1.0D0-
     +                     SIGYSIGZ_I_0/SIGYSIGZ_I_X(ISRC))*NO2CONC_BG
                  PLUMEBKGNO(ISRC) = (1.0D0-
     +                     SIGYSIGZ_I_0/SIGYSIGZ_I_X(ISRC))*NOCONC_BG
                ELSE
                  PLUMEBKGNO2(ISRC)= 0.0D0
                  PLUMEBKGNO(ISRC) = 0.0D0
                END IF
                
                PLUMEBKGO3(ISRC)=(1.0D0-SIGYSIGZ_I_0/
     +                           SIGYSIGZ_I_X(ISRC))*O3CONC_BG
              ELSE
                !Upwind fully entrains background
                IF(GRP_BACK(IGRP_USE).AND..NOT.L_NIGHTHOUR)THEN
                  PLUMEBKGNO2(ISRC)= NO2CONC_BG
                  PLUMEBKGNO(ISRC) = NOCONC_BG     
                ELSE
                  PLUMEBKGNO2(ISRC)= 0.0D0
                  PLUMEBKGNO(ISRC) = 0.0D0
                END IF
                PLUMEBKGO3(ISRC)= O3CONC_BG        
              END IF
                
              INSTCONCNO2 = INSTCONCNO2 + CONCSOURCENO2
              INSTCONCNO  = INSTCONCNO  + CONCSOURCENO
              NOXPERSOURCE(ISRC) = CONCSOURCENO2 + CONCSOURCENO
              INSTCONCNOX = INSTCONCNOX + NOXPERSOURCE(ISRC)
                
              IF (NOXPERSOURCE(ISRC) > CONCMAXNOX) THEN
                CONCMAXNOX = NOXPERSOURCE(ISRC)
              END IF
                
            END DO
           
            IF(GRP_BACK(IGRP_USE).AND..NOT.L_NIGHTHOUR)THEN
              MINBKGNO2 = NO2CONC_BG
              MINBKGNO  = NOCONC_BG
            ELSE
              MINBKGNO2 = 0.0D0
              MINBKGNO  = 0.0D0
            END IF
            MINBKGO3  = O3CONC_BG
            
            !Calculate the minimum background based on O3
            DO ISRC = 1, NUMSRC
              IF (NOXPERSOURCE(ISRC)<=0.5*CONCMAXNOX) CYCLE
              IF (MINBKGO3 > PLUMEBKGO3(ISRC)) THEN          
                MINBKGNO2 = PLUMEBKGNO2(ISRC)
                MINBKGNO  = PLUMEBKGNO(ISRC)
                MINBKGO3  = PLUMEBKGO3(ISRC)
              END IF
            END DO
            
            !Put this set of background values into equilibrium
            IF(GRP_BACK(IGRP_USE).AND..NOT.L_NIGHTHOUR)THEN
              CALL QuadraticEquil(MINBKGNO2,MINBKGNO,MINBKGO3)
            ENDIF
            
            INSTCONCNO2 = INSTCONCNO2 + MINBKGNO2
            INSTCONCNO  = INSTCONCNO  + MINBKGNO
            
C ---       Calculate weighted time
            IF(SUM(CHI(IREC,:,1),MASK=IGROUP(:,IGRP_USE)==1)/=0.0D0)THEN
              TTRAVCHM(IREC)=
     &        SUM(CHI_TTRAVCHM(IREC,:),MASK=IGROUP(:,IGRP_USE)==1)/
     &        SUM(CHI(IREC,:,1),MASK=IGROUP(:,IGRP_USE)==1)
            ELSE
              TTRAVCHM(IREC)=0.0D0  
            END IF
C ---       Set time to be a maximum of one hour
            TTRAVCHM(IREC)=MIN(TTRAVCHM(IREC),3600.0D0)
            
            CONCTEMP(nNO)  = INSTCONCNO
            CONCTEMP(nNO2) = INSTCONCNO2
            CONCTEMP(nO3)  = MINBKGO3         

C ---       Do GRS chemistry without dilution and entrainment
            CALL DoGRSMChem          
            
            INSTCONCNO  = CONCTEMP(nNO)  - MINBKGNO
            INSTCONCNO2 = CONCTEMP(nNO2) - MINBKGNO2
            
            ENSCONCNO   = 0.0
            ENSCONCNO2  = 0.0
            
            DO ISRC = 1, NUMSRC
              IF (INSTCONCNOX==0.OR.NOXPERSOURCE(ISRC)==0) THEN
                CONCSOURCENO2 = 0.0
                CONCSOURCENO  = 0.0
              ELSE
                CONCSOURCENO2=INSTCONCNO2*NOXPERSOURCE(ISRC)/INSTCONCNOX
                CONCSOURCENO =INSTCONCNO *NOXPERSOURCE(ISRC)/INSTCONCNOX
              
                CALL SETSRC
                WDSIN = AWDSIN(ISRC)
                WDCOS = AWDCOS(ISRC)
                IF (EVONLY) THEN
                  CALL XYDIST(IEVENT)
                ELSE
                  CALL XYDIST(IREC)
                END IF
                IF(X>0.0)THEN
                  CONCSOURCENO2=CONCSOURCENO2*
     +                         SIGYSIGZ_I_X(ISRC)/(SIGYSIGZ_E_X(ISRC))
                  CONCSOURCENO =CONCSOURCENO*
     +                         SIGYSIGZ_I_X(ISRC)/(SIGYSIGZ_E_X(ISRC))
                END IF
              END IF
              
              ENSCONCNO2 = ENSCONCNO2 + CONCSOURCENO2
              ENSCONCNO  = ENSCONCNO  + CONCSOURCENO
            END DO
            
            IF(GRP_BACK(IGRP_USE).AND..NOT.L_NIGHTHOUR)THEN
              ENSCONCNO2 = ENSCONCNO2 + NO2CONC_BG
              ENSCONCNO  = ENSCONCNO  + NOCONC_BG     
            END IF
            
C ---       Calculate secondary NO2 fraction after chemistry
            IF((ENSCONCNO2+ENSCONCNO)/=0.0D0)THEN
              NO2Frac = ENSCONCNO2 / (ENSCONCNO2 + ENSCONCNO)  
            ELSE
              NO2Frac = 0.0D0
            END IF
C ---       Calculate post chemistry concentrations from secondary NO2 fraction, partitioning background and plume
            IF(L_NIGHTHOUR)THEN
              !Night-time
              BGCONC_OUT = NO2CONC_BG/NO2_PPB  !No NO at night-time
              BGCONC_PPB = NO2CONC_BG
            ELSE
              !Day-time
              !Background NO2 (other background concs not o/p)
              BGCONC_OUT = NO2Frac * NOXCONC_BG/NO2_PPB 
              BGCONC_PPB = NO2Frac * NOXCONC_BG
            END IF
          ELSE  !Missing O3 or NOx doing full conversion
            NO2Frac=1.0D0
            !Warning has already been issued in O3EXT
            BGCONC_OUT = BGCONC_IN
            BGCONC_PPB = BGCONC_IN*NO2_PPB
          END IF

          
          SOURCE_LOOP: DO ISRC = 1, NUMSRC          

            IF( IGROUP(ISRC,IGRP_USE) .NE. 1 )THEN
C ---         This source is not included in the current SRCGROUP
              CYCLE SOURCE_LOOP            
            END IF
 
            !Plume
            DO ITYP = 1,NUMTYP
              HRVAL(ITYP) = NO2Frac * CHI(IREC,ISRC,ITYP)  
            END DO 
                   
C ---       Call SUMVAL to add current source's contribution to AVEVAL
            IF( .NOT.EVONLY )THEN
C ---         Processing for NON-EVENT application 
              CALL SUMVAL            
              
C ---         Write debug file
              IF (GRSMDEBUG) THEN
                IF (IGROUP(ISRC,IGRP) .EQ. 1) THEN
                  WRITE(GRSMDBG,9989,ERR=999) KURDAT, IREC,
     &                 GRPID(IGRP),  ISRC, SRCID(ISRC), NIGHTSTR, 
     &                 O3CONC_BG,NOXCONC_BG,NO2CONC_BG,TTRAVCHM(IREC),
     &                 CHI_TTRAVCHM(IREC,ISRC), ANO2_RATIO(isrc),
     &                 CHI(IREC,ISRC,1), NO2Frac, BGCONC_PPB, HRVAL(1),
     &                 AVEVAL(IREC,IGRP,1,1),SIGYSIGZ_I_X(ISRC),
     &                 SIGYSIGZ_E_X(ISRC)
                END IF
              END IF
9989          FORMAT(1x,7x,i8.8,2x,i6,2x,a8,2x,i6,2x,a12,2x,a3,8x,2x,7x,
     &               e12.5,2x,8x,e12.5,2x,8x,e12.5,2x,3x,e12.5,2x,20x,
     &               e12.5,2x,4x,e12.5,2x,13x,e12.5,2x,2x,e12.5,2x,7x,
     &               e12.5,2x,5x,e12.5,2x,6x,e12.5,2x,7x,e12.5,2x,10x,
     &               e12.5)
            ELSE
C ---         Processing for EVENT application
              CALL EV_SUMVAL             
              
C ---         Write debug file
              IF (GRSMDEBUG) THEN
                IF (IGROUP(ISRC,IDXEV(IEVENT)) .EQ. 1) THEN
                  WRITE(GRSMDBG,9990,ERR=999) KURDAT, IEVENT, 
     &                 EVNAME(IEVENT), EVAPER(IEVENT), 
     &                 GRPID(IDXEV(IEVENT)), ISRC, SRCID(ISRC), 
     &                 NIGHTSTR, O3CONC_BG,NOXCONC_BG, NO2CONC_BG,
     &                 TTRAVCHM(IREC), CHI_TTRAVCHM(IREC,ISRC), 
     &                 ANO2_RATIO(isrc),CHI(IREC,ISRC,1), NO2Frac, 
     &                 BGCONC_PPB, HRVAL(1), GRPAVE(IDXEV(IEVENT)),
     &                 SIGYSIGZ_I_X(ISRC),SIGYSIGZ_E_X(ISRC)
                END IF
              END IF
9990          FORMAT(1x,7x,i8.8,2x,i6,2x,a10,2x,1x,i3,2x,a8,2x,i6,2x,
     &               a12,2x,a3,8x,2x,7x,e12.5,2x,8x,e12.5,2x,8x,e12.5,
     &               2x,3x,e12.5,2x,20x,e12.5,2x,4x,e12.5,2x,13x,e12.5,
     &               2x,2x,e12.5,2x,7x,e12.5,2x,5x,e12.5,2x,9x,e12.5,2x,
     &               7x,e12.5,2x,10x,e12.5)
            END IF
          
            IF (EVAL(ISRC)) THEN
C ---         Check ARC centerline values for EVALFILE output - this is same for EVENT/NON-EVENT
              CALL EVALCK
            END IF              

C ---       ReInitialize __VAL arrays (1:NUMTYP)
            HRVAL  = 0.0D0              
          END DO SOURCE_LOOP          
            
          IF (GRP_BACK(IGRP_USE)) THEN
            IF( .NOT.EVONLY )THEN  
C ---         Call SUMBACK_NO2 to update BACKGROUND contributions
              BGCONC = BGCONC_OUT
              CALL SUMBACK_NO2
            ELSE
C ---         Call EV_SUMBACK to update BACKGROUND contributions
              EV_BGCONC(IHOUR) = BGCONC_OUT
              CALL EV_SUMBACK
            ENDIF
          ENDIF
        END DO  !Loop over points
      END DO  !Loop over groups

      GO TO 3456
999   CALL ERRHDL(PATH,MODNAM,'E','520','GRSMDEBUG')
3456  CONTINUE

      RETURN    
      END SUBROUTINE GRSM_CALC
      
      SUBROUTINE RXNRATES
C***********************************************************************
C        RxnRates Module of the AMS/EPA Regulatory Model - AERMOD
C
C        PURPOSE: Calculate the reaction rates for GRS chemistry
C
C        PROGRAMMER: CERC
C
C        DATE: November 2020
C
C        INPUTS:  
C
C        OUTPUTS: 
C      
C        NOTES:
C      
C        R1, R2 reaction rates for
C
C                 NO + O3 -> NO2         (1)
C
C                 NO2 + hv -> NO + O3      (2)
C
C        CALLED FROM: GRSM_CALC
C***********************************************************************

C     Variable Declarations
      USE MAIN1
      USE GRSMMOD
      IMPLICIT NONE
      CHARACTER MODNAM*12
      
      DOUBLE PRECISION, PARAMETER:: SCRNHT=1.2D0
      DOUBLE PRECISION, PARAMETER:: CONST1=4.405D-2
      DOUBLE PRECISION, PARAMETER:: EXP1=1.370D3
      DOUBLE PRECISION, PARAMETER:: CONST2A=8.0D-4
      DOUBLE PRECISION, PARAMETER:: CONST2B=7.4D-6
      DOUBLE PRECISION, PARAMETER:: EXP2=10.0D0
      DOUBLE PRECISION:: CHEMTK, CHEMPT
      
      INTEGER::NSCRNHT

C --- Variable Initializations
      MODNAM = 'RXNRATES'
      
C --- Determine temperature (K) at screen height
      IF(TREFHT==SCRNHT)THEN
        !Reference height is screen height
        CHEMTK=TA
      ELSE
        !Interpolate from potential temperature
        CALL LOCATE(GRIDHT, 1, MXGLVL, SCRNHT, NSCRNHT)    
        CALL GINTRP(GRIDHT(NSCRNHT),GRIDPT(NSCRNHT),GRIDHT(NSCRNHT+1),
     &              GRIDPT(NSCRNHT+1),SCRNHT,CHEMPT)    
        !Convert to temperature
        CHEMTK=CHEMPT-GOVRCP*(SCRNHT+ZBASE)
      END IF
      
C --- Calculate reaction rates
      R1=CONST1*EXP(-EXP1/CHEMTK)
      R1 = MAX(R1,1.0D-6) !1e-6 corresponds to temperature of around -150 degC
      IF (QSW==0) THEN
        R2=0.0
      ELSE
        R2=CONST2A*EXP(-EXP2/QSW)+CONST2B*QSW
      END IF
            
      RETURN
      END SUBROUTINE RXNRATES
      
      
      SUBROUTINE GETBGDEQUIL
C***********************************************************************
C        GETBDGEQUIL Module of the AMS/EPA Regulatory Model - AERMOD
C
C        PURPOSE: Calculate the equilibrium background for GRS chemistry
C
C        PROGRAMMER: CERC
C
C        DATE: November 2020
C
C        INPUTS:  
C
C        OUTPUTS:
C
C        CALLED FROM: GRSM_CALC
C***********************************************************************
C     Variable Declarations
      USE MAIN1
      USE GRSMMOD
      IMPLICIT NONE
      CHARACTER MODNAM*12

C --- Variable Initializations
      MODNAM = 'GETBGDEQUIL'  
C --- Check for day or night
      IF (.NOT.L_NIGHTHOUR) THEN      
        IF(L_CalcNOxFromNO2.OR.NOXMISS)THEN
C ---     Calculate NOx bgd from NO2
C ---     Solve equilibrium eqns for NOx
            IF(O3CONC_BG/=0.0D0)THEN
            NOXCONC_BG=NO2CONC_BG/(O3CONC_BG)*(O3CONC_BG+R2/R1)
                NOCONC_BG=NOXCONC_BG-NO2CONC_BG
            END IF
        ELSE
!Calculate NO2 background from NOX values
C ---     If the concentration of NOx < NO2, then complain
             IF (NOXCONC_BG<NO2CONC_BG) THEN
                CALL ERRHDL(PATH, MODNAM, 'W','613','GRSM') 
                NOXCONC_BG = NO2CONC_BG 
          END IF
          NOCONC_BG = NOXCONC_BG - NO2CONC_BG
          CALL QuadraticEquil(NO2CONC_BG,NOCONC_BG,O3CONC_BG)
        END IF
        ELSE ! Nighttime
        IF(L_CalcNOxFromNO2.OR.NOXMISS)THEN
          !NOx is missing but don't need it
          NOXCONC_BG=-999.0D0
          NOCONC_BG=0.0D0
        ELSE  
          ! No adjustments made to background at night
          NOCONC_BG = NOXCONC_BG - NO2CONC_BG
        END IF
      END IF
          
      RETURN
      END SUBROUTINE GETBGDEQUIL
      
      SUBROUTINE QuadraticEquil(NO2,NO,O3)
C***********************************************************************
C        QuadraticEquil Module of the AMS/EPA Regulatory Model - AERMOD
C
C        PURPOSE: Puts NO2, NO and O3 concentrations into photo-stationary
C                 equilibrium, conserving mass of NOx and NO2 + O3
C
C        PROGRAMMER: CERC
C
C        DATE: November 2020
C
C        INPUTS:  NO2 - Input NO2 concentration in volumetric units
C                 NO - Input NO concentraiton in volumetric units
C                 O3 - Input O3 concentration involumetric units
C
C        OUTPUTS: NO2 - Equilibrium NO2 concentration in the same volumetric units
C                 NO - Equilibrium NO concentraiton in the same volumetric units
C                 O3 - Equilibrium O3 concentration in the same volumetric units
C
C        CALLED FROM: GRSM_CALC, GETBGDEQUIL
C***********************************************************************
      USE GRSMMOD
      IMPLICIT NONE
C --- Arguments      
      DOUBLE PRECISION, INTENT(INOUT)::NO2,NO,O3
C --- Local variables
      DOUBLE PRECISION :: B, C, NOX, NO2plusO3
      
C --- We have 3 equations in 3 unknowns:
C     1. Photo-stationary equilibrium
C      [NO2] = (R1/R2)*[NO]*[O3]
C     2. Conservation of NOx
C      [NO2] + [NO] = [NO2]initial + [NO]initial = [NOX]
C     3. Conservation of [NO2] + [O3]
C      [NO2] + [O3] = [NO2]initial + [O3]initial
C     These 3 lead to quadratic in [NO2]

C --- Set conserved quantities:
      NOX = NO2 + NO
      NO2plusO3 = NO2 + O3

C --- Quadratic in final [NO2]:  [NO2]*[NO2] + B*[NO2] + C = 0
C     where:
      B = -(NOX + NO2plusO3 + (R2/R1))
      C = NOX*NO2plusO3

C --- Take negative root, as positive root will be bigger than NO2plusO3 and NOX
      NO2 = 0.5*(-B - SQRT(B*B - 4.*C))

C --- Use equations 2 and 3 above to calculate [NO] and [O3]
      NO = NOX - NO2
      O3 = NO2plusO3 - NO2
      
      RETURN
      END SUBROUTINE QuadraticEquil

      FUNCTION FindInitDt()
C***********************************************************************
C        FindInitDt Module of the AMS/EPA Regulatory Model - AERMOD
C
C        PURPOSE: Find initial GRSM chemistry time step
C
C        PROGRAMMER: CERC
C
C        DATE: November 2020
C
C        INPUTS:  
C
C        OUTPUTS:
C
C        CALLED FROM: DoGRSMChem
C***********************************************************************
      USE GRSMMOD
      IMPLICIT NONE
C --- Local variables
      INTEGER::ConcLoop
      DOUBLE PRECISION:: dCdt(nPolsGRSM), TimeScale, TimeLoop 
      DOUBLE PRECISION:: FindInitDt
      CHARACTER::MODNAM*12

C --- Initialisations
      MODNAM='FindInitDt'
      
C --- Calculate the first order dreivatives
      CALL DConcsDt(CONCTEMP,dCdt)

C --- Loop through each non zero concentration and estimate the
C     'time scale' for each pollutant. We want to find the minimum non-
C     zero time period so that all pollutants change by no more than
C     CFrac*Initial Concentration.

      TimeScale = 1. ! initial value
      DO ConcLoop = 1, nPolsGRSM
        IF ( ConcTemp(ConcLoop ).GT.0.) THEN
          IF(ABS(dCdt(ConcLoop))>CFrac*ConcTemp(ConcLoop))THEN
            TimeLoop = CFrac*ABS(ConcTemp(ConcLoop)/dCdt(ConcLoop))
            IF ( TimeLoop.LT.TimeScale ) TimeScale = TimeLoop
          END IF
        END IF
      END DO

      FindInitDt = TimeScale

      RETURN
      END FUNCTION FindInitDt

      SUBROUTINE DConcsDt(CONC, dCdt)
C***********************************************************************
C        DConcsDt Module of the AMS/EPA Regulatory Model - AERMOD
C
C        PURPOSE: Calculate first-order time derivatives of each chemistry
C                 pollutant from the reaction equations
C
C        PROGRAMMER: CERC
C
C        DATE: November 2020
C
C        INPUTS:  CONC - input concentrations
C
C        OUTPUTS: dCdt - first-order time derivatives
C
C        CALLED FROM: DoGRSMChem, FindInitDt, CashKarpRK
C***********************************************************************
      USE GRSMMOD
      IMPLICIT NONE
C --- Arguments
      DOUBLE PRECISION, INTENT(OUT):: dCdt(nPolsGRSM)
      DOUBLE PRECISION, INTENT(INOUT):: CONC(nPolsGRSM)
      
C --- Local variables
      CHARACTER::MODNAM*12
CRCO 3/4/2021 Removing unused variable
C      INTEGER::I
     
C --- Initialisations
      MODNAM='DConcsDt'

C --- Calculate VALUE OF dC/dt

C --- NO2
      dCdt(nNO2)=R1*CONC(nNO)*CONC(nO3) - R2*CONC(nNO2)
C --- NO
      dCdt(nNO)=R2*CONC(nNO2) - R1*CONC(nNO)*CONC(nO3)
C --- O3
      dCdt(nO3)=R2*CONC(nNO2) - R1*CONC(nNO)*CONC(nO3)

      RETURN
      END SUBROUTINE DConcsDt

      SUBROUTINE DoGRSMChem
C***********************************************************************
C        DoGRSMChem Module of the AMS/EPA Regulatory Model - AERMOD
C
C        PURPOSE: Calculate the final concentrations after applying
C                 GRSM chemistry scheme
C
C        PROGRAMMER: CERC
C
C        DATE: November 2020
C
C        INPUTS:  
C
C        OUTPUTS:
C
C        CALLED FROM: GRSM_CALC
C***********************************************************************
      USE MAIN1
      USE GRSMMOD
      IMPLICIT NONE
       
C --- Local variables
      CHARACTER::MODNAM*12
      INTEGER, PARAMETER::MAXSTP=1000000
      INTEGER:: i,nstp
      DOUBLE PRECISION:: dt, dtnext, tLocal, 
     &         dCdt(nPolsGRSM),Cscal(nPolsGRSM)
      DOUBLE PRECISION, PARAMETER::dtmin=1.0D-6
C --- functions
      DOUBLE PRECISION::FindInitDt
!     
C --- Initialisations
      MODNAM='DoGRSMChem'
      tLocal=0.D0
      
C --- Check for negative concentrations (should never start off
C     negative but check for safety)
      DO  I=1,nPolsGRSM
        IF (CONCTEMP(I).LT.0.) CONCTEMP(I) = 0.
      END DO
      
C --- Estimate initial time step
      dt = FindInitDt()

C --- Increment in timesteps until end time reached or we hit max No. of steps
      DO nstp=1,MAXSTP

C       Calculate the derivatives
        CALL DConcsDt(CONCTEMP,dCdt)

        DO i=1,nPolsGRSM
          Cscal(i)=CONCTEMP(i)+abs(dt*dCdt(i))
        END DO
        
C       Ensure we don't go past the travel time
        IF(tLocal+dt.gt.TTRAVCHM(IREC)) dt=TTRAVCHM(IREC)-tLocal
        
C       Perform Runge-Kutta integration using adaptive timestep
        CALL AdaptiveRK(CONCTEMP,dCdt,tLocal,dt,Cscal,dtnext)
        
C       Check for negative concentrations
        DO  I=1,nPolsGRSM
          IF (CONCTEMP(I).LT.0.) CONCTEMP(I) = 0.
        END DO

C       If we've reached the travel time, return
        IF(tLocal.ge.TTRAVCHM(IREC)) RETURN
C       Otherwise set the timestep for the next iteration
        IF(dtnext.lt.dtmin) dtnext=dtmin
        dt=dtnext
      END DO
C     Reached maximum No. of time steps - return with latest concentrations
      RETURN
      END SUBROUTINE DoGRSMChem

      SUBROUTINE AdaptiveRK(CLocal,dCdt,tLocal,dttry,Cscal,dtnext)
C***********************************************************************
C        AdaptiveRK Module of the AMS/EPA Regulatory Model - AERMOD
C
C        PURPOSE: Performs Runge-Kutta integration using input timestep fisrt.
C                 If the error is too high, reduces the timestep and
C                 tries again until error is below threshold. Also tries
C                 to set a sensible timestep for the next time this
C                 routine is called based on the magnitude of the error
C
C        PROGRAMMER: CERC
C
C        DATE: November 2020
C
C        INPUTS:  CLocal - Input concentrations
C                 dCdt - Time derivatives of input concentrations
C                 tLocal - Total time so far
C                 dttry - First timestep to try
C                 Cscal - CLocal(i)+abs(dttry*dCdt(i))
C
C        OUTPUTS: tLocal - Total time so far (updated)
C                 CLocal - Output concentrations (updated)
C                 dtnext - Timestep to use next time around
C
C        CALLED FROM: DoGRSMChem
C***********************************************************************
      USE MAIN1
      USE GRSMMOD
      IMPLICIT NONE
!      
C --- Arguments
      DOUBLE PRECISION, INTENT(INOUT)::CLocal(nPolsGRSM), tLocal
      DOUBLE PRECISION, INTENT(IN)::dttry, dCdt(nPolsGRSM),
     &                              Cscal(nPolsGRSM)
      DOUBLE PRECISION, INTENT(OUT)::dtnext

C --- Local variables
      CHARACTER::MODNAM*12
      INTEGER::I
      DOUBLE PRECISION::dt, CTemp(nPolsGRSM), GrowthFac,
     &               CErr(nPolsGRSM), ErrMax, ttemp, tNew
      !P_Shrink - Determines how much we shrink dt by for the next 
      !iteration if the error is too high
      DOUBLE PRECISION, PARAMETER::P_Shrink=-0.25D0
      !P_Grow - Determines how much we increase dt by for the next
      !call to this routine
      DOUBLE PRECISION, PARAMETER::P_Grow=-0.2D0
      !MaxGrow - Maximum growth factor for dt
      DOUBLE PRECISION, PARAMETER::MaxGrow=5.D0
      !SAFETY - reduces dt slightly for the next iteration
      DOUBLE PRECISION, PARAMETER::SAFETY=0.9D0 
          
C --- Initialisations
      MODNAM = 'AdaptiveRK'
      dt=dttry
1     CALL CashKarpRK(CLocal,dCdt,dt,CTemp,CErr)
      errmax=0.

      DO i=1,nPolsGRSM
        IF (Cscal(i).gt.0.)THEN
          errmax=max(errmax,abs(CErr(i)/Cscal(i)))
        END IF
      END DO 

      errmax=errmax/CFrac
      IF(errmax.gt.1.)THEN
        !Error too large - shrink the timestep and try again
        ttemp=SAFETY*dt*(errmax**P_Shrink)
        dt=max(ttemp,0.1*dt)
        tNew=tLocal+dt
        GOTO 1
      ELSE
        !Error below threshold - calculate initial timestep for
        !next call to this routine and return
        GrowthFac = SAFETY*(errmax**P_Grow)
        IF(GrowthFac.gt.MaxGrow)THEN
          dtnext=MaxGrow*dt
        ELSE
          dtnext=GrowthFac*dt
        END IF
        tLocal=tLocal+dt
        DO i=1,nPolsGRSM
          CLocal(i)=CTemp(i)
        END DO 
        RETURN
      END IF
      END SUBROUTINE AdaptiveRK 

      SUBROUTINE CashKarpRK(C,dCdt,dt,COut,CErr)
C***********************************************************************
C        CashKarpRK Module of the AMS/EPA Regulatory Model - AERMOD
C
C        PURPOSE: Performs Runge-Kutta integration (Cash-Karp method) once
C                 using input timestep. Also calculates error.
C
C        PROGRAMMER: CERC
C
C        DATE: November 2020
C
C        INPUTS:  C - Input concentrations
C                 dCdt - Time derivatives of input concentrations
C                 dt - Timestep to use
C
C        OUTPUTS: COut - Output concentrations
C                 CErr - Error per pollutant
C
C        CALLED FROM: AdaptiveRK
C***********************************************************************
      USE GRSMMOD
      IMPLICIT NONE

C --- Arguments      
      DOUBLE PRECISION, INTENT(IN)::dt,dCdt(nPolsGRSM),
     &                              C(nPolsGRSM)
      DOUBLE PRECISION, INTENT(OUT)::CErr(nPolsGRSM),
     &                               COut(nPolsGRSM)

C --- Local variables
      CHARACTER::MODNAM*12
      INTEGER::i
      DOUBLE PRECISION:: ak2(nPolsGRSM),ak3(nPolsGRSM),
     &                   ak4(nPolsGRSM),ak5(nPolsGRSM),
     &                   ak6(nPolsGRSM),CTemp(nPolsGRSM)
CRCO 3/4/2021 removing unused variables
C      DOUBLE PRECISION::A2,A3,A4,A5,A6,B21,B31,B32,B41,B42,B43,B51,
      DOUBLE PRECISION::B21,B31,B32,B41,B42,B43,B51,
     &                  B52,B53,B54,B61,B62,B63,B64,B65,C1,C3,C4,C6,
     &                  DC1,DC3,DC4,DC5,DC6
C      PARAMETER (A2=.2D0,A3=.3D0,A4=.6D0,A5=1.D0,A6=.875D0,B21=.2D0,
      PARAMETER (B21=.2D0,
     &B31=3.D0/40.D0,B32=9.D0/40.D0,B41=.3D0,B42=-.9D0,B43=1.2D0,
     &B51=-11.D0/54.D0,B52=2.5D0,B53=-70.D0/27.D0,B54=35.D0/27.D0,
     &B61=1631.D0/55296.D0,B62=175.D0/512.D0,B63=575.D0/13824.D0,
     &B64=44275.D0/110592.D0,B65=253.D0/4096.D0,C1=37.D0/378.D0,
     &C3=250.D0/621.D0,C4=125.D0/594.D0,C6=512.D0/1771.D0,
     &DC1=C1-2825.D0/27648.D0,DC3=C3-18575.D0/48384.D0,
     &DC4=C4-13525.D0/55296.D0,DC5=-277.D0/14336.D0,DC6=C6-.25D0)
      
C --- Initialisations
      MODNAM = 'CashKarpRK'
      
      DO i=1,nPolsGRSM
        CTemp(i)=C(i)+B21*dt*dCdt(i)
        IF(CTemp(i).LT.0.) CTemp(i) = 0. !Prevent negative concs
      END DO
      CALL DConcsDt(CTemp,ak2)
      DO i=1,nPolsGRSM
        CTemp(i)=C(i)+dt*(B31*dCdt(i)+B32*ak2(i))
        IF(CTemp(i).LT.0.) CTemp(i) = 0. !Prevent negative concs
      END DO
      CALL DConcsDt(CTemp,ak3)
      DO i=1,nPolsGRSM
        CTemp(i)=C(i)+dt*(B41*dCdt(i)+B42*ak2(i)+B43*ak3(i))
        IF(CTemp(i).LT.0.) CTemp(i) = 0. !Prevent negative concs
      END DO
      CALL DConcsDt(CTemp,ak4)
      DO i=1,nPolsGRSM
        CTemp(i)=C(i)+dt*(B51*dCdt(i)+B52*ak2(i)+B53*ak3(i)+B54*ak4(i))
        IF(CTemp(i).LT.0.) CTemp(i) = 0. !Prevent negative concs
      END DO
      CALL DConcsDt(CTemp,ak5)
      DO i=1,nPolsGRSM
        CTemp(i)=C(i)+dt*(B61*dCdt(i)+B62*ak2(i)+B63*ak3(i)+B64*ak4(i)+
     &           B65*ak5(i))
        IF(CTemp(i).LT.0.) CTemp(i) = 0. !Prevent negative concs
      END DO
      CALL DConcsDt(CTemp,ak6)
      DO i=1,nPolsGRSM
        COut(i)=C(i)+dt*(C1*dCdt(i)+C3*ak3(i)+C4*ak4(i)+C6*ak6(i))
      END DO
      DO i=1,nPolsGRSM
        CErr(i)=dt*(DC1*dCdt(i)+DC3*ak3(i)+DC4*ak4(i)+DC5*ak5(i)+DC6*
     &          ak6(i))
      END DO
      RETURN
      END SUBROUTINE CashKarpRK

  
      SUBROUTINE PLUMESIZES(XARG,TIME,SIGYSIGZ_I,SIGYSIGZ_E)
C***********************************************************************
C        PLUMESIZES Module of the AMS/EPA Regulatory Model - AERMOD
C
C        PURPOSE: Gets the size of instantaneous & ensemble plumes at X
C
C        PROGRAMMER: CERC
C
C        DATE: November 2020
C
C        INPUTS:   XARG - downwind location
C                  TIME - travel time to location 
C
C        OUTPUTS: SIGYSIGZ_I,SIGYSIGZ_E - instantaneous and ensemble plume size
C
C        CALLED FROM: GRSM_CALC
C***********************************************************************
      USE MAIN1
      IMPLICIT NONE

      !Arguments
      DOUBLE PRECISION, INTENT(IN)::XARG, TIME
      DOUBLE PRECISION, INTENT(INOUT)::SIGYSIGZ_I, SIGYSIGZ_E

      !Local variables
      CHARACTER::MODNAM*12
      CHARACTER(8)::sSrcTypeName
      DOUBLE PRECISION::SIGY_NM, SIGZ, SIGY_I, SIGZ_I, SIGY_E, SIGZ_E
      DOUBLE PRECISION::SIGYINIT
CRCO 3/4/2021 removing unused variables
C      DOUBLE PRECISION::EPS, T, DUM1, DUM2, FRAN
      DOUBLE PRECISION::EPS, FRAN
C      DOUBLE PRECISION::VSEQ, HSPRIM, ZPLM, DHFOLD, SVPM, SVPM2, UPM
      DOUBLE PRECISION::VSEQ, HSPRIM, ZPLM, DHFOLD, SVPM, UPM
      DOUBLE PRECISION::TGPM, PTPM, PTP
      INTEGER::I, KITER, NDXZPL
  
      MODNAM='PLUMESIZES'

      !Initialise
      SIGYINIT=0.0D0
      SIGY_NM=0.0D0
      SIGZ=0.0D0
      SIGY_I=0.0D0
      SIGZ_I=0.0D0
      SIGY_E=0.0D0
      SIGZ_E=0.0D0


C     Set Mixing Height and Profiles for Urban Option if Needed
      IF (URBSRC(ISRC) .EQ. 'Y') THEN
C        Find Urban Area Index for This Source
         DO I = 1, NUMURB
            IF (IURBGRP(ISRC,I) .EQ. 1) THEN
               IURB = I
               EXIT
            END IF
         END DO
         IF (STABLE .OR. L_MorningTrans(IURB)) THEN
            URBSTAB = .TRUE.
            ZI = MAX( ZIURB(IURB), ZIMECH )
            GRIDSV = GRDSVU(1:MXGLVL,IURB)
            GRIDSW = GRDSWU(1:MXGLVL,IURB)
            GRIDTG = GRDTGU(1:MXGLVL,IURB)
            GRIDPT = GRDPTU(1:MXGLVL,IURB)
            OBULEN = URBOBULEN(IURB)
            USTAR  = URBUSTR(IURB)
         ELSE
            URBSTAB = .FALSE.
            ZI = ZIRUR
            GRIDSV = GRDSVR
            GRIDSW = GRDSWR
            GRIDTG = GRDTGR
            GRIDPT = GRDPTR
            OBULEN = RUROBULEN
            USTAR  = RURUSTR
         END IF
      ELSE IF (URBAN .AND. URBSRC(ISRC) .EQ. 'N') THEN
         URBSTAB = .FALSE.
         ZI = ZIRUR
         GRIDSV = GRDSVR
         GRIDSW = GRDSWR
         GRIDTG = GRDTGR
         GRIDPT = GRDPTR
         OBULEN = RUROBULEN
         USTAR  = RURUSTR
      ELSE
         URBSTAB = .FALSE.
      END IF

C     Set the Source Variables for This Source           ---   CALL SETSRC
      CALL SETSRC

C     Calculate the initial meteorological variables     ---   CALL METINI
      CALL METINI

      IF (SRCTYP(ISRC) .EQ. 'VOLUME' .OR.
     &    SRCTYP(ISRC) .EQ. 'LINE' .OR.
     &    SRCTYP(ISRC)(1:4) .EQ. 'AREA' .OR.
     &    SRCTYP(ISRC) .EQ. 'OPENPIT') THEN
         FB  = 0.0D0
         FM  = 0.0D0
         PPF = 0.0D0
         HSP = HS
         DHP  = 0.0D0
         DHP1 = 0.0D0
         DHP2 = 0.0D0
         DHP3 = 0.0D0
         DHCRIT = 0.0D0
         XFINAL = 0.0D0
         XMIXED = ZI * UAVG / SWAVG
         IF(XMIXED .LT. XFINAL) XMIXED = XFINAL
         ZMIDMX = 0.5D0 * ZI
C        Define temporary values of CENTER and SURFAC based on HS
C        to account for non-POINT sources
         CENTER = HECNTR(IREC,ISRC)
         IF( CENTER .LT. 0.1D0*ZI )THEN
            SURFAC = .TRUE.
         ELSE
            SURFAC = .FALSE.
         END IF

      ELSE IF (SRCTYP(ISRC)(1:5) .EQ. 'POINT') THEN
C        Calculate Buoyancy and Momentum Fluxes             ---   CALL FLUXES
         CALL FLUXES(VSEQ)

C        Set Wake and Building Type Switches                ---   CALL WAKFLG
C ---    NOTE:  WAKFLG sets building dimensions based on wind
C        direction at stack top.
C ---    WAKE is set to false for purposes of calculating plume volume,
C        since minimum volume is intended 
         WAKE = .FALSE.

C        Define temporary values of CENTER and SURFAC based on HS
         CENTER = HECNTR(IREC,ISRC)
         IF( CENTER .LT. 0.1D0*ZI )THEN
            SURFAC = .TRUE.
         ELSE
            SURFAC = .FALSE.
         END IF

C ---    Apply HSP for POINTCAP and POINTHOR first to avoid NOSTD option
C        overriding POINTCAP
         IF (SRCTYP(ISRC) .EQ. 'POINTCAP') THEN
C           Apply stack-tip downwash for capped stacks with VS = 0.001m/s
            HSP = HSPRIM ( US, VSEQ, HS, DS )
         ELSE IF (SRCTYP(ISRC) .EQ. 'POINTHOR') THEN
C           Do not apply stack-tip downwash for horizontal releases
            HSP = HS
         ELSE IF( NOSTD )THEN
C           No stack-tip downwash, no adjustments necessary
            HSP = HS
         ELSE
C           Make adjustments for stack-tip downwash
            HSP = HSPRIM ( US, VS, HS, DS )
         END IF

C        Calculate Distance to Final Rise                   ---   CALL DISTF
         CALL DISTF

C        Calculate the plume penetration factor             ---   CALL PENFCT
         CALL PENFCT

         IF (STABLE .OR. (UNSTAB.AND.(HS.GE.ZI))) THEN
C           Use iterative approach to stable plume rise calculations
            KITER = 0
50          ZPLM = HSP + 0.5D0 * DHFAER
            DHFOLD = DHFAER

C----       Locate index below ZPLM

            CALL LOCATE(GRIDHT, 1, MXGLVL, ZPLM, NDXZPL)

C----       Get Wind speed at ZPLM; replace UP.  Also, replace TGP,
C           vertical potential temperature gradient, if stable.

            CALL GINTRP( GRIDHT(NDXZPL), GRIDSV(NDXZPL),
     &           GRIDHT(NDXZPL+1), GRIDSV(NDXZPL+1), ZPLM, SVPM )
            CALL GINTRP( GRIDHT(NDXZPL), GRIDWS(NDXZPL),
     &           GRIDHT(NDXZPL+1), GRIDWS(NDXZPL+1), ZPLM, UPM )
            SVPM = MAX( SVPM, SVMIN, SVUMIN*UPM )
            IF( L_VECTORWS )THEN
               UPM = DSQRT( UPM*UPM + 2.0D0*SVPM*SVPM )
            ENDIF

            UPM  = MAX( UPM, WSMIN )
CRWB        Use average of stack top and midpoint wind speeds.
            UP = 0.5D0 * (US + UPM)

            CALL GINTRP( GRIDHT(NDXZPL), GRIDTG(NDXZPL),
     &           GRIDHT(NDXZPL+1), GRIDTG(NDXZPL+1), ZPLM, TGPM )
            CALL GINTRP( GRIDHT(NDXZPL), GRIDPT(NDXZPL),
     &           GRIDHT(NDXZPL+1), GRIDPT(NDXZPL+1), ZPLM, PTPM )
CRWB        Use average of stack top and midpoint temperature gradients.
            TGP = 0.5D0 * (TGS + TGPM)
            PTP = 0.5D0 * (PTS + PTPM)
            BVF = DSQRT( G * TGP / PTP )
            IF(BVF .LT. 1.0D-10) BVF = 1.0D-10
            BVPRIM  = 0.7D0 * BVF

            CALL DISTF

            KITER = KITER + 1

C           Check for convergence            
            IF(DABS((DHFOLD - DHFAER)/DHFAER) .LT. 0.01D0) GO TO 60
            
            IF(KITER .GE. 5) THEN
               DHFAER = 0.5D0 * (DHFAER + DHFOLD)
               GO TO 60
            ELSE
               GO TO 50
            END IF

60          CONTINUE

CRWB        After completing iteration, reset UP and TGP to stack top
CRWB        values for subsequent distance-dependent plume rise calcs.
            UP = US
            TGP = TGS
            PTP = PTS
            BVF = DSQRT( G * TGP / PTP )
            IF(BVF .LT. 1.0D-10) BVF = 1.0D-10
            BVPRIM  = 0.7D0 * BVF
         END IF

C        Initialize PRM_FSTREC Logical Switch for First Receptor of Loop;
         PRM_FSTREC = .TRUE.

         ZMIDMX = 0.5D0 * ZI

CRJP
CRJP     Calculate distance to uniformly mixed plume within the
CRJP     boundary layer (XMIXED) after Turner's Workbook (1970), page 7:
CRJP     distance is approximately (Zi * UAVG)/SWAVG, where UAVG
CRJP     and SWAVG are wind speed and sigma-w averaged over the depth
CRJP     between the ground and Zi (or the plume height, if higher in
CRJP     stable conditions); this height is denoted as 2 * ZMIDMX.
CRJP
CRJP     First, get refined estimate of final rise and distance to final
CRJP     rise if downwash conditions prevail.
CRJP
         XFINAL = XMAX
         DHCRIT = DHFAER
         XMIXED = ZI * UAVG / SWAVG
         IF (UNSTAB .AND. HS.LT.ZI) THEN
C           Check for XMIXED smaller than 1.25*XFINAL
            IF (XMIXED .LT. 1.25D0*XFINAL) THEN
               XFINAL = 0.8D0 * XMIXED
               CALL CBLPRD (XFINAL)
               DHCRIT = DHP1
            END IF
         END IF

      END IF

C     Define plume centroid height (CENTER)
      CALL CENTROID ( XARG )

      IF (SRCTYP(ISRC)(1:5) .EQ. 'POINT') THEN
C       Calculate the plume rise                  ---   CALL DELTAH
        CALL DELTAH ( XARG )
      ELSE                
C ---   Assign 0.0 to DHP plume rise variables for non-POINT sources
        DHP  = 0.0D0
        DHP1 = 0.0D0  
        DHP2 = 0.0D0  
        DHP3 = 0.0D0  
      END IF

C     If the atmosphere is unstable and the stack
C     top is below the mixing height, calculate
C     the CBL PDF coefficients                     ---   CALL PDF
      IF( UNSTAB  .AND.  (HS .LT. ZI) ) THEN
        CALL PDF
      END IF

C     Determine Effective Plume Height             ---   CALL HEFF
      CALL HEFF ( XARG )

C     Compute effective parameters using an
C     average through plume rise layer
      CALL IBLVAL ( XARG )

C     Call PDF & HEFF again for final CBL plume heights
      IF (UNSTAB .AND. (HS.LT.ZI) ) THEN
        CALL PDF
        CALL HEFF ( XARG )
      END IF

        
      !Calculate SIGMA_Y and SIGMA_Z
      !Find source type
      sSrcTypeName=''
      sSrcTypeName=SrcTyp(ISRC)
      !Call appropriate routine to get plume spread
      SELECT CASE(sSrcTypeName)
      CASE('POINT   ','POINTHOR','POINTCAP')
        !Get initial spread
        CALL PDIS(0.0D0)
        SIGYINIT=SY+DS/2.0
        !Get plume spread at current location   
        CALL PDIS(XARG)
        SIGY_NM =SY+DS/2.0

      CASE('LINE    ','BUOYLINE','AREA    ','AREAPOLY','AREACIRC',
     &                                                 'OPENPIT ')
        !Get initial spread
        CALL ADISY(0.0D0)
        SIGYINIT=SY+YINIT/2.0
        !Get plume spread at current location   
        CALL ADISY(XARG)
        CALL ADISZ(XARG)
        SIGY_NM =SY+YINIT/2.0
      CASE('VOLUME  ')
        !Get initial spread
        CALL VDIS(0.0D0)
        SIGYINIT=SY
        !Get plume spread at current location   
        CALL VDIS(XARG) 
        SIGY_NM =SY
      CASE DEFAULT
        !Unknown source type
        CALL ERRHDL(PATH,MODNAM,'E','614','')
      END SELECT

      !Store plume spread variables
      IF( STABLE  .OR.  (UNSTAB .AND. (HS .GE. ZI) ) )THEN
        SIGZ=SZ
      ELSEIF( UNSTAB )THEN
        SIGZ=0.5*(SZD1+SZD2)
      END IF

      SIGY_NM = MAX(SIGY_NM,0.01D0)
      SIGYINIT = MAX(SIGYINIT,0.01D0)
      SIGZ = MAX(SIGZ,0.01D0)
      
      !Calculate the epsilon term
      IF(ZI/OBULEN<=0.0D0)THEN
         !Convective turbulent dissipation
         EPS=(2.5D0/CENTER+2.0D0/ZI)*USTAR**3+0.4D0*(WSTAR**3)/ZI
      ELSE
         !Stable turbulent dissipation
         EPS=(2.5D0/CENTER+4.0D0/ZI)*USTAR**3
      END IF 

C      !Calculate the instantaneous plume spread at X
CRCO 2/25/2021 CERC determined to change the formulation for the instantaneous
C    plume spread. The original equation is equivalent to eq 3 in the 
C    2017 JAWMA paper documenting the method and evaluation. This change
C    will need to be noted somewhere in documentation (white paper, maybe). 
C      SIGY_I=(1.0D0/SIGY_NM+
C     &          (1.0D0/(SQRT((2.0D0/3.0D0)*EPS*TIME**3)+SIGYINIT)))**-1
C      SIGZ_I=(1.0D0/SIGZ+
C     &          (1.0D0/(SQRT((2.0D0/3.0D0)*EPS*TIME**3)+SIGYINIT)))**-1
     
      SIGY_I=MIN(SIGY_NM,SQRT((2.0D0/3.0D0)*EPS*TIME**3+SIGYINIT**2))
      SIGZ_I=MIN(SIGZ,SQRT((2.0D0/3.0D0)*EPS*TIME**3+SIGYINIT**2))

      !Get fraction of random kinetic energy
      IF (STABLE .OR. (UNSTAB.AND.(HS.GE.ZI))) THEN
        CALL MEANDR(UEFF, SVEFF, FRAN)
      ELSE
        CALL MEANDR(UEFFD, SVEFFD, FRAN)
      END IF
      !Get the meandered plume spread (ensemble)   
      IF(XARG/=0.0D0)THEN
        SIGY_E=1.0D0/((FRAN/(SRT2PI*XARG))+(1.0D0-FRAN)/SIGY_NM)
      ELSE
        SIGY_E=0.0
      END IF
      SIGZ_E=SIGZ 

      !Calculate return values
      SIGYSIGZ_I=SIGY_I*SIGZ_I
      SIGYSIGZ_E=SIGY_E*SIGZ_E

      RETURN
      END SUBROUTINE PLUMESIZES