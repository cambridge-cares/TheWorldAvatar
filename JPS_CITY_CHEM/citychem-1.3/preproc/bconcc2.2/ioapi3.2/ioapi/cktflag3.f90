
LOGICAL FUNCTION CKTFLAG3( FID, VID, JDATE, JTIME, TSTEP, NSTEPS, JSTEP, DELTA )    &
        RESULT( CKFLAG )

    !!***********************************************************************
    !! Version "$Id: cktflag3.f90 1 2017-06-10 18:05:20Z coats $"
    !! EDSS/Models-3 I/O API.
    !! Copyright (C) 1992-2002 MCNC and Carlie J. Coats, Jr.,
    !! (C) 2003-2013 Baron Advanced Meteorological Systems,
    !! (C) 2007-2013 Carlie J. Coats, Jr., and
    !! (C) 2014 UNC Institute for the Environment.
    !! Distributed under the GNU LESSER GENERAL PUBLIC LICENSE version 2.1
    !! See file "LGPL.txt" for conditions of use.
    !!.........................................................................
    !!  function body starts at line  79
    !!
    !!  FUNCTION:
    !!       reads and checks time step flags for file # FID and
    !!       variable VID for the time series starting at date and time
    !!       JDATE (coded YYYYDDD) and time JTIME (HHMMSS), and time interval
    !!       TSTEP (HHMMSS).
    !!       For time-independent files, JDATE:JTIME:TSTEP are ignored.
    !!       If VID is -1, checks all variables.
    !!
    !!  RETURN VALUE:  TRUE iff the operation succeeds.
    !!
    !!  PRECONDITIONS REQUIRED:  (FID,VID) valid.
    !!
    !!  REVISION  HISTORY:
    !!      prototype 5/1996 by CJC
    !!      revised   6/1999 by CJC:  OpenMP thread-safety
    !!      Modified 03/20010 by CJC: F9x changes for I/O API v3.1
    !!      Modified 01/20013 by CJC: Fix possible integer log-output overflow
    !!      Modified 02/2015 by CJC for I/O API 3.2: USE M3UTILIO, MODNCFIO,
    !!      NF_*() netCDF bindings; F90 "free" source format.
    !!***********************************************************************

    USE M3UTILIO
    USE MODNCFIO

    IMPLICIT NONE

    !!...........   INCLUDES:

    INCLUDE 'STATE3.EXT'


    !!...........   ARGUMENTS and their descriptions:

    INTEGER, INTENT(IN   ) :: FID             !  file subscript  for STATE3 arrays
    INTEGER, INTENT(IN   ) :: VID             !  vble subscripts for STATE3 arrays
    INTEGER, INTENT(IN   ) :: JDATE           !  date, formatted YYYYDDD
    INTEGER, INTENT(IN   ) :: JTIME           !  time, formatted HHMMSS
    INTEGER, INTENT(IN   ) :: TSTEP           !  time step
    INTEGER, INTENT(IN   ) :: NSTEPS          !  number of steps
    INTEGER, INTENT(  OUT) :: JSTEP           !  starting step number
    INTEGER, INTENT(  OUT) :: DELTA           !  time-step increment


    !!...........   SCRATCH LOCAL VARIABLES and their descriptions:

    INTEGER         VAR             !  subscripts for STATE3 arrays
    INTEGER         STEP            !  time step record number
    INTEGER         IERR            !  netCDF error status return
    INTEGER         DIMT( 5 )       !  corner   for NF_GET_VARA_*()
    INTEGER         DELT( 5 )       !  diagonal for NF_GET_VARA_*()
    INTEGER         FLAGS( 2,MXVARS3 )!  values array from NF_GET_VARA_*()
    INTEGER         FLAG1, FLAG2    !  test values for FLAGS
    INTEGER         VV3             !  file vble-count
    INTEGER         TS3             !  file time step
    INTEGER         DT, DT3         !  time steps (in secs)
    LOGICAL         EFLAG


    !!***********************************************************************
    !!   begin body of function  CKTFLAG3
    !!...........   Compute record number, and check availability:

    STEP = JSTEP3( JDATE, JTIME, SDATE3( FID ), STIME3( FID ), TSTEP3( FID ) )

    EFLAG = .FALSE.

!$OMP   CRITICAL( S_NC )

    IF ( STEP .LT. 0 ) THEN
        WRITE( LOGDEV,91030 )                                           &
            'Time step error reading file:  ' // FLIST3( FID ) ,        &
            'Requested date & time:    ', JDATE, JTIME ,                &
            'File starting date & time:', SDATE3( FID ), STIME3( FID ), &
            'File time step:           ', TSTEP3( FID )
        EFLAG = .TRUE.
        GO TO 999
    END IF          !  check on step number


    TS3 = TSTEP3( FID )
    IF ( TS3 .NE. 0 ) THEN

        IF ( TS3 .GT. 0 ) THEN
            JSTEP = 1 + MOD( STEP - 1, 2 )
        ELSE
            JSTEP = STEP
        END IF

        FLAG1 = JDATE
        FLAG2 = JTIME
        IF ( NSTEPS .GT. 1 ) THEN

            DT  = TIME2SEC( TSTEP )
            DT3 = TIME2SEC( TSTEP3( FID ) )
            IF ( MOD( DT, DT3 ) .NE. 0 ) THEN
                WRITE( LOGDEV,91031 )                                       &
                    'Time step error reading file:  ' // FLIST3( FID ) ,    &
                    'Requested time step:', TSTEP,                          &
                    'File      time step:', TS3
                EFLAG = .TRUE.
                GO TO 999
            END IF

        ELSE IF ( NSTEPS .EQ. 1 ) THEN

            DELTA = 1

        ELSE
                WRITE( LOGDEV,91032 )                                       &
                    'Time step error reading file:  ' // FLIST3( FID ) ,    &
                    'Requested number of steps:', NSTEPS
                EFLAG = .TRUE.
                GO TO 999

        END IF      !  if nsteps >1, or else =1, or not

    ELSE    ! tstep3( fid ) = 0


        IF ( NSTEPS .NE. 1 ) THEN

            WRITE( LOGDEV,91030 )                                       &
                'Time step error reading file:  ' // FLIST3( FID ) ,    &
                'Requested number of steps:               ', NSTEPS,    &
                'Number of steps in time independent file:', 0
            EFLAG = .TRUE.
            GO TO 999

        END IF

        JSTEP = STEP
        FLAG1 = 0
        FLAG2 = 0
    END IF

    DIMT( 1 ) = 1           !  field:  date or time
    DELT( 1 ) = 2           !  extent:  entire field
    DIMT( 3 ) = JSTEP       !  timestep dimension
    DELT( 3 ) = 1       !  extent in timestep dimension

    IF ( VID .GT. 0 ) THEN  !  reading just one variable

        DIMT( 2 ) = VID     !  variable-number
        DELT( 2 ) = 1       !  extent:  one variable

        DO  11  STEP = 1, NSTEPS

            IERR = NF_GET_VARA_INT( CDFID3( FID ), TINDX3( FID ), DIMT, DELT, FLAGS( 1,VID ) )
            IF ( IERR .EQ. 8 ) THEN     !  timestep flag not yet written

                WRITE( LOGDEV,91020 )                               &
                    'Reading ' // VLIST3( VID,FID ) //              &
                    ' -- date & time:', FLAG1, FLAG2,               &
                    'Not yet written in file ' // FLIST3( FID )
                EFLAG = .TRUE.
                GO TO 999

            ELSE IF ( IERR .NE. 0 ) THEN

                WRITE( LOGDEV,91020 )                               &
                    'Error reading netCDF time step flag for ' //   &
                    VLIST3( VID,FID ) // ' from ' // FLIST3( FID ), &
                    'Date and time', FLAG1, FLAG2,                  &
                    'netCDF error number', IERR

                EFLAG = .TRUE.
                GO TO 999

            ELSE  IF(  FLAGS( 1,VID ) .NE. FLAG1 .OR.               &
                       FLAGS( 2,VID ) .NE. FLAG2 ) THEN

                    WRITE( LOGDEV,91020 )                           &
                        'Requested date & time:', FLAG1, FLAG2,     &
                        'Variable ' // VLIST3( VID,FID ) //         &
                        ' not available in file ' // FLIST3( FID )

                    EFLAG = .TRUE.
                    GO TO 999

                END IF          !  if ierr bad or if timestep flags bad

            CALL NEXTIME( FLAG1, FLAG2, TSTEP )
            DIMT( 3 ) = DIMT( 3 ) + 1

11      CONTINUE        !  end loop on steps

    ELSE            !  reading all variables

        VV3 = MAX( 1, NVARS3( FID ) )
        DIMT( 2 ) = 1               !  initial variable-number
        DELT( 2 ) = VV3             !  extent:  all variables

        DO  33  STEP = 1, NSTEPS

            IERR = NF_GET_VARA_INT( CDFID3( FID ), TINDX3( FID ), DIMT, DELT, FLAGS )

            IF ( IERR .EQ. 8 ) THEN     !  timestep flag not yet written

                WRITE( LOGDEV,91020 )                                   &
                    'Error reading netCDF time step flag for ' //       &
                    'ALL VBLES from ' // FLIST3( FID ),                 &
                    'Date and time', FLAG1, FLAG2, 'not yet written.'
                EFLAG = .TRUE.
                GO TO 999

            ELSE IF ( IERR .NE. 0 ) THEN

                WRITE( LOGDEV,91020 )                                   &
                    'Error reading netCDF time step flag for ' //       &
                    'ALL VBLES from ' // FLIST3( FID ),                 &
                    'Date and time', FLAG1, FLAG2,                      &
                     'netCDF error number', IERR
                EFLAG = .TRUE.
                GO TO 999

            END IF          !  if ierr nonzero or not for NF_GET_VARA_*()


    !!...........   Check time step flags for all variables:

            IF ( NVARS3( FID ) .GT. 0 ) THEN

                DO  22  VAR = 1, VV3

                    IF ( FLAGS( 1,VAR ) .NE. FLAG1  .OR.            &
                         FLAGS( 2,VAR ) .NE. FLAG2  ) THEN

                        WRITE( LOGDEV,91020 )                       &
                            'Reading ALL variables -- ' //          &
                            'requested date&time:',  JDATE, JTIME , &
                            'Time step not available in file ' //   &
                            FLIST3( FID ) //                        &
                            ' for variable ' // VLIST3( VAR,FID )

                        EFLAG = .TRUE.
                        GO TO 999

                    END IF          !  if bad flag value

22              CONTINUE        !  end loop on user-variables VAR

            ELSE            !  nvars zero (structured no-user-vble time step)

                IF ( FLAGS( 1,1 ) .NE. FLAG1  .OR.                  &
                     FLAGS( 2,1 ) .NE. FLAG2  ) THEN

                    WRITE( LOGDEV,91020 )                           &
                        'Reading entire time step -- ' //           &
                        'requested date       time:', JDATE, JTIME, &
                        'Time step not available in file ' // FLIST3( FID )

                    EFLAG = .TRUE.
                    GO TO 999

                END IF          !  if bad flag value

            END IF      !  if nvars positive, or not

            CALL NEXTIME( FLAG1, FLAG2, TSTEP )
            DIMT( 3 ) = DIMT( 3 ) + 1

33      CONTINUE        !  end loop on STEP

    END IF          !  if reading one variable or many (checking flags)

999 CONTINUE

!$OMP   END CRITICAL( S_NC )

    CKFLAG = ( .NOT. EFLAG )
    RETURN

        !!******************  FORMAT  STATEMENTS   ******************************

        !!...........   Error and warning message formats..... 91xxx

91020   FORMAT ( //5X , '>>> WARNING in subroutine CKTFLAG3 <<<',   &
              3 ( /5X , A , :, I9, :, ':' , I6.6 ) // )

91030   FORMAT ( //5X , '>>> WARNING in subroutine CKTFLAG3 <<<',   &
                  /5X , A ,                                         &
                  /5X , A , I9, ':' , I6.6,                         &
                  /5X , A , I9, ':' , I6.6,                         &
                  /5X , A , I110 // )

91031   FORMAT ( //5X , '>>> WARNING in subroutine CKTFLAG3 <<<',   &
                  /5X , A ,                                         &
                  /5X , A , I10,                                    &
                  /5X , A , I10, // )

91032   FORMAT ( //5X , '>>> WARNING in subroutine CKTFLAG3 <<<',   &
                  /5X , A ,                                         &
                  /5X , A , I10, // )

END FUNCTION CKTFLAG3

