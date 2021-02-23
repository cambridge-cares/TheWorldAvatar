! <tspsrc.f90 - A component of the City-scale
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

      subroutine TSPSRC(I)

! *** The subroutine reads, calculates and writes point sources subgrid
! *** model data for one timestep.
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
!           2017  M. Karl: No treatment of point sources if PSRCMTYPE == 0
!                          Plume segments are now written every hour (routine WNEWP)
!    15 Feb 2018  M. Karl: L120 Do not read point sources at start of simulation
!
! ----------------------------------------------------------------------------------

      use mod_util
      use mod_main
      use mod_site
      use mod_time
      use mod_mete
      use mod_conc
      use mod_psrc

      implicit none

      integer :: I

! *** I - Indicator of pre (0) or post (1) processing

! *** Local variables

!MSK      LOGICAL ATDATE
!MSK      LOGICAL ATTIME
!MSK      LOGICAL BFTIME

! *** ATDATE - If at     given date then true else false
! *** ATTIME - If at     given time then true else false
! *** BFTIME - If before given time then true else false

! *** Read Haifa emission data specific variables

      integer :: NEXTUN
      integer :: IUN
      integer :: I1
      integer :: I2
      character(LEN=256) TXTSTR
      character(LEN=3) T1
      real    :: Q1
      logical :: ISOPEN


!!!===============================================
!!!  for moving point source, by Kang Dec.10,2019
!!   further changed for circular moving route @ Jan 21, 2020 by Kang  
!! 
         real :: dis_point,dir_point,ddx_point,ddy_point,dt_point !!! used for calculating the point source new location
         real :: f0,f1
         logical :: run_cpsrc  !!! if run cprsc.  if any point source moves, it is true.
         logical :: nlast_moving !! if last timestep for moving, if yes, update the final point source location
         integer :: IQ

                
         real :: dcir_ang, dR_ship, dcir_ang_virtual, dr_ship_virtual 
         real :: dxr_ship_virtual,dyr_ship_virtual,dtheta,dr_ship_real,dxr_ship,dyr_ship
!!!===============================================
          

!!     
!MSK start
! *** IF ZERO POINT SOURCES LEAVE
       IF (.not.PSRCFE) RETURN
!MSK end

! *** Perform pre- or postprocessing calculations

      IF (I .EQ. 0) GOTO 100
      IF (I .EQ. 1) GOTO 200

  100 CONTINUE

!$$$$$$         If(ATTIME(BDAT)) then 
!$$$$$$          open(unit=123, file='../OUTPUT/point_location.txt')
!$$$$$$          write(123,*) 'dd_point(1), QX,QY,QX1,QY1,ITS, dR_ship,dcir_ang,dr_ship_virtual,', &
!$$$$$$               'dxr_ship_virtual,dyr_ship_virtual,dr_ship_real,dxr_ship,dyr_ship'
!$$$$$$         endif
! *** Preprocessing calculations

! *** Beginning of simulation period?

      IF (ATTIME(BDAT)) THEN

! ***    Open all point sources files
           call oppsrc
! ***    Read old plume segments data
           call roldp

      ENDIF
!$$$$$$              print *,'tspsrc: after read oldplume, CHTIME=',CHTIME, ', BFTIME(CHTIME)=',BFTIME(CHTIME),&
!$$$$$$              'RONCE=',RONCE,', ATTIME(BDAT)=',ATTIME(BDAT), ', BDAT=',BDAT, 'ITS=',ITS, ', NTS=',NTS


!!!==============================================
!!! orignal code !!!!
! *** Changes has occurred to point sources data?
!MSK start
!      IF (.NOT. ATTIME(BDAT)) THEN
!        IF (.NOT. BFTIME(CHTIME) .OR. .NOT. RONCE) THEN
! ***     Read new point sources data
             !print *,'tspsrc: read new psrc data'
!             call rpsrc
! ***     Calculate point sources data
!            IF (RUNOK) call cpsrc
!        ENDIF
! *** First timestep in current simulation period?
!        IF (ITS .EQ. 1) THEN
         !print *,'tspsrc: before cpsrc'
! ***    Calculate point sources data
!            IF (RUNOK)  call cpsrc
!        ENDIF
!      ENDIF
!MSK end
!!! end of orignal code !!!
!!!==============================================

! *** Changes has occurred to point sources data?
!MSK start
!      IF (.NOT. ATTIME(BDAT)) THEN  !!! disable this to read point source input at ITS=1
        IF (.NOT. BFTIME(CHTIME) .OR. .NOT. RONCE) THEN

! ***     Read new point sources data
             print *,'tspsrc: read new psrc data'
             call rpsrc
 

!!!===============================================
!!!  for moving point source, by Kang Dec.10,2019
!!! update the new location when point source is moving!! 
!!  

          point_moving= .False.
          t_point=0.0   !!!! simulation running time in current simluation period @ s (0-3600s)
          
        do IQ=1,CHANQ
                        
        if (vel_point(IQ) .gt. 0.0 ) then   
          !!! only when point is moving (vel>0 and within moving time period). t_mpoint1 and t_mpoint2 are the start and end time of point if moving within 1hour (0-3600)
          point_moving= .True.  

        endif 
          
          QX1(IQ)=QX(IQ)
          QY1(IQ)=QY(IQ)
        enddo

!$$$$$$            write(123,*) dd_point(1), QX(1), QY(1), QX1(1),QY1(1), ITS, dR_ship,dcir_ang/3.141592654*180 &
!$$$$$$                         ,dr_ship_virtual,dxr_ship_virtual,dyr_ship_virtual,dr_ship_real,dxr_ship,dyr_ship

        if (point_moving) then 
         

          
     !    point_moving= .False.
         
        do IQ=1,CHANQ
  
 
          
        if (vel_point(IQ).gt.0.0 .and. (t_point+DT).ge.t_mpoint1(IQ) .and. t_point.le.t_mpoint2(IQ)) then   !!! only when point is moving (vel>0 and within moving time period)
          point_moving= .True.  
       !    QX(IQ)=QX1(IQ)
       !    QY(IQ)=QY1(IQ)
          dt_point=DT

        if (cir_ang(IQ) .eq. 0.0) then  !! straight line 

         if ( t_point .lt. t_mpoint1(IQ))     dt_point=t_point+DT-t_mpoint1(IQ)  !! if point is close to start.
         if ((t_point+DT) .gt. t_mpoint2(IQ)) dt_point=t_mpoint2(IQ)-t_point  !! if point is close to stop.     
                          
          dis_point=vel_point(IQ)*dt_point   !!! distance for point moving in this DT or dt_point
          
          dir_point=dd_point(IQ)*3.141592654/180
          
          ddx_point=dis_point*sin(dir_point)  !!! check sin() and cos() for degree or for rad?? 
          ddy_point=dis_point*cos(dir_point)

          QX(IQ)=QX1(IQ)+ddx_point/2   !! virtual location of point during this DT if moving
          QY(IQ)=QY1(IQ)+ddy_point/2          

          if (t_point .lt. t_mpoint1(IQ)) then
            f1=dt_point/DT   !! time fraction for moving
            f0=1.0-f1        !! time fraction before moving
              
            QX(IQ)=QX1(IQ)+ ddx_point*0.5*f1/(f1+f0)
            QY(IQ)=QY1(IQ)+ ddy_point*0.5*f1/(f1+f0)
          endif

          if ((t_point+DT) .gt. t_mpoint2(IQ)) then
            f1=dt_point/DT   !! time fraction for moving
            f0=1.0-f1        !! time fraction after moving
              
            QX(IQ)=QX1(IQ)+ ddx_point - ddx_point*0.5*f1/(f1+f0)
            QY(IQ)=QY1(IQ)+ ddy_point - ddy_point*0.5*f1/(f1+f0)
          endif
          
   
          QX1(IQ)=QX1(IQ)+ddx_point   !! real location of point after this DT if moving
          QY1(IQ)=QY1(IQ)+ddy_point
        
          endif !!! end of straight line (cirang=0)
 


          if (cir_ang(IQ) .ne. 0.0) then
           dcir_ang=cir_ang(IQ)/NTS*3.141592654/180  !!! angle that ship is moving around for DT.      >0 clockwise, <0, counter-clockwise.  Should <90
           dR_ship=vel_point(IQ)*DT/abs(dcir_ang)   !!! radius that ship is moving around
           
           dcir_ang_virtual=dcir_ang/2  !!! virtual angle that ship is sitting at for DT 

           dr_ship_virtual=2*dR_ship*abs(sin(dcir_ang_virtual/2)) 
           dxr_ship_virtual=dr_ship_virtual*cos(dcir_ang_virtual/2)  !! x of ship virtual position at circular coordinate based on orig @ ship start position. Always >=0
           dyr_ship_virtual=dr_ship_virtual*sin(-dcir_ang_virtual/2) !! >0, counterclockwise;  <0, clockwise
          
           dtheta=(90-dd_point(IQ))*3.141592654/180 
           QX(IQ)=QX1(IQ) + cos(dtheta)*dxr_ship_virtual - sin(dtheta)*dyr_ship_virtual   !!! x of ship virtual position for DT
           QY(IQ)=QY1(IQ) + sin(dtheta)*dxr_ship_virtual + cos(dtheta)*dyr_ship_virtual 


           
           dr_ship_real=2*dR_ship*abs(sin(dcir_ang/2)) 
           dxr_ship=dr_ship_real*cos(dcir_ang/2)  !! x of ship real position at circular coordinate based on orig @ ship start position
           dyr_ship=dr_ship_real*sin(-dcir_ang/2)
          
           QX1(IQ)=QX1(IQ) + cos(dtheta)*dxr_ship - sin(dtheta)*dyr_ship   !!! x of ship real position for DT
           QY1(IQ)=QY1(IQ) + sin(dtheta)*dxr_ship + cos(dtheta)*dyr_ship 

           dd_point(IQ)=dd_point(IQ)+cir_ang(IQ)/NTS
           endif  !!! end of cirang >0           

        endif   !!! end of if for calculating QX and QY
        
        enddo
        
         t_point=t_point+DT

         
        endif !!! end of if for point_moving 

!$$$$$$          print *,'tspsrc 1: IQ= 1, vel_point=',vel_point(1),', dd_point=',dd_point(1), &
!$$$$$$                ',t_mpoint1=',t_mpoint1(1),',t_mpoint2=',t_mpoint2(1),'cir_ang(IQ)=',cir_ang(1)   
!$$$$$$          print *,'tspsrc 1: IQ= 1, point_moving=',point_moving,', t_point=',t_point,', dt=',DT, &
!$$$$$$                ',QX1=',QX1(1),',QY1=',QY1(1),',QX=',QX(1),',QY=',QY(1),', ITS=',ITS    
!$$$$$$ 
!$$$$$$            write(123,*) dd_point(1), QX(1), QY(1), QX1(1),QY1(1), ITS, dR_ship,dcir_ang/3.141592654*180 &
!$$$$$$                         ,dr_ship_virtual,dxr_ship_virtual,dyr_ship_virtual,dr_ship_real,dxr_ship,dyr_ship
                            
! ***     Calculate point sources data
            IF (RUNOK) call cpsrc
              
         do IQ=1,CHANQ
           QX(IQ)=QX1(IQ)  !!! update the location to real location after cpsrc.
           QY(IQ)=QY1(IQ)
         enddo
         
        ENDIF


! *** First timestep in current simulation period?

  !      IF (ITS .EQ. 1) THEN
         !print *,'tspsrc: before cpsrc'
! ***    Calculate point sources data
  !          IF (RUNOK)  call cpsrc

  !      ENDIF
!      ENDIF  
!MSK end

!$$$$$$          print *,'tspsrc: before 2nd moving cycle, point_moving=',point_moving,& 
!$$$$$$                ', CHANQ=',CHANQ, 'run_cpsrc=',run_cpsrc, 'ITS=',ITS, 'DT=',DT, 'runok=',RUNOK 
                        
        if (ITS .gt. 1 ) then 
          if (point_moving) then
                   
      !   point_moving= .False.
          run_cpsrc = .False.    

          nlast_moving =.False.
                    
          do IQ=1,CHANQ           !!! if point source stops after last step. if yes, then point source location needs to be updated to final value in cpsrc.
        !     nmoving_mpoint(IQ)=0.0  
             if (vel_point(IQ).gt.0.0 .and. (t_point-DT).lt.t_mpoint2(IQ) .and. t_point.ge.t_mpoint2(IQ)) then
                nlast_moving = .True.
        !        nmoving_mpoint(IQ)=1.0    !!! if one point source is moving
             endif  
          enddo
          
        do IQ=1,CHANQ
 
!$$$$$$          print *,'tspsrc33: before 2nd moving cycle, point_moving=',point_moving,& 
!$$$$$$                ', CHANQ=',CHANQ, 'run_cpsrc=',run_cpsrc, 'ITS=',ITS, 'runok=',RUNOK,", nlast_moving=",nlast_moving         

         
        if (vel_point(IQ).gt.0.0 .and. (t_point+DT).ge.t_mpoint1(IQ) .and. t_point.le.t_mpoint2(IQ)) then   !!! only when point is moving (vel>0 and within moving time period)
          run_cpsrc = .True.  
       !   nmoving_mpoint(IQ)=1 
       !    QX(IQ)=QX1(IQ)
       !    QY(IQ)=QY1(IQ)
          dt_point=DT 
   
          if (cir_ang(IQ) .eq. 0.0) then   !! start of straight line

         if ( t_point .lt. t_mpoint1(IQ))     dt_point=t_point+DT-t_mpoint1(IQ)  !! if point is close to start.
         if ((t_point+DT) .gt. t_mpoint2(IQ)) dt_point=t_mpoint2(IQ)-t_point  !! if point is close to stop.     
                          
          dis_point=vel_point(IQ)*dt_point   !!! distance for point moving in this DT or dt_point
          
          dir_point=dd_point(IQ)*3.141592654/180
          
          ddx_point=dis_point*sin(dir_point)  !!! check sin() and cos() for degree or for rad?? 
          ddy_point=dis_point*cos(dir_point)

          QX(IQ)=QX1(IQ)+ddx_point/2   !! virtual location of point during this DT if moving
          QY(IQ)=QY1(IQ)+ddy_point/2          

          if (t_point .lt. t_mpoint1(IQ)) then
            f1=dt_point/DT   !! time fraction for moving
            f0=1.0-f1        !! time fraction before moving
              
            QX(IQ)=QX1(IQ)+ ddx_point*0.5*f1/(f1+f0)
            QY(IQ)=QY1(IQ)+ ddy_point*0.5*f1/(f1+f0)
          endif

          if ((t_point+DT) .gt. t_mpoint2(IQ)) then
            f1=dt_point/DT   !! time fraction for moving
            f0=1.0-f1        !! time fraction after moving
              
            QX(IQ)=QX1(IQ)+ ddx_point - ddx_point*0.5*f1/(f1+f0)
            QY(IQ)=QY1(IQ)+ ddy_point - ddy_point*0.5*f1/(f1+f0)
          endif
          
   
          QX1(IQ)=QX1(IQ)+ddx_point   !! real location of point after this DT if moving
          QY1(IQ)=QY1(IQ)+ddy_point
          
          endif !!! end of straight line (cir_ang = 0)
          


          if (cir_ang(IQ) .ne. 0.0) then
           dcir_ang=cir_ang(IQ)/NTS*3.141592654/180  !!! angle that ship is moving around for DT.    should < 90, otherwise, may need to think about the location calacuation equations.
           dR_ship=vel_point(IQ)*DT/abs(dcir_ang)   !!! radius that ship is moving around
           
           dcir_ang_virtual=dcir_ang/2  !!! virtual angle that ship is sitting at for DT 

           dr_ship_virtual=2*dR_ship*abs(sin(dcir_ang_virtual/2)) 
           dxr_ship_virtual=dr_ship_virtual*cos(dcir_ang_virtual/2)  !! x of ship virtual position at circular coordinate based on orig @ ship start position
           dyr_ship_virtual=dr_ship_virtual*sin(-dcir_ang_virtual/2)
           


           dtheta=(90-dd_point(IQ))*3.141592654/180 
           QX(IQ)=QX1(IQ) + cos(dtheta)*dxr_ship_virtual - sin(dtheta)*dyr_ship_virtual   !!! x of ship virtual position for DT
           QY(IQ)=QY1(IQ) + sin(dtheta)*dxr_ship_virtual + cos(dtheta)*dyr_ship_virtual 


           
           dr_ship_real=2*dR_ship*abs(sin(dcir_ang/2)) 
           dxr_ship=dr_ship_real*cos(dcir_ang/2)  !! x of ship real position at circular coordinate based on orig @ ship start position
           dyr_ship=dr_ship_real*sin(-dcir_ang/2)
          
           QX1(IQ)=QX1(IQ) + cos(dtheta)*dxr_ship - sin(dtheta)*dyr_ship   !!! x of ship real position for DT
           QY1(IQ)=QY1(IQ) + sin(dtheta)*dxr_ship + cos(dtheta)*dyr_ship 

           dd_point(IQ)=dd_point(IQ)+cir_ang(IQ)/NTS
           endif  !!! end of cirang >0           

        endif   !!! end of if for calculating QX and QY

     !   
      !  if(t_point .gt. t_mpoint2(IQ)  point_moving = .False.   

        enddo   !!! end of IQ cycle


                 
           
!$$$$$$          print *,'tspsrc44: before 2nd moving cycle, point_moving=',point_moving,& 
!$$$$$$                ', CHANQ=',CHANQ, 'run_cpsrc=',run_cpsrc, 'ITS=',ITS, 'runok=',RUNOK         

! ***     Calculate point sources data
         
            if (RUNOK ) then
              if (run_cpsrc .or. nlast_moving) call cpsrc
            endif           

!$$$$$$          print *,'tspsrc 2: IQ= 1, run_cpsrc=',run_cpsrc,', t_point=',t_point,', dt=',DT, &
!$$$$$$                ',QX1=',QX1(1),',QY1=',QY1(1),',QX=',QX(1),',QY=',QY(1),', ITS=',ITS, &
!$$$$$$                ', sin(dd)=',sin(dir_point),', dis_point=',dis_point, 'dt_point=',dt_point, &
!$$$$$$                ', nlast_moving=',nlast_moving,'dcir_ang=',dcir_ang,'dR_ship=',dR_ship, &
!$$$$$$                 'dr_ship_virtual=',dr_ship_virtual,'dr_ship=',dr_ship 
               
         t_point=t_point+DT

!$$$$$$            write(123,*) dd_point(1), QX(1), QY(1), QX1(1),QY1(1), ITS, dR_ship,dcir_ang/3.141592654*180 &
!$$$$$$                         ,dr_ship_virtual,dxr_ship_virtual,dyr_ship_virtual,dr_ship_real,dxr_ship,dyr_ship
        
          do IQ=1,CHANQ        
           QX(IQ)=QX1(IQ)  !!! update the location to real location after cpsrc.
           QY(IQ)=QY1(IQ)
          enddo
                            
        endif !!! end of if for ITS>1 
        endif
               

!!!===============================================
!!!  for moving point source, by Kang Dec.10,2019
!!! end of updating the new location when point source is moving!! 
!!!===============================================




!MSK start
! No treatment of point sources if PSRCMTYPE == 0
      if (PSRCMTYPE .eq. 0) then
        goto 200
      endif
!MSK end


      IF (PSRCMTYPE .GE. 2) THEN

! ***    Beginning of simulation period?
         IF (ATTIME(BDAT)) THEN
! ***       Initialize INPUFF model
!MSK            IF (NQ .GE. 1) CALL INI_INPUFF
              IF (NQ .GE. 1) THEN
                print *,'INPUFF currently not available. STOP'
                stop
              ENDIF 
        ENDIF

      ENDIF


!MSK start
      IF (PSRCMTYPE .EQ. 1) THEN
!MSK end

!MSK start
! ***    Beginning of simulation period?
         IF (ATTIME(BDAT)) THEN

            print *,'tspsrc: newpfn ',NEWPFN
            call opofil(NEWPFN,NEWPUN,NEWPFE,NEWPFM)

         ENDIF
!MSK end

! ***    Run the EPISODE segmented plume model for current timestep
         IF (RUNOK) THEN

! ***       Perform advection and diffusion for all plume segments
            call psadif

            !print *,'tspsrc: before psgene'
! ***       Generate new plume segments
            call psgene

! ***       Transfer deletable plume segments to the grid model
            call psgrid

! ***       Delete all plume segments with no mass
            call psdele

         ENDIF

      ENDIF

! *** Finished with preprocessing
      RETURN

  200 CONTINUE

! *** Postprocessing calculations

!MSK start
! No treatment of point sources if PSRCMTYPE == 0
      if (PSRCMTYPE .eq. 0) then
        goto 300
      endif
!MSK end

! *** If run model not ok then return

      IF (PSRCMTYPE .GE. 2) THEN
      
! ***   Last timestep in current simulation period?
        IF (ITS .EQ. NTS) THEN
! ***       Run the INPUFF model for the current simulation period
            IF (RUNOK) THEN
!MSK                IF (NQ .GE. 1) CALL RUN_INPUFF(PSRCAI)
              IF (NQ .GE. 1) THEN
                print *,'INPUFF currently not available. STOP'
                stop
              ENDIF

                IF (MESSFE) THEN
                    WRITE (MESSUN,2000) NQ
                ENDIF
            ENDIF
        ENDIF

      ENDIF

!MSK start
      IF(PSRCMTYPE .EQ. 1) THEN
!MSK end

! ***   Now the segmented plume model is considered
        IF (RUNOK) THEN
! ***      Calculate plume segments depositions
           call psdepo

! ***      Calculate plume segments radioactive decays
           call psradi




!!!===============================================
!!! for averaged receptor concentration, by Kang Dec.23,2019
!!! need to be double check 
!!!===============================================
            if(averaged_output .and. ITS .ne. NTS) then
! ***         Calculate plume segments concentrations and depositions
! ***         ... in main grid and subgrid cells
              call csubpx(PSRCAI,PSRCAI,PSRCAI,TSIMRES)

! ***         ... in the irregular receptor points
              call csubpr(PSRCAI,PSRCAI,PSRCAI,TSIMRES)

! ***         ... in the line source associated receptor points
              call csubpl(PSRCAI,PSRCAI,PSRCAI,TSIMRES)
             endif


! ***      Last timestep in current simulation period?
           IF (ITS .eq. NTS) THEN
!!!===============================================             
              IF (MESSFE) THEN
                 WRITE (MESSUN,*)
                 WRITE (MESSUN,2000) NQ
                 WRITE (MESSUN,2010) NP
              ENDIF
!MSK start
! ***      Write new plume segments
              call wnewp
!MSK end

! ***         Calculate plume segments concentrations and depositions
! ***         ... in main grid and subgrid cells
              call csubpx(PSRCAI,PSRCAI,PSRCAI,TSIMRES)

! ***         ... in the irregular receptor points
              call csubpr(PSRCAI,PSRCAI,PSRCAI,TSIMRES)

! ***         ... in the line source associated receptor points
              call csubpl(PSRCAI,PSRCAI,PSRCAI,TSIMRES)

           ENDIF
        ENDIF

! ***   End of simulation period?
        IF (ATDATE(EDAT) .AND. ISH .EQ. NSH .AND. ITS .EQ. NTS) THEN
! ***      Write new plume segments
!MSK plume segments are now written every hour
!MSK            CALL WNEWP
!MSK close the plume segment run file
            call clofil(NEWPFN,NEWPUN,NEWPFE,NEWPFM)

        ENDIF

      ENDIF  ! ELSEIF(PSRCMTYPE .EQ. 1) i.e. Segmented Plume treatment.

  300 CONTINUE

! *** End of simulation period?
      IF (ATDATE(EDAT) .AND. ISH .EQ. NSH .AND. ITS .EQ. NTS) THEN
! ***    Close all point sources files
          call clpsrc
      ENDIF

! *** Finished with postprocessing

      RETURN

 2000 format('TSPSRC: Calculating on ',I6,' point sources')
 2010 format('TSPSRC: Calculating on ',I8,' plume segments')

! *** End of subroutine TSPSRC

      end subroutine tspsrc
