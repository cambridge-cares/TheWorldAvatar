! <rmain.f90 - A component of the City-scale
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

      subroutine RMAIN

!     The subroutine reads main input and messages filenames.
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
!    ----------------------------------------------------------------
!    11 Nov 2019  M. Karl: L152 Commented inquire/close of messfn -
!                               causes problems with gfortran 7.4.0
!    11 Nov 2019  M. Karl: L217 Commented close of infofn, see before
! ----------------------------------------------------------------------------------

      use mod_main
!MSK
      use mod_time

       implicit none

!     External function:

      integer INDXL

!     INDXL - Index function

!     Local variables

      integer I
      integer YEARV
      integer MNTHV
      integer DAYMV
      integer HOURV
      integer MINUV
      integer SECOV
      logical LEOF
      character(len=256) TXTSTR
      integer word_len
!MSK
      character(len=256) :: DUMMY


!     I      _ Index of textstring
!     YEARV  - Start machine time year
!     MNTHV  - Start machine time month
!     DAYMV  - Start machine time day of month
!     HOURV  - Start machine time hour
!     MINUV  - Start machine time minute
!     SECOV  - Start machine time seconds
!     LEOF   - If end of file then true else false
!     TXTSTR - Text string

!     Read messages filenames:

      IF (MAINFE) THEN
!       Subroutine GETDAT reads the next uncommented line
!       (line that do not start with *) from the MAINUN file.
        CALL GETDAT(MAINUN,TXTSTR,LEOF)
        IF (TXTSTR(1:1) .NE. '<') THEN
!MSK first entry is SIM-ID
!          READ (TXTSTR,1000) MESSFN
          READ (TXTSTR,1000) sim_id
        ENDIF

        CALL GETDAT(MAINUN,TXTSTR,LEOF)
        print *,'Testbench log file: ', TXTSTR

        CALL GETDAT(MAINUN,TXTSTR,LEOF)
        IF (TXTSTR(1:1) .NE. '<') THEN
          READ (TXTSTR,1000) MESSFN
        ENDIF


      ENDIF

!     Open file:
      print *,'SIM-ID', sim_id
      print *,'message file: ',MESSFN

!MSK 11.11.2019 Commented the next inquire and close 
!MSK close the already open message input-file
!MSK is not needed
!MSK      INQUIRE(FILE=MESSFN,EXIST=MESSFE, NUMBER=MESSUN)
!MSK      close(MESSUN)
!MSK end

      CALL OPOFIL(MESSFN,MESSUN,MESSFE,MESSFM)
      
      IF (MESSFE) THEN

!       Get current system date/time:
        CALL SYSDAT(YEARV,MNTHV,DAYMV,HOURV,MINUV,SECOV)

!       Write start message:
        WRITE (MESSUN,2000) (VERSION(I:I),I=1,INDXL(VERSION,' ') - 1)


        word_len = LEN_TRIM(sim_id)
        if (word_len > 0) then
          write(MESSUN,'(2A)')      &
           'RMAIN: The unique ID for this simulation: ',     &
           sim_id(1:word_len)
        else
          write(MESSUN,'(A)')     &
           'RMAIN: No unique ID specified for this simulation: '       
        endif

        
        write (MESSUN,2010) YEARV,MNTHV,DAYMV,HOURV,MINUV,SECOV 

!       Write main input data filenameand messages filenames:
        if (MAINFE) then
          write (MESSUN,2020) (MAINFN(I:I),I=1,INDEX(MAINFN,' ') - 1)
        endif
         
        write (MESSUN,2030) (MESSFN(I:I),I=1,INDEX(MESSFN,' ') - 1)

      ENDIF

!_NEST_Start:
!     Read info filenames:
      IF (MAINFE) THEN
        CALL GETDAT(MAINUN,TXTSTR,LEOF)
        IF (TXTSTR(1:1) .NE. '<') THEN
          READ (TXTSTR,1000) INFOFN
        ENDIF
!MSK start
! **** If 1 then read from files from AirQUIS/McWIND.
! **** If 2 then read from external   UM files.
! **** If 3 then read from external   TAPM files.
! Default is 1 (=McWIND as met driver)
        pimeteexternaldata = 1
        CALL GETDAT(MAINUN,TXTSTR,LEOF)
        IF (TXTSTR(1:1) .NE. '<') THEN
          READ (TXTSTR,*) pimeteexternaldata
        ENDIF
        print *,'Met driver choice: ', pimeteexternaldata
!MSK end
      ENDIF

!     Open file:

!MSK close the already open info-file
      print *,'info file: ',INFOFN
!MSK 11.11.2019 Commented the next inquire and close 
!MSK      INQUIRE(FILE=INFOFN,EXIST=INFOFE, NUMBER=INFOUN)
!MSK      close(INFOUN)
!MSK end

      CALL OPOFIL(INFOFN,INFOUN,INFOFE,INFOFM)

      IF (INFOFE) THEN

!       Get current system date/time:
        CALL SYSDAT(YEARV,MNTHV,DAYMV,HOURV,MINUV,SECOV)

!       Write start message:
        write (INFOUN,2000) (VERSION(I:I),I=1,INDXL(VERSION,' ') - 1)
        write (INFOUN,2010) YEARV,MNTHV,DAYMV,HOURV,MINUV,SECOV 

!       Write main input data and messages filenames
      
        write (INFOUN,2020) (MAINFN(I:I),I=1,INDEX(MAINFN,' ') - 1)
        write (INFOUN,2031) (INFOFN(I:I),I=1,INDEX(INFOFN,' ') - 1)

        write (INFOUN,*) ' '
        write (INFOUN,2032) 'Comp name',' Comp nr.','HOUR',     &
                           ' Tot Volume','   Tot Mass',' Avg Conc',     &
            ' EastB Mflux',' Hor. Mflux',' Vert. Mflux',' Tot Mflux'  
        write (INFOUN,2033) ' (km**3)','   (kg)   ',' (um/m**3)',     &
                           ' kg/h  ', 'kg/h  ','kg/h  ',     &
                           ' kg/h  '
!     &     ' EastB Vflux',' Hor. Vflux',' Vert. Vflux',' Tot Vflux'  
!	WRITE (INFOUN,2033) ' (km**3)',' (kg  NOx)',' (um/m**3)',
!     &                    ' (km**3)/s', ' (km**3)/s',' (km**3)/s',
!     &                    ' (km**3)/s' 

      ENDIF
!_NEST_End.

      RETURN

 1000 format (A256)

 2000 format ('RMAIN: CityChem ',256A1)
 2010 format ('RMAIN: Simulation started: ',I4.4,'.',I2.2,'.',I2.2,1X,     &
             I2.2,':',I2.2,':',I2.2)
 2020 format ('RMAIN: Main input data file = ',256A1)
 2030 format ('RMAIN: Messages ...... file = ',256A1)

!_NEST_Start:
 2031 format ('RMAIN: Info     ...... file = ',256A1)
 2032 format ('RMAIN:',A13,A10,A6,A11,2A13,4A18)
 2033 format ('RMAIN:',18X,A22,2A13,4A18)
!_NEST_End.

!     End of subroutine RMAIN

      end subroutine RMAIN
