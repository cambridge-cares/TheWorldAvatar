! <wconcm.f90 - A component of the City-scale
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

      subroutine  WCONCM

!     The subroutine write main grid concentrations to file.
!     In EPISODE the 2D surface concentration was written to ASCII or BINARY file
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
!           2017  M. Karl: In CityChem the (instantaneous) 3D concentration is 
!                          written to netCDF file.
!                          Commented CITYDELTA extension
!
! ----------------------------------------------------------------------------------

      use mod_main
      use mod_site
      use mod_time
      use mod_conc
      use mod_depo
      use mod_mete
!MSK start
      use mod_util
      use mod_grid
      use mod_writenc
!MSK end

!     Local variables

      character(len=10) TEXT1,TEXT2
      integer IC,IX_UT,IY_UT
      integer IX,IY

!     TEXT1,2 - Textstrings
!     IC      - Index of compound

!MSK start
!     Variables for netCDF output
      character(len=10) :: namefield
      character(len=10) :: unitname
      character(len=23) :: validity

      integer :: Nhh_in
      integer :: K

      integer, dimension(4,1) :: mdate

      logical :: dopacking  = .false.
      logical :: dofloat    = .false.
      logical :: domirror   = .false.

      !double precision        :: field2D(NY,NX,1)
      !double precision        :: field3D(NY,NX,NZ)
      double precision        :: field3D(NX,NY,NZ)
!MSK end


!MSK start
!MSK test of CM for missing values
       do IY = 1,NY
         do IX = 1,NX
           do IC = 1,NC

!!             If missing data then no changes

              IF (CM(IX,IY,IC) .EQ. MISS) THEN
                  print *,'WCONCM: Missing data in CM at IX IY IC) ', IX,IY,IC
                  call stopit('WSTATM: stopped because CMAVED<0')
              ENDIF
           enddo
         enddo
       enddo 

      !print *,'wconcm: write CM to file', MCONFN(1)
      !print *,'wconcm: write CM to file', CM(2,2,1)

!MSK end


      DO 100 IC = 1,NC

!MSK start
!MSK commented below because 2D concentrations are now written to netCDF

!MSK        IF (MCONFN(IC)(1:1) .EQ. ' ') THEN      
!MSK
!MSK          print *,'WCONCM: Warning no main grid concentration output file'        
!MSK        
!MSK          GOTO 100
!MSK        ENDIF


!MSK Consider the CITYDELTA insertion if main grid concentration
!MSK should be scaled from mid-layer height to 1m above ground

!_CITYDELTA_Start:
!        WRITE (TEXT2,2010) '     ug/m3'
!_CITYDELTA_End.
! Write main grid concentrations (1 m above ground level)
!_CITYDELTA_Start:
!
!       USE PROFILE AND DEPOSITION VELOCITY TO CALCULATE CONC.
!       AT 1 M ABOVE THE GROUND ACCORDING TO SIMPSON ()
!
!	  ZB1=1.0
!	  ZB2=DZ(1)/2.
!
!	  DO I=1,NX
!	    DO J=1,NY
!	      PHIDIF = (ALOG(ZB2/ZB1)
!    &              -PSIH((ZB2)/MOBUL(I,J))+PSIH(ZB1/MOBUL(I,J)))
!
!	      DEPFACTOR=1.0/(1.0+DDEPV(IC)/0.4/USTAR(I,J)*PHIDIF)
!	      DEPFACTOR=MAX(0.0,DEPFACTOR)
!	      DEPFACTOR=MIN(1.0,DEPFACTOR)
!
!	      CM(I,J,IC)=CM(I,J,IC)*DEPFACTOR
!
!	      if (i.eq.17.and.j.eq.21.and.ic.eq.4) then
!	        write(*,*)'DEP FACTOR: ',DEPFACTOR
!	      endif
!
!	    ENDDO
!	  ENDDO
!
!_CITYDELTA_End.


!     cm   - Main grid concentration field (nx,ny,nc)
!     W3DFLD expects FLD(MX,MY,MZ). Here: FLD(MX,MY,MC)
!     Therefore new routine W3DSURFFLD is used 
!           vertical index K     ==>     compound index IC

!MSK          if (MCONFE(IC)) then
!MSK
!MSK             WRITE (TEXT1,2000) MOD(YEAR,100),MNTH,DAYM,HOUR
!MSK             WRITE (TEXT2,2010) CUNIT(IC)
!MSK
!MSK             CALL W3DSURFFLD(MCONUN(IC),MCONFM(IC),IC,TEXT1,TEXT2,NX,NY,NC,  &
!MSK                    MX,MY,MC,CM)
!MSK
!MSK
             IF (MESSFE) WRITE (MESSUN,2020)   &
                      MOD(YEAR,100),MNTH,DAYM,HOUR

!MSK          endif

!MSK end

!MSK start
! Write the netCDF output file of all species,
! main grid hourly 3D concentration field [instantaneous, C()]

          if (NH1CONFE) then

              ! one time step 
              Nhh_in = 1

              ! current simulation date
              mdate(1,Nhh_in) = YEAR
              mdate(2,Nhh_in) = MNTH
              mdate(3,Nhh_in) = DAYM
              mdate(4,Nhh_in) = HOUR

              namefield = CMPND(IC)
              if ( trim(CMPND(IC)).eq."PM2.5") then
                  namefield = "PM25      "
              endif

              unitname = "ug/m^3"

              if (averaged_output) then
                 validity = 'averaged'
              else
                 validity = 'instantaneous'
              endif

              dopacking = .false.
              dofloat   = .true.
              ! domirror  = .true.
              domirror  = .false.

              ! Do neglect the outer boundaries
              ! Read C() from IX=2, IY=2, until NZ
              ! 28.01.2017: field3D(IX,IY,K) is the right orientation
              do 110 K=1,NZ
                do 120 IX = 1,NX
                  do 130 IY = 1,NY

                     field3D(IX,IY,K) = C(IC,IX+1,IY+1,K)

  130             continue
  120           continue
  110         continue

              call writeconcfield(NH1CONFN, namefield,unitname,field3D(1:NX,1:NY,1:NZ), NX, NY, NZ,   &
                                 Nhh_in, mdate, validity, dopacking, dofloat, domirror)

          endif
!MSK end

  100 CONTINUE

      RETURN

 2000 FORMAT (4I2.2,2X)
 2010 FORMAT (A10)
 2020 FORMAT ('WCONCM: Write main grid concentrations  for time ',  &
             4I2.2)
 2100 FORMAT ('WCONCM: Main grid concentrations  for hour ',  &
             4I2.2)

!     End of subroutine WCONCM

      end subroutine wconcm
