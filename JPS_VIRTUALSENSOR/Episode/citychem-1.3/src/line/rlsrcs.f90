! <rlsrcs.f90 - A component of the City-scale
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

      subroutine RLSRCS

! The subroutine reads static line sources data from file.
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
!           2016  M. Karl: modified handling of the line source emission reading
!                          Width of road QLW set to 2 times (lane) width WV
!
! ----------------------------------------------------------------------------------

      use mod_util
      use mod_main
      use mod_site
      use mod_time
      use mod_mete
      use mod_conc
      use mod_depo
      use mod_lsrc

      implicit none

! Local variables

      real :: RDV
      real :: RMAXV
      real :: RMINV
      real :: W1V
      real :: W2V
      real :: X1V
      real :: X2V
      real :: Y1V
      real :: Y2V
      real :: Z1V
      real :: Z2V
      integer :: IQL
      integer :: IQLV
      logical :: LEOF
!MSK start
      real :: WV
      character(len=50) :: COMMENT
      integer :: QLRIV
!MSK end

! RDV   - Receptor distance
! RMAXV - Receptor maximum distance
! RMINV - Receptor minimum distance
! W1V   - Line source width lane 1
! W2V   - Line source width lane 2
! X1V   - Line source start point x-coordinate
! X2V   - Line source end   point x-coordinate
! Y1V   - Line source start point y-coordinate
! Y2V   - Line source end   point y-coordinate
! Z1V   - Line source start point z-coordinate
! Z2V   - Line source end   point z-coordinate
! IQL   - Line source index
! IQLV  - Line source index
! LEOF  - If end of file then true else false

! Define number of line source lanes

!MSK start
!MSK line source emission files contain only data for one lane
!MSK assume that second lane has same emission as first lane
      NQLL = 2
      MQLL = 2

! Open file
      CALL OPIFIL(LSRSFN,LSRSUN,LSRSFE,LSRSFM)


! Check if file exists

      IF (LSRSFE) THEN

! Read static line sources data

          IQL = 0

!MSK start
!MSK first readin needed to calculate NQL based on the dynamic line source data
!          CALL NXTDAT(LSRSUN,LEOF)
!MSK this reads the line source metadata file
!MSK 8 comment lines
          READ (LSRSUN,*,END=199) COMMENT
          READ (LSRSUN,*,END=199) COMMENT
          READ (LSRSUN,*,END=199) COMMENT
          READ (LSRSUN,*,END=199) COMMENT

          READ (LSRSUN,*,END=199) COMMENT
          READ (LSRSUN,*,END=199) COMMENT
          READ (LSRSUN,*,END=199) COMMENT
          READ (LSRSUN,*,END=199) COMMENT

  100     CONTINUE

! new format of metadata
!  IQLV   X1V    X2V     Y1V     Y2V     Z1V     Z2V      WV   RMAXV   INDICES
!
!MSK          READ (LSRSUN,*,END = 199) IQLV,X1V,Y1V,Z1V,X2V,Y2V,Z2V,       &
!MSK                                   W1V,W2V,RMINV,RMAXV,RDV

          READ (LSRSUN,*,END = 199) IQLV, X1V, X2V, Y1V, Y2V, Z1V, Z2V,     &
                                    WV, RMAXV, QLRIV

!MSK end

          IQL = IQL + 1


! Goto next line source

          GOTO 100

  199     CONTINUE


! Define total number of line sources

          NQL = IQL

          IF (NQL .GT. 0) THEN

              IF (.NOT. ALLOCATED(QL))        ALLOCATE(QL(NC,NQL,NQLL))
              IF (.NOT. ALLOCATED(QLL))       ALLOCATE(QLL(NQL))
              IF (.NOT. ALLOCATED(QLRMAX))    ALLOCATE(QLRMAX(NQL))
              IF (.NOT. ALLOCATED(QLRMIN))    ALLOCATE(QLRMIN(NQL))
              IF (.NOT. ALLOCATED(QLSW0))     ALLOCATE(QLSW0(NQL))
              IF (.NOT. ALLOCATED(QLWREDFAC)) ALLOCATE(QLWREDFAC(NQL))
              IF (.NOT. ALLOCATED(QLX))       ALLOCATE(QLX(NQL,2))
              IF (.NOT. ALLOCATED(QLY))       ALLOCATE(QLY(NQL,2))
              IF (.NOT. ALLOCATED(QLZ))       ALLOCATE(QLZ(NQL,2))
              IF (.NOT. ALLOCATED(QLW))       ALLOCATE(QLW(NQL))
              IF (.NOT. ALLOCATED(QLTMAX))    ALLOCATE(QLTMAX(NQL))
!MSK start
              IF (.NOT. ALLOCATED(QLRI))       ALLOCATE(QLRI(NQL))
!MSK: to allocate only after NRL is calculated in clsrcs.for
!              IF (.NOT. ALLOCATED(CRL))       ALLOCATE(CRL(NC,2*NQL))
!              IF (.NOT. ALLOCATED(DDEPRL))    ALLOCATE(DDEPRL(NC,2*NQL))
!              IF (.NOT. ALLOCATED(WDEPRL))    ALLOCATE(WDEPRL(NC,2*NQL))
              IF (.NOT. ALLOCATED(QLRD))      ALLOCATE(QLRD(2*NQL))
              IF (.NOT. ALLOCATED(XRL))       ALLOCATE(XRL(2*NQL))
              IF (.NOT. ALLOCATED(YRL))       ALLOCATE(YRL(2*NQL))
              IF (.NOT. ALLOCATED(ZRL))       ALLOCATE(ZRL(2*NQL))
!MSK end

          ENDIF

          MQL = NQL

! Rewind file

          REWIND (LSRSUN)

! Read static line sources data again

          IQL = 0

!MSK start
!          CALL NXTDAT(LSRSUN,LEOF)
!MSK 8 comment lines
          READ (LSRSUN,*,END=999) COMMENT
          READ (LSRSUN,*,END=999) COMMENT
          READ (LSRSUN,*,END=999) COMMENT
          READ (LSRSUN,*,END=999) COMMENT

          READ (LSRSUN,*,END=999) COMMENT
          READ (LSRSUN,*,END=999) COMMENT
          READ (LSRSUN,*,END=999) COMMENT
          READ (LSRSUN,*,END=999) COMMENT


  200     CONTINUE

! new format of metadata
!  IQLV   X1V    X2V     Y1V     Y2V     Z1V     Z2V      WV   RMAXV   INDICES
!
!MSK          READ (LSRSUN,*,END = 999) IQLV,X1V,Y1V,Z1V,X2V,Y2V,Z2V,       &
!MSK                                   W1V,W2V,RMINV,RMAXV,RDV

          READ (LSRSUN,*,END = 999) IQLV, X1V, X2V, Y1V, Y2V, Z1V, Z2V,     &
                                    WV, RMAXV, QLRIV

          IQL = IQL + 1

! Start and end coordinates of the line source

          QLX(IQL,1) = X1V
          QLY(IQL,1) = Y1V
          QLZ(IQL,1) = Z1V
          QLX(IQL,2) = X2V
          QLY(IQL,2) = Y2V
          QLZ(IQL,2) = Z2V

! Define width and influence sone for the line source

!MSK start

! Width of road QLW
!
!MSK          QLW(IQL)    = W1V + W2V
!MSK          QLW(IQL)    = WV !guess
          QLW(IQL)    = 2.*WV 
!MSK          QLRMIN(IQL) = RMINV

! Absolute maximum value for the "maximum influence distance" (in meters) QLRMAX
!
!MSK start: Maximum influence distance should not be greater than DX/2
!MSK          QLRMAX(IQL) = RMAXV
!MSK          QLRMAX(IQL) = min(RMAXV, 500.)
          QLRMAX(IQL) = min(RMAXV, DX/2.)

! Create two receptor points on each side of the line source
!
!MSK: unfortunately RDV is not in the Metadata 
!MSK          QLRD(2*IQL-1) = RDV
!MSK          QLRD(2*IQL  ) = RDV
!
! Line source index
          QLRI(IQL)   = QLRIV

! Minimum distance from receptor to road side QLRMIN
!
!MSK copied from below, value from LHS
!MSK          QLRMIN(IQL) = 10.
!MSK use value for Transphorm from BDE:
          QLRMIN(IQL) = 5.

! Distance from road side to line source associated receptor point:
!
          IF (QLRI(IQL) .GT. 0) THEN
!MSK: qlridist = 40.0 m, value from LHS
!MSK use value of 20.0 m for Transphorm from BDE
              QLRD(2*IQL - 1) = QLW(IQL)/2. + 20.
              QLRD(2*IQL    ) = QLW(IQL)/2. + 20.
          ELSE
              QLRD(2*IQL - 1) = MISS
              QLRD(2*IQL    ) = MISS
          ENDIF
!MSK end


! Goto next line source

          GOTO 200

  999     CONTINUE

          print *,'rlsrcs: finished second reading of metadata'

          IF (MESSFE) WRITE (MESSUN,2000) NQL

      ELSE

          print *,'RLSRCS: No Line Source emission metadata file provided. Stopped'
          stop

      ENDIF



! Close file

      CALL CLIFIL(LSRSFN,LSRSUN,LSRSFE,LSRSFM)


      RETURN

! Formats

 1000 format(A256)

 2000 format('RLSRCS: Read static data for ',I5,' line source(s)')
 2010 format('RLSRCS: No line sources metadata!')

! End of subroutine RLSRCS

      end subroutine rlsrcs
