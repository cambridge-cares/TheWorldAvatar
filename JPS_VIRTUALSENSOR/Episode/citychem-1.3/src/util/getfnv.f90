! <getfnv.f90 - A component of the City-scale
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

      SUBROUTINE GETFNV(TXTSTR,FN,FE,NV,MV,FV)

! *** The subroutine reads a filename FN or associated file values(s)
! *** FV(I) for I = 1,NV from a 256-character textstring TXTSTR.
! *** If TXTSTR is blank , or no valid values are specified in the
! *** runfile then the file values, FV(NV), is returned with the 
! *** missing value: MISS = -9900.
!_LHS_Start: Change for applying GETFNV outside of episode_dll.
!_LHS        USE mod_util
!_LHS_End: Change for applying GETFNV outside of episode_dll.
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

      IMPLICIT NONE


! *** Declaration of arguments:
      INTEGER       :: MV,NV
      character(len=256)  :: TXTSTR, FN
      LOGICAL       :: FE
      REAL          :: FV(MV)

!_LHS_Start: Change for applying GETFNV outside of episode_dll.
      REAL,PARAMETER :: MISS = -9900.
!_LHS_End: Change for applying GETFNV outside of episode_dll.

! *** MV     - Dimension of FV as declared in the calling program
! *** NV     - Number of file value(s)
! *** TXTSTR - Text string
! *** FN     - Filename
! *** FE     - If file exists then true else false
! *** FV     - File value(s)

! *** Local variables:
      INTEGER :: I,IOS,IV,J
      LOGICAL :: LEOF

! *** I      - Textstring index
! *** IOS    - Input/output status
! *** IV     - Index of file value(s)
! *** J      - Textstring index
! *** LEOF   - If end of file then true else false

! *** If textstring starts with a < character then just return:
      IF (TXTSTR(1:1) .EQ. '<') THEN
        FN = ' '
        FE = .FALSE.
        DO I = 1,NV
          FV(I) = MISS
        ENDDO
        RETURN
      ENDIF

! *** Read filename from textstring:

! *** Set the whole textstring FN to blank characters.
      FN = ' '
! *** The INDEX-function returns the position-number of the specified text,
! *** in this case the first blank-character of TXTSTR.
      J = INDEX(TXTSTR,' ') - 1
      READ (TXTSTR,1000) (FN(I:I),I=1,J)

! *** NOTE: If TXTSTR(1:1) == ' ' then "FN" is returned as all blank.

! *** If FN is all blank then the file does not exist:
      IF (FN .EQ. ' ') THEN
          FE = .FALSE.
! ***     Try to read values instead:
          GOTO 200
      ENDIF

! *** Check if file exists:
      INQUIRE(FILE=FN,EXIST = FE)
      IF (FE) THEN
! ***   Set associated file value(s) to missing data:
        DO 100 IV = 1,NV
          FV(IV) = MISS
  100   CONTINUE
        RETURN
      ENDIF

  200 CONTINUE

! *** Check if the user given "TXTSTR" is all blank:
      IF (TXTSTR .EQ. ' ') THEN
! ***   Set associated file value(s) to missing data:
        DO 210 IV = 1,NV
          FV(IV) = MISS
  210   CONTINUE
        RETURN
      ENDIF

! *** Try to read file associated value(s):
      READ (TXTSTR,*,END = 220,ERR = 220,IOSTAT = IOS) (FV(IV),IV=1,NV)

  220 CONTINUE

! *** If successful read then return:
      IF (IOS .EQ. 0) RETURN

! *** Set associated file value(s) to missing data:
      DO 230 IV = 1,NV
        FV(IV) = MISS
  230 CONTINUE
      RETURN

 1000 FORMAT (256A1)

! *** End of subroutine GETFNV

      END SUBROUTINE GETFNV
