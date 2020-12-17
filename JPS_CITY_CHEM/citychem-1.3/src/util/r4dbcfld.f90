! <r4dbcfld.f90 - A component of the City-scale
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
!* ========================================== 
!*
!*****************************************************************************!
!*
!*        CITY-scale CHEMistry Transport Extension
!*
!*        Copyright (C) 2018  Matthias Steffen Karl
!*
!*        Contact Information: 
!*            Institute of Coastal Research
!*            Helmholtz-Zentrum Geesthacht
!*            Max-Planck-Str. 1
!*            21502 Geesthacht
!*            Germany
!*            email:  matthias.karl@hzg.de
!*
!*      EPISODE-CityChem, developed at Helmholtz-Zentrum Geesthacht (HZG) is designed
!*      for treating complex atmospheric chemistry in urban areas (Karl, 2018). The model
!*      is an extension of the EPISODE dispersion model to enable chemistry/transport
!*      simulations of reactive pollutants on city scale. EPISODE is an Eulerian dispersion
!*      model developed at the Norwegian Institute for Air Research (NILU) appropriate for
!*      air quality studies at the local scale (Slørdal et al. 2003 &2008). The model is an
!*      open source code subject to the Reciprocal Public License ("RPL") Version 1.5,
!*      https://opensource.org/licenses/RPL-1.5.
!*
!*        Reference:
!*      Karl, M. (2018):  Development of the city-scale chemistry transport model 
!*      CityChem-EPISODE and its application to the city of Hamburg, 
!*      Geosci. Model Dev. Discuss.,
!*      https://doi.org/10.5194/gmd-2018-8, 2018.
!*
!*****************************************************************************!

!MSK      SUBROUTINE R4DBCFLD(UN,IRWF,IC,K,TEXT1,TEXT2,NC,NX,NY,NZ,       &
      SUBROUTINE R4DBCFLD(UN,IRWF,IC, TEXT1,TEXT2,NC,NX,NY,NZ,       &
                         NBCX,NBCY,NBCZ,MC,MX,MY,MZ,MBCX,MBCY,MBCZ,  &
                         FLD)

!       2016 Matthias Karl, HZG, CityChem extension:
!       Substantial modification to read 3D BCON input files from CMAQ
! ----------------------------------------------------------------------------------
! REVISION HISTORY
!
!    xx Dat 201x  Name: Line  Description of Change
!
! ----------------------------------------------------------------------------------

!MSk start
      USE mod_time
!MSK end

! *** The subroutine reads from fileunit UN a 2D Boundary Condition field 
! *** which are for compound IC and layer K, of the 4D-field FLD using 
! *** read/write format (index) IRWF.

!     NOTE: The Date and Time specified in the Boundary Condition file is not
!           taken into consideration. The file just read sequentially starting
!           at the beginning.

!     Scalar arguments   ! Note NC is not used here.

      INTEGER UN,IRWF,IC, NC,NX,NY,NZ,NBCX,NBCY,NBCZ,   &
             MC,MX,MY,MZ,MBCX,MBCY,MBCZ
      CHARACTER(len=10) TEXT1,TEXT2
      CHARACTER(len=20) TEXT3 

!     UN             - Fileunit
!     IRWF           - Read/write format index (1=ASCII,  2=Binary)
!     IC,K           - Fixed indices
!     NC,NX,NY,NZ    - Dimensions of FLD
!     NBCX,NBCY,NBCZ - Dimensions of boundary width.
!     MC,MX,MY,MZ    - Declared dimensions of FLD in the calling program
!     MBCX,MBCY,MBCZ - Declared dimensions of the open boundaries in the
!                  calling program
!     TEXT1,TEXT2    - Text strings read from file

!     Array arguments

!MSK      REAL FLD(MC,1-MBCX:MX+MBCX,1-MBCY:MY+MBCY,MZ+MBCZ)
      double precision FLD(MC, MX+2*MBCX, MY+2*MBCY, MZ+MBCZ)

!     FLD - The field

!     Local variables

      INTEGER I,J,IX,JY,NX_D,NY_D,NZ_D,NBCX_D,NBCY_D,NBCZ_D
!MSK start
      integer K
!MSK end

      LOGICAL JUST_BV

!     ---------------------  END DECLARATIONS  ---------------------

!MSK       JUST_BV = .TRUE.
       JUST_BV = .FALSE.

!_old      IX_DIM = NX + (2 * NBCX)
!_old      JY_DIM = NY + (2 * NBCY)


!     Read field data for layer IC,K depending on read/write format

      IF (IRWF .LE. 0) THEN

!     No data is read

          RETURN

      ENDIF

      IF (IRWF .EQ. 1) THEN

!    Read data using simple ASCII  format

         IF( .NOT. JUST_BV)THEN
	 		
!       ALTERNATIVE 1: Reading the whole field.
!       --------------------------------------------

!MSK start
          IF (ATTIME(BDAT)) THEN
            READ (UN,2103) TEXT1,TEXT2,NX_D,NY_D,NZ_D,NBCX_D,NBCY_D,NBCZ_D

            !print *,'r4dbcfld,',NX,NY,NZ,NBCX,NBCY,NBCZ
            !print *,'r4dbcfld,',NX_D,NY_D,NZ_D,NBCX_D,NBCY_D,NBCZ_D

            IF (NX_D .NE. NX .OR. NY_D .NE. NY .OR. NZ_D .NE. NZ .OR.    &
                NBCX_D .NE. NBCX .OR. NBCY_D .NE. NBCY .OR.             &
                NBCZ_D .NE. NBCZ)                                       &
              CALL STOPIT('R4DBCFLD: Wrong Boundary Condition indexes!')
          ENDIF
!MSK end


!MSK index null not allowed
!MSK         DO J=1-NBCY,NY+NBCY
!MSK           DO I=1-NBCX,NX+NBCX

!MSK start
        DO K=1,NZ+NBCZ

          !read one comment line inbetween
          READ(UN,*) TEXT3

          !print *,'r4dbcfld TXT3', TEXT3

          DO J=1,NY+ 2*NBCY
            DO I=1,NX+ 2*NBCX
               READ(UN,*) IX,JY,FLD(IC,I,J,K)
            ENDDO
          ENDDO

       ENDDO

!MSK         READ(UN,*)

!MSK end

       ELSE

!       ALTERNATIVE 2: Reading only boundary values.
!       --------------------------------------------

        READ (UN,2103) TEXT1,TEXT2,NX_D,NY_D,NZ_D,NBCX_D,NBCY_D,NBCZ_D

        IF (NX_D .NE. NX .OR. NY_D .NE. NY .OR. NZ_D .NE. NZ .OR.    &
            NBCX_D .NE. NBCX .OR. NBCY_D .NE. NBCY .OR.             &
            NBCZ_D .NE. NBCZ)                                       &
          CALL STOPIT('R4DBCFLD: Wrong Boundary Condition indexes!')

          IF(K .LE. NZ)THEN
!          Treating the internal layers:

            DO J=1-NBCY,0
              DO I=1-NBCX,NX+NBCX
                READ (UN,*) IX,JY,FLD(IC,I,J,K)
!_LHS_EMEP_BC_DATA
!             READ (UN,*) mm,dd,hh,(episode_data(ispec,dd,hh,k),k=1,EPISODE_LEV)
!_LHS_EMEP_BC_DATA
              ENDDO
            ENDDO

            DO J=1,NY
              DO I=1-NBCX,0
                  READ (UN,*) IX,JY,FLD(IC,I,J,K)
              ENDDO
              DO I=NX+1,NX+NBCX
                  READ (UN,*) IX,JY,FLD(IC,I,J,K)
             ENDDO
            ENDDO

            DO J=NY+1,NY+NBCY
               DO I=1-NBCX,NX+NBCX
                     READ (UN,*) IX,JY,FLD(IC,I,J,K)
               ENDDO
            ENDDO

        ELSE
!          Treating the upper boundary value(s):

            DO J=1-NBCY,NY+NBCY
              DO I=1-NBCX,NX+NBCX
                     READ (UN,*) IX,JY,FLD(IC,I,J,K)
              ENDDO
            ENDDO

        ENDIF

         READ (UN,*)

        ENDIF

      ENDIF


      IF (IRWF .EQ. 2) THEN

!     Read data using simple binary format

          IF( .NOT. JUST_BV)THEN

!       ALTERNATIVE 1: Reading the whole field.
!       ---------------------------------------

!MSK        READ (UN) TEXT1,TEXT2,NX_D,NY_D,NZ_D,NBCX_D,NBCY_D,NBCZ_D,    &
!MSK               ((FLD(IC,I,J,K),I=1-NBCX,NX+NBCX),J=1-NBCY,NY+NBCY)

!MSK        IF (NX_D .NE. NX .OR. NY_D .NE. NY .OR. NZ_D .NE. NZ .OR.     &
!MSK            NBCX_D .NE. NBCX .OR. NBCY_D .NE. NBCY .OR.              &
!MSK            NBCZ_D .NE. NBCZ)                                        &  
!MSK           CALL STOPIT('R4DBCFLD: Wrong Boundary Condition indexes!')
           ! print *,'r4d dims2',MC,MX,MY,MZ,MBCX,MBCY,MBCZ
          DO K=1,NZ+NBCZ
            DO J=1,NY+ 2*NBCY
              DO I=1,NX+ 2*NBCX
                 FLD(IC,I,J,K) = DBLE(0.0)
              ENDDO
            ENDDO
          ENDDO

!MSK start  FLD is double precision; BC input file has to be double precision
          DO K=1, NZ+NBCZ

           ! print *,'r4d k',K
            !READ(UN)  ( ( FLD(IC,I,J,K),I=1,NX+ 2*NBCX ),J=1,NY+ 2*NBCY )
            READ(UN)  TEXT1,TEXT2,NX_D,NY_D, ( ( FLD(IC,I,J,K),I=1,NX+ 2*NBCX ),J=1,NY+ 2*NBCY )
            !print *,'r4dbcfld TXT3', TEXT1,TEXT2,NX_D,NY_D

            !DO J=1, NY+ 2*NBCY
            !  DO I=1, NX+ 2*NBCX 
            !     print *,'r4dbcfld fld',IC,K,I,J,FLD(IC,I,J,K)
            !  ENDDO
            !ENDDO

         ENDDO

!MSK end


       ELSE

!       ALTERNATIVE 2: Reading only boundary values.
!       --------------------------------------------

           READ (UN) TEXT1,TEXT2,NX_D,NY_D,NZ_D,NBCX_D,NBCY_D,NBCZ_D

           IF (NX_D .NE. NX .OR. NY_D .NE. NY .OR. NZ_D .NE. NZ .OR.     &
              NBCX_D .NE. NBCX .OR. NBCY_D .NE. NBCY .OR.              & 
              NBCZ_D .NE. NBCZ)                                        &
           CALL STOPIT('R4DBCFLD: Wrong Boundary Condition indexes!')

           IF(K .LE. NZ)THEN
!          Treating the internal layers:

!	    DO J=1-NBCY,0
!	     DO I=1-NBCX,NX+NBCX
!		    READ (UN) FLD(IC,I,J,K)
!           ENDDO
!	    ENDDO

!	    DO J=1,NY
!	     DO I=1-NBCX,0
!		    READ (UN) FLD(IC,I,J,K)
!           ENDDO
!	     DO I=NX+1,NX+NBCX
!	      READ (UN) FLD(IC,I,J,K)
!	     ENDDO
!	    ENDDO

!	    DO J=NY+1,NY+NBCY
!	     DO I=1-NBCX,NX+NBCX
!		    READ (UN) FLD(IC,I,J,K)
!           ENDDO
!	    ENDDO

            READ(UN)((FLD(IC,I,J,K),I=1-NBCX,NX+NBCX),J=1-NBCY,0)
            READ(UN)((FLD(IC,I,J,K),I=1-NBCX,0),J=1,NY)
            READ(UN)((FLD(IC,I,J,K),I=NX+1,NX+NBCX),J=1,NY)
            READ(UN)((FLD(IC,I,J,K),I=1-NBCX,NX+NBCX),J=NY+1,NY+NBCY)

        ELSE
!          Treating the upper boundary value(s):
!	    DO J=1-NBCY,NY+NBCY
!	     DO I=1-NBCX,NX+NBCX
!		    READ (UN) FLD(IC,I,J,K)
!           ENDDO
!	    ENDDO

            READ(UN)((FLD(IC,I,J,K),I=1-NBCX,NX+NBCX),J=1-NBCY,NY+NBCY)

        ENDIF

       ENDIF

      ENDIF

!      IF (IRWF .EQ. 5) THEN
!
!          Read data using print format
!
!          CALL R4DPRN(UN,IC,K,0.,TEXT1,TEXT2,NC,NX,NY,NZ,MC,MX,MY,MZ,
!     &                FLD)
!      ENDIF
!
!
! *** If only the boundary values are read into the 4D variable "fld
      IF(JUST_BV .AND. K .LE. NZ)THEN

         DO J=1,NY
             DO I=1,NX
               FLD(IC,I,J,K) = -9900.
             ENDDO
         ENDDO
      
      ENDIF
      
      RETURN

 2100 FORMAT (2A10,2I3,/,32767(2I3,1P,E16.8,/))
 2101 FORMAT (2A10,2I3)
 2102 FORMAT (2I3,1P,E16.8)
 2103 FORMAT (2A10,6I3)
! End of subroutine R4DBCFLD

      END
