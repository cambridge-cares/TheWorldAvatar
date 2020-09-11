! <csubg.f90 - A component of the City-scale
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

      subroutine CSUBG(XRV,YRV,ZRV,NCV,MCV,CRV,DDRV,WDRV)

!     The subroutine calculates grid model concentrations and 
!     dry and wet depositions in the given receptor point.
!       2017 Matthias Karl, HZG, CityChem extension:
!            This routine is the place where grid concentrations are handed
!            over to the sub-grid concentrations of the regular receptor
!            gird. It is ensured that there is no feed back of the
!            local sub-grid concentrations to the main grid 
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
!           2016  M. Karl: CITYDELTA extension commented
!           2016  M. Karl: CRVV = C(IC,IX+1,IY+1,IZ)
!           2017  M. Karl: CRVV has to be double precision, as C
!
! ----------------------------------------------------------------------------------

      use mod_util
      use mod_main
      use mod_site
      use mod_time
      use mod_mete
      use mod_conc
      use mod_depo
      use mod_grid

      implicit none

      integer :: NCV,MCV
      real    :: XRV,YRV,ZRV,DDRV(MCV),WDRV(MCV)

!     NCV  - Number of compounds
!     MCV  - Maximum number of compounds
!     XRV  - Receptor x-coordinate
!     YRV  - Receptor y-coordinate
!     ZRV  - Receptor z-coordinate
!     CRV  - Receptor concentration  values
!     DDRV - Receptor dry deposition values
!     WDRV - Receptor wet deposition values

!     Local variables

      integer :: IC,IX,IY,IZ
      real    :: C0,C1,C2,DDRVV,PRECV,QDV,QWV,WDRVV
!MSK start CRVV has to be double precision, as C
      double precision :: CRVV
      double precision, dimension(MCV) :: CRV
!MSk end

!     CRVV  - Receptor concentration  value
!     C0    - Concentration value
!     C1    - Concentration value
!     C2    - Concentration value
!     DDRVV - Receptor dry deposition value
!     PRECV - Precipitation value
!     QDV   - Dry deposition rate
!     QWV   - Wet deposition rate
!     WDRVV - Receptor wet deposition value
!     IC    - Compound index
!     IX    - Grid model grid index in x-direction
!     IY    - Grid model grid index in y-direction
!     IZ    - Grid model grid index in z-direction

!     Get main grid indices:

      CALL GETMGI(1,XRV,YRV,ZRV,IX,IY,IZ)

!     Go through all compounds

      DO 100 IC = 1,NCV

!         Apply grid model concentration for the grid cell
!         containing the receptor coordinate point.

!MSK start
!MSK *** This is the connection between Eulerian grid concentration C
!MSK *** and the local main grid, receptor point, line receptor
!MSK *** and subgrid concentrations CM, CR, CLR, CS 
!MSK          CRVV = C(IC,IX,IY,IZ)
          CRVV = C(IC,IX+1,IY+1,IZ)
                !if ( (ic==6) .and. (iy==16).and.(ix==1) ) then
                !   print *,'csubg CO', CRVV
                !endif
!MSK end

!_CITYDELTA_Start:
!
!         Use profile and deposition velocity to calculate the 
!         concentration at "ZRV" m above the ground according to 
!         Simpson ().

!          if ( DDEPV(IC) > 0 ) then
!	       ZB1 = 2.0   ! Bruce implementation
!	    else
!             ZB1 = ZRV
!          endif
!          
!	    ZB2 = (0.5*DZ(IZ)*DEPTHM(IX,IY))/MOD_H
!
!	    PHIDIF=(ALOG(ZB2/ZB1)
!     &            -PSIH((ZB2)/MOBUL(IX,IY))+PSIH(ZB1/MOBUL(IX,IY)))
!
!
!_LHS_Sep_2004_Start:
!
!         The DEPFACTOR below is just a reduction factor which is 
!         multiplied with the calculated concentration value in the 
!         lowermost layer, z = ZB2, in order to get the corresponding
!         concentration value at z = ZB1 = 2 m in this case.   
!
!	    DEPFACTOR=1.0/(1.0+DDEPV(IC)/0.4/USTAR(IX,IY)*PHIDIF)
!
!_LHS     DEPFACTOR = (DDEPV(IC) * PHIDIF) / (0.4 * USTAR(IX,IY) )
!_LHS     DEPFACTOR = 1.0 / (1.0 + DEPFACTOR)
!
!	    DEPFACTOR=MAX(0.0,DEPFACTOR)
!	    DEPFACTOR=MIN(1.0,DEPFACTOR)
!
!_LHS_Sep_2004_End.
!
!         IF (DEPFACTOR /= 1.0)THEN
!           if (messfe) then  
!              write(messun,*)
!              write(messun,'(A,F12.4)') 'CSUBG: DEPFACTOR = ',DEPFACTOR
!              write(messun,'(A,F12.4)') 'CSUBG: PROGRAM TERMINATES! '
!           endif
!           STOP
!         ENDIF
!
!	    CRVV=CRVV*DEPFACTOR
!
!_LHS     CRVV is now the estimated receptor concentration value at 
!_LHS     the height of z = ZB1 = 2 m.

!	    if (IX == 11 .and. IY == 13 .and. IC ==4) then
!	      write(*,*)'DEP FACTOR R: ',DEPFACTOR
!	    endif

!_CITYDELTA_End.

!         Convert precipitation from mm/h to m/s

          PRECV = PREC(IX,IY)/3.6E+6

!         Calculate dry deposition rate (/s)

          QDV = 2.*(DDEPV(IC)/(DDEPV(IC)*AERO(IX,IY) + 1.))/DZ(1)

!         Calculate wet deposition rate (/s)

          QWV = WDEPSR(IC)*PRECV/HMIX(IX,IY)

! ***     Calculate dry and wet depositions (g/m2)

!          C0 = CRVV
!          C1 = C0
!          C2 = C1

!_LHS     Below the dry and wet deposition is set identically to zero.
          DDRVV = 0.
          WDRVV = 0.

!         Set concentration and dry and wet depositions in the given
!         receptor point

            CRV(IC) =  CRVV
           DDRV(IC) = DDRVV
           WDRV(IC) = WDRVV

  100 CONTINUE

      RETURN

! *** End of subroutine CSUBG

      end subroutine csubg
