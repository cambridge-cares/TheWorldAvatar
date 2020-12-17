! <depthxy.for  - A component of the City-scale
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
!* Walker, S.-E., Solberg, S., Denby, B. (2003) Development and implementation of a simplified 
!*    EMEP photochemistry scheme for urban areas in EPISODE. NILU TR 13/2013. 
!*    ISBN 82-425-1524-7
!*
!* ========================================== 
!*
!**********************************************************************

      real function depthxy(im,jm,x,y,dx,dy,depth)

!***********************************************************************
!
!     This function calculates the depth at position (x,y) based on the
!     gridded depth-values. The calculated depth-value is based on a 
!     bi-linear interpolation of the nearest 4 grid-values.
!
!     INPUT:
!           im,jm  : Basic dimension of the real (inner) model domain.
!           x,y    : position in meters of the point where the model
!                    depth is calculated. Relative to zero values at
!                    the domain origo. Must be inside the real (inner)
!                    model domain.
!
!           dx,dy  : Grid distances in meters in the x, and y direction.
!
!           depth(0:im+1,0:jm+1) : 2D array containing the domain depth
!                                  in the regular model domain.
!
!
!     OUTPUT:
!
!           depthxy : The calculated depth value.
!
! Modifications:
!    29 Jan 2019  M. Karl: replaced real*8 by double precision
!
!***********************************************************************

! *** We require the use of: implicit none

      implicit none

! *** Global declarations:
      integer :: im,jm
      real    :: x,y,dx,dy
      double precision  :: depth(0:im+1,0:jm+1)

! *** Local declarations:
      integer :: isw,jsw
      real    :: xsw,ysw
      real    :: xlength,ylength
      double precision  :: depthx_j,depthx_jp1
      double precision  :: w1,w2

! *** Content of the routine:
      xlength = x + (0.5*dx)
      ylength = y + (0.5*dy)

      isw = INT(xlength/dx)
      jsw = INT(ylength/dy)

      xsw = (isw*dx) - (0.5*dx)
      ysw = (jsw*dy) - (0.5*dy)

! *** Start Bilinear interpolation; applying the 4 nearest depth values:
      w1 = (x - xsw)/dx
      w2 = 1.0 - w1

      depthx_j   = w1*depth(isw+1,jsw)   + w2*depth(isw,jsw)
      depthx_jp1 = w1*depth(isw+1,jsw+1) + w2*depth(isw,jsw+1)

      w1 = (y - ysw)/dy
      w2 = 1.0 - w1

      depthxy   = w1*depthx_jp1 + w2*depthx_j      

! *** End Bilinear interpolation.

      return
      end function depthxy
