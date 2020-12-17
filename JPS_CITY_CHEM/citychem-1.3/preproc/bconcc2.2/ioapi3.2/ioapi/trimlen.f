
C.........................................................................
C Version "$Id: trimlen.f 1 2017-06-10 18:05:20Z coats $"
C EDSS/Models-3 I/O API.
C Copyright (C) 1992-2002 MCNC and Carlie J. Coats, Jr.,
C (C) 2003-2010 Baron Advanced Meteorological Systems, and
C (C) 2014 UNC Institute for the Environment.
C Distributed under the GNU LESSER GENERAL PUBLIC LICENSE version 2.1
C See file "LGPL.txt" for conditions of use.
C.........................................................................

        INTEGER  FUNCTION  TRIMLEN ( STRING )

C***********************************************************************
C  function body starts at line 43
C
C  FUNCTION:  return the effective length of argument CHARACTER*(*) STRING,
C             after trailing blanks have been trimmed.
C
C  PRECONDITIONS REQUIRED:  none
C
C  SUBROUTINES AND FUNCTIONS CALLED:  none
C
C  REVISION  HISTORY:  
C       Prototype  8/1991 by CJC
C       Version    2/1993 for CRAY by CJC
C       Version    9/2014 by CJC:  Use F90 LEN_TRIM
C***********************************************************************

      IMPLICIT NONE


C...........   ARGUMENTS and their descriptions:

        CHARACTER*(*), INTENT( IN ) :: STRING


C***********************************************************************
C   begin body of function  TRIMLEN

        TRIMLEN = LEN_TRIM( STRING )

        RETURN

        END FUNCTION  TRIMLEN

