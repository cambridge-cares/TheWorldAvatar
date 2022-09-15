#!/bin/csh -f
#!/usr/local/bin/tcsh
#.........................................................................
# Version "$Id: fix_src.csh 1 2017-06-10 18:05:20Z coats $"
# EDSS/Models-3 I/O API.
# Copyright (C) 2002 MCNC and
#           (C) 2016 UNC Institute for the Environment
# Distributed under the GNU Lesser PUBLIC LICENSE version 2.1
# See file "LGPL.txt" for conditions of use.
#.........................................................................
#  Script to take I/O API-style Fortran source that is compatible with both 
#  standard F77/F90 fixed source form and F90 free source form, and produce
#  a file compatible with F77/F90 extended-column fixed source form
#.........................................................................
#  USAGE
#       fix_src.csh <input-file> <output-file>
#.........................................................................
#  IBM NOTE
#       IBM's "csh" misbehaves, so you may need to substitute "tcsh"
#       as indicated above
#.........................................................................

switch ( $#argv )
    case 2
        breaksw
    default:
        echo "Usage:  fix_src.csh <input-file> <output-file>" 
        exit( 2 ) 
endsw

if ( -e $2 ) chmod u+w $2
sed -e 's/ *& *$//' < $1 > $2

set foo=$status
chmod a-w $2

if ( ${foo} != 0 )  then
    echo "ERROR ${foo} in script fix_src.csh \n\n"
    exit( ${foo} )
endif

chmod a-w $2
exit( 0 )


