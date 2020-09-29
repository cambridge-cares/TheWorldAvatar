#! /bin/csh -f


set M3HOME  = .
setenv EXECUTION_ID Linux2_x86_64gfort
setenv PROMPTFLAG   N

# 201201:   1-32
# 201202:  32-61
# 201203:  61-92
# 201204:  92-122
# 201205: 122-153
# 201206: 153-183
# 201207: 183-214
# 201208: 214-245
# 201209: 245-275
# 201210: 275-306
# 201211: 306-336
# 201212: 336-366

 set STDATE       = 2013182
 set ENDATE       = 2013213

 while ( ${STDATE} <= ${ENDATE} )

   set YEAR = ${STDATE}
   @ YEAR = ${YEAR} / 1000
   set JDAY = ${STDATE}
   @ JDAY = ${STDATE} - 1000 * ${YEAR}

#> mulit day loop
   set DATE = ${STDATE}

#CDxx INPUT
#
# CMAQ CONC FILE
setenv INPUT_FILE1   /home/matthias/CMAQ/Hamburg/m3bcon/conc_utm_cd06_${DATE}
# CMAQ METCRO3D FILE
setenv INPUT_FILE2   /home/matthias/CMAQ/Hamburg/m3meteo/metc_utm_cd06_${DATE}


     ${M3HOME}/cmaq4cc.exe

     @ STDATE = ${STDATE} + 1

end
  
exit(0)
