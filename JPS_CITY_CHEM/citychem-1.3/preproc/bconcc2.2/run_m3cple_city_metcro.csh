#!/bin/csh -f

#Run m3cple for grid interpolation and reprojection
# This is the script for the METCRO3D files from MCIP

### sphere: 'WGS 84'
### UTM 32 use SW corner of the CRO2D grid
### UTM only works for a smaller sub-domain
###
### convert CD04a and CD04b to regular lat-lon grids on 4 km
### read the grid corner points: dot-grid
### LATD and LOND in GRIDDOT2D
### keywords:
### isph = 12  'WGS 84'
### LAM to LL
### input coords  GDTYP1 = 2 (from GRIDDESC, first value)
### output coords GDTYP2 = 1 (from GRIDDESC, last value)
###               XORIG, YORIG, XCELL, YCELL in degrees
### (GRIDCRO2D also interesting)
###
### sphere: 'WGS 84'
### UTM 32 use SW corner of the CRO2D grid
### UTM only works for a smaller sub-domain
### SW corner of TAPM origin -1000 m is the GRIDDESC origin
### Hamburg
### X  551750 - 1000 =  550750 
### Y 5918656 - 1000 = 5917656


# environment variables
setenv IOAPI_ISPH    12
setenv EXECUTION_ID  '0001'

### grid conversion to CityChem grid
###   ENTER here the path and filename of the GRIDDESC file
###   for the city model domain:
#setenv GRIDDESC      ./city_grids/GRIDDESC_Riga
setenv GRIDDESC      ./city_grids/GRIDDESC_Hamburg

###   ENTER here the path of the METCRO3D input files:
# set inpath    = /home/matthias/Work/current/CMAQ/BCONCC2.0/input
###   ENTER here the path of the metc output files
# set outpath   = /home/matthias/Work/current/CMAQ/BCONCC2.0/Gdansk/m3meteo.2012


# loop over days of 2012
 set DAYin      =  1
 set Run_Id     = $DAYin
 set Run_Id_end = 31  #366

# start loop

    while ($Run_Id <= $Run_Id_end )

      if( $DAYin < 10 ) then
       set strDAYin = 00$DAYin
      else if ( $DAYin < 100) then
       set strDAYin = 0$DAYin
      else if ( $DAYin >= 100 ) then
       set strDAYin = $DAYin
      endif

# m3cple does not work with surface-only CONC files (ncks-manipulated)

### CD04a

# Convert the METCRO3D files
### LMMCIP metcro3d
### Read and write from/to USB external drive
#setenv INFILE_SH     /media/matthias/TOSHIBA\ EXT/CMAQ/CD04a/METCRO3D_cd04a_2012${strDAYin}
#setenv OUTFILE_SH    /media/matthias/TOSHIBA\ EXT/CMAQ/Riga/m3meteo.2012/metc_riga_2012${strDAYin}

setenv INFILE_SH     /home/matthias/CMAQ/CD04a/meteo/METCRO3D_cd04a_2012${strDAYin}
setenv OUTFILE_SH    /home/matthias/CMAQ/Hamburg/m3meteo/metc_hh_2012${strDAYin}

# new environment variable for input date, e.g. "2012182" (6th line of m3cple input)

setenv NEWDATE       2012${strDAYin}

     # start program m3cple
        rm ${OUTFILE_SH}
        ./m3cpleplus.exe <  ./city_input/input_conc.txt

	@ Run_Id ++
 	@ DAYin ++


# End Loop
end


# Ende der Schleife ueber die Tage

# program runtime prompts
#Y
#NONE
#INFILE_SH
#SHEBA1A
#2011345
#0
#10000
#24
#OUTFILE_SH


  exit 0
