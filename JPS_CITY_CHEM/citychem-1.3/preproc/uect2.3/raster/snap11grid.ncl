;*************************************************
;    receptorgrid.ncl
;
;*************************************************
; Generates the receptor raster grid file
; for input to EPISODE-CityChem
;
;*************************************************
; http://www.ncl.ucar.edu/Document/Functions/Contributed/regline_stats.shtml
; Uses 6.4.0 updates 
;*************************************************
; These files are loaded by default in NCL V6.2.0 and newer
;load "/usr/lib/ncarg/lib/ncarg/nclscripts/csm/gsn_code.ncl"
;load "/usr/lib/ncarg/lib/ncarg/nclscripts/csm/gsn_csm.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_code.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_csm.ncl"

begin

;---- User Edit Section ---------------------------------

     citystring="{SHEBA-HamburgCity--2012withtapm-0001}"

; coordinates origin (sw corner)
     xorig =  551750.0
     yorig = 5918656.0
; domain length in meter
     lx    = 30000.0
     ly    = 30000.0

;---- end user edit  ------------------------------------


;--- default parameters
     dx=1000.0
     dy=1000.0
     hi=3.5
     nper=-9900
     snap=11
     zl=0
     zh=0

;--- emission values
     bvocval=1.0
     otheval=0.0


;--- calculate loop indices
     nx = floattointeger(lx/dx)
     ny = floattointeger(ly/dy)

;--- Output Filename
     filepath = ""
     fpathstr = (/ filepath,"receptor_bvoc_hh",".txt"  /)
     fiouta = str_concat(fpathstr)

;--- write header lines

     ;sepa    = (/ "	"/)
     ;slabel  = (/ citystring /)
     ;hlist0  = [/slabel/]
     ;write_table( fiouta , "w", hlist0, "%s")
     ;header1 = (/"* Dummy Receptor points for 1000x1000 m2 raster grid"  /)
     ;hlist1  = [/header1/]
     ;write_table( fiouta , "a", hlist1, "%s")

     ;header2 = (/"snap,xcor_sw,ycor_sw,zcor_sw,xcor_ne,ycor_ne,zcor_ne,NOx,NMVOC,CO,SO2,NH3,PM2.5,PM10"/)
     ;hlist2  = [/header2/]
     ;write_table( fiouta , "a", hlist2, "%s")

;--- write receptors of the raster grid to ascii file

     ycoord=yorig-dy


     do n=0, ny-1

       xcoord=xorig-dx
       ycoord=ycoord+dy
       ycoord2=ycoord+dy/2

       do m=0, nx-1

         xcoord=xcoord+dx

         xcoord2=xcoord+dx/2

         xlist = [/ snap, xcoord, ycoord, zl, xcoord2, ycoord2, zh, otheval,bvocval,otheval,otheval,otheval,otheval,otheval  /] 

         write_table( fiouta , "a", xlist, "%i,%6.0f,%7.0f,%i,%6.0f,%7.0f,%i,%2.1f,%2.1f,%2.1f,%2.1f,%2.1f,%2.1f,%2.1f"  )

       end do

     end do

     end

;;;; END receptorgrid.ncl
