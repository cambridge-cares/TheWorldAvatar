;----------------------------------------------------------------------
;   vertical_profile.ncl              
;
;  Creates a contour plot of the time series of the
;  vertical concentration profile up to 450 m height
;
; These files are loaded by default in NCL V6.2.0 and newer
;----------------------------------------------------------------------
;
; REPLACE BELOW LINES WITH THE PATH OF THE NCARG LIBRARY
; ON YOUR MACHINE !!!
;
;----------------------------------------------------------------------

load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_code.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_csm.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/wrf/WRFUserARW.ncl"
;load "/usr/local/ncl/lib/ncarg/nclscripts/csm/gsn_code.ncl"
;load "/usr/local/ncl/lib/ncarg/nclscripts/csm/gsn_csm.ncl"
;load "/usr/local/ncl/lib/ncarg/nclscripts/wrf/WRFUserARW.ncl"

;----------------------------------------------------------------------

begin

;---- User Edit Section ---------------------------------

;** CITYCHEM main grid hourly 3-D output
    gridfilename = "../OUTPUT/concmhour.nc"

;**  simulation period
    month        = "201307" 

;** species
    species       = "NO2"
    ;species       = "O3"    

;**  main grid model cell
    cellx        = 10
    celly        = 16

;**  start/end day of month
    startday     =  15.5
    endday       =  20

;**  concentration range for contour plot
    if (species.eq."NO2") then
      minlev =  2.0
      maxlev = 20.0
      stepl  =  2.0
    else if (species.eq."O3") then
      minlev =  2.0
      maxlev = 38.0
      stepl  =  4.0
    end if
    end if
;---- end user edit  ------------------------------------

;================================================
; define variable
;================================================

    ntim = 744
    nlev = 24
;**  grafics path and filename   
    fplotpath    = "../GRAFICS/vertical_"+species+"_201307" 
    CNO2       = new((/nlev,ntim/), float) 
    time_hours = new((/ntim/), float) 
    wks  = gsn_open_wks("png",fplotpath)

;================================================
; get data
;================================================

;**  grid output path

    gridc        = addfile ( gridfilename, "r")
    vNames       = getfilevarnames (gridc) ; get names of all variables on file
      
   ; print( vNames )

    height  = gridc->k
    timeh   = gridc->time

    if (species.eq."NO2") then
      concn   = gridc->NO2( :, :, celly,cellx )
    else if (species.eq."O3") then
      concn   =  gridc->O3( :, :, celly,cellx )
    end if
    end if
    
    print( dimsizes(concn) )

    print( concn(0,0) )
    
    scf      = tofloat(concn@scale_factor)
    off      = tofloat(concn@add_offset)

    do n=0, ntim-1
      do k=0, nlev-1
         CNO2(k,n) = concn(n,k) * scf(0)  + off(0)
      end do
    end do

;** convert ug/m3 to ppbV  by division with 2.024 
;** even if constant pressure was used
;** assuming a dT/dz <= -1 K/100m the Tair changes 
;** from 22 degC in 2m to 18 degC in 400m, so 
;** the air density change is only a few percent
;** from 0m to 400m height.
;
;    if(species.eq."NO2") then
;      CNO2   = CNO2/2.024
;    else if(species.eq."O3") then   
;      CNO2   = CNO2/2.112
;    end if
;    end if

;** time in days fractions

    mlon   = 8760
    dtim   = 365. / mlon
    time2  = fspan (0, (mlon - 1) * dtim, mlon)    ; lon = (/0, 2.8125, ... , 357.1875/)

;================================================
; plot
;================================================
 res=True

 res@cnFillOn             = True       ; turn on color fill
 res@cnLineLabelsOn       = False      ; turns off contour line labels
 res@cnInfoLabelOn        = False      ; turns off contour info label
 res@cnFillPalette        = "gui_default" ; set color map
 res@lbOrientation        = "vertical" ; vertical label bar

 res@sfXArray             = time2(0:ntim-1)    ; uses timeh as plot x-axis
 res@sfYArray             = height        ; height in m as plot y-axis

 res@gsnYAxisIrregular2Linear = True      ; converts irreg depth to linear 
 res@trYReverse        = False     ; True reverses y-axis

  res@tiMainString     = "Vertical NO2 Concentration [ug/m~S~3~N~], cell ("+cellx+","+celly+")"  ; main title
  res@tiYAxisString    = "Altitude [m]"
  res@tiXAxisString    = "Day of month"

  res@trYMinF          = 0.0 
  res@trYMaxF          = 400.0

  res@trXMinF          = startday
  res@trXMaxF          = endday

    res@cnLevelSelectionMode   = "ManualLevels" 
    res@cnMinLevelValF         =  minlev
    res@cnMaxLevelValF         =  maxlev
    res@cnLevelSpacingF        =  stepl

    plot = gsn_csm_contour(wks,CNO2,res)     ; create plot

end
