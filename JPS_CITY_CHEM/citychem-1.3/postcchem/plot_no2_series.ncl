;*****************************************************
;BEFORE STARTING THIS SCRIPT
;
;  * Check the CSV file with monitoring data in a text editor
;  * Replace all comma by decimal point
;  * Remove all header lines at the beginning
;  * Replace ';;' by ;-99;
;
;         plot_no2_series.ncl
;
; For plotting concentration time series at several
; AQ monitoring sites for comparison between model
; and measurements.
;
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
;************************************************

;---- User Edit Section ---------------------------------

; CityChem output file ("model")
; File path and name
     filename_mod = "../OUTPUT/statmonihour.nc"
; Number of monitoring stations in output
     nstat  =  20
; Number of hours in output
     ntim   =  744
; Difference Local Time to UTC Time (UT-LT)
     st     = -2 ; LT summer +2 hours ahead
; Grafical plot path and name
     wks1   = gsn_open_wks("png","../GRAFICS/hamburg_series_no2_201307_pan1")

;---- end user edit  ------------------------------------

;***********************************************************
; AQ monitoring data
;     NO2 in ug/m3 hourly averages
; This is an example of reading a CSV file that has string,
; integer, and float fields.
;***********************************************************
  filename = "./obsdat/HH_NO2_monit_2013.csv"

;---Read in file as array of strings so we can parse each line
;---  NO2 observation csv file is without header lines

  lines = readAsciiTable(filename,1,"string",0)

  delim = ";"

;Stations:
;13ST Sternschanze
;17SM Stresemannstrasse, Traffic
;20VE Veddel
;21BI Billbrook
;24FL Flughafen
;27TA Tatenberg
;51BF Bramfeld
;52NG Neugraben
;54BL Blankenese
;61WB Wilhelmsburg
;64KS Kieler Strasse, Traffic
;68HB Habichtstrasse, Traffic
;70MB Max-Brauer Allee, Traffic
;72FI Finkenwerder West
;73FW Finkenwerder Airbus
;74BT Bilstedt
;80KT Altona Elbhang
;82HF Hafen/Kl., Grasbrock (no values)

;---Read fields 1,2,3, ...
  datehour     = str_get_field(lines,1,delim)

  station_13ST = tofloat(str_get_field(lines,2,delim))
  station_17SM = tofloat(str_get_field(lines,3,delim))
  station_20VE = tofloat(str_get_field(lines,4,delim))
  station_21BI = tofloat(str_get_field(lines,5,delim))
  station_24FL = tofloat(str_get_field(lines,6,delim))
  station_27TA = tofloat(str_get_field(lines,7,delim))
  station_51BF = tofloat(str_get_field(lines,8,delim))
  station_52NG = tofloat(str_get_field(lines,9,delim))
  station_54BL = tofloat(str_get_field(lines,10,delim))
  station_61WB = tofloat(str_get_field(lines,11,delim))
  station_64KS = tofloat(str_get_field(lines,12,delim))
  station_68HB = tofloat(str_get_field(lines,13,delim))
  station_70MB = tofloat(str_get_field(lines,14,delim))
  station_72FI = tofloat(str_get_field(lines,15,delim))
  station_73FW = tofloat(str_get_field(lines,16,delim))
  station_74BT = tofloat(str_get_field(lines,17,delim))
  station_80KT = tofloat(str_get_field(lines,18,delim))
;no values  station_82HF = tofloat(str_get_field(lines,19,delim))

;---Print the information
;  print("Date is '" + datehour + "',Station 1 is '" + station_13ST)

   ; print( station_80KT(0:200,:) )
   ; exit

;***********************************************************
;  CITYCHEM stats output
;  NO2 in ug/m3 hourly averages
;***********************************************************
;NETCDF

     cstat   =  new((/ntim,nstat,nstat/), float)
     outconc =  new((/ntim,nstat/), float) 
     monitc  =  addfile ( filename_mod, "r")
     pNames  =  getfilevarnames (monitc) ; get names of all variables on file
     print ( dimsizes(monitc->NO2(time|:,j|:,i|:)) )

     cstat   =  monitc->NO2(time|:,j|:,i|:)
     scfr    = tofloat(cstat@scale_factor)
     offr    = tofloat(cstat@add_offset)

     do n=0,ntim-1
       k=0
       do j=0, nstat-1
         do i=0, nstat-1
            if (i.eq.j) then
               outconc(n,k) = cstat(n,j,i) * scfr(0)  + offr(0)
               k=k+1
            end if
         end do 
       end do
     end do

   ;print("Conc is '" + outconc(221,16))
   ;exit


;***********************************************************
;  Combine measurements and model output in one array
;  NO2 in ug/m3 hourly averages
;***********************************************************

  no2_01  = new((/2,8784/),float)
  no2_02  = new((/2,8784/),float)
  no2_03  = new((/2,8784/),float)
  no2_04  = new((/2,8784/),float)
  no2_05  = new((/2,8784/),float)
  no2_06  = new((/2,8784/),float)
  no2_07  = new((/2,8784/),float)
  no2_08  = new((/2,8784/),float)
  no2_09  = new((/2,8784/),float)
  no2_10  = new((/2,8784/),float)
  no2_11  = new((/2,8784/),float)
  no2_12  = new((/2,8784/),float)

  ; Monitoring NO2 data gets index 1
  do i= 0,8760-1+st   ; time steps, hourly

      no2_01(1,i) = station_13ST(i-st,0)
      no2_02(1,i) = station_20VE(i-st,0)
      no2_03(1,i) = station_21BI(i-st,0)
      no2_04(1,i) = station_24FL(i-st,0)

      no2_05(1,i) = station_17SM(i-st,0)
      no2_06(1,i) = station_64KS(i-st,0)
      no2_07(1,i) = station_68HB(i-st,0)
      no2_08(1,i) = station_70MB(i-st,0)

      no2_09(1,i) = station_61WB(i-st,0)
      no2_10(1,i) = station_80KT(i-st,0)
      no2_11(1,i) = station_54BL(i-st,0)
      no2_12(1,i) = station_72FI(i-st,0)

  end do

  ; CityChem NO2 data gets index 0
  ; j is the position of the station in the station raster file
    do j=0,nstat-1
      do i=0, 24*31 -1  ; 

;---Suburban and urban background stations
        if (j.eq.15)  then    ;13ST
           no2_01(0,i) = outconc(i,j)
        end if
        if (j.eq.18)  then    ;20VE
           no2_02(0,i) = outconc(i,j)
        end if
        if (j.eq.1)  then    ;21BI
           no2_03(0,i) = outconc(i,j)
        end if
        if (j.eq.7)  then    ;24FL
           no2_04(0,i) = outconc(i,j)
        end if
;---Traffic stations
;17SM Stresemannstrasse, Traffic
;64KS Kieler Strasse, Traffic
;68HB Habichtstrasse, Traffic
;70MB Max-Brauer Allee, Traffic
        if (j.eq.16)  then     ;17SM
           no2_05(0,i) = outconc(i,j)
        end if
        if (j.eq.11)  then     ;64KS
           no2_06(0,i) = outconc(i,j)
        end if
        if (j.eq. 9)  then     ;68HB
           no2_07(0,i) = outconc(i,j)
        end if
        if (j.eq.12)  then     ;70MB
           no2_08(0,i) = outconc(i,j)
        end if

     ;** SW Hamburg
        if (j.eq.19)  then     ;61WB
           no2_09(0,i) = outconc(i,j)
        end if
        if (j.eq.0)  then     ;80KT
           no2_10(0,i) = outconc(i,j)
        end if
        if (j.eq.3)  then     ;54BL
           no2_11(0,i) = outconc(i,j)
        end if
        if (j.eq.6)  then     ;72FI
           no2_12(0,i) = outconc(i,j)
        end if

      end do
    end do

    ;print("Conc @13ST is '" + no2_01(0,0:ntim-1) )
    ;exit

; Shift model data to July (start day-of-year is 182)
    k=182*24
    do i=0, 24*31
      no2_01(0,k) = no2_01(0,i)
      no2_02(0,k) = no2_02(0,i)
      no2_03(0,k) = no2_03(0,i)
      no2_04(0,k) = no2_04(0,i)
      no2_05(0,k) = no2_05(0,i)
      no2_06(0,k) = no2_06(0,i)
      no2_07(0,k) = no2_07(0,i)
      no2_08(0,k) = no2_08(0,i)
      no2_09(0,k) = no2_09(0,i)
      no2_10(0,k) = no2_10(0,i)
      no2_11(0,k) = no2_11(0,i)
      no2_12(0,k) = no2_12(0,i)

      k=k+1
  end do

 
; rename array for plotting
  y1 = no2_01
  y1 = runave (y1, 6, 0)
  y2 = no2_02
  y2 = runave (y2, 6, 0)
  y3 = no2_03
  y3 = runave (y3, 6, 0)
  y4 = no2_04
  y4 = runave (y4, 6, 0)
  y5 = no2_05
  y5 = runave (y5, 6, 0)
  y6 = no2_06
  y6 = runave (y6, 6, 0)
  y7 = no2_07
  y7 = runave (y7, 6, 0)
  y8 = no2_08
  y8 = runave (y8, 6, 0)
  y9 = no2_09
  y9 = runave (y9, 6, 0)
  y10= no2_10
  y10= runave (y10, 6, 0)
  y11= no2_11
  y11= runave (y11, 6, 0)
  y12= no2_12
  y12= runave (y12, 6, 0)

  mlon = 8760
  dtim   = 365. / mlon
  time2  = fspan (0, (mlon - 1) * dtim, mlon)    ; lon = (/0, 2.8125, ... , 357.1875/)

;************************************************
; plot resources
;************************************************
  res             = True
  res@gsnMaximize = True
  res@gsnDraw     = False
  res@gsnFrame    = False
  res@vpWidthF    = 0.8      ; Make plots wider than 
  res@vpHeightF   = 0.2      ; they are high.
  res@tmYUseLeft           = False   ; Make right axis independent of left
  res@tmYLOn               = False   ; Turn off left tickmarks
  res@tmYROn               = True    ; Turn on right tickmarks
  res@tmXTOn               = False   ; Turn off top tickmarks
  res@tmYLLabelsOn         = False   ; Turn off left labels
  res@tmYRLabelsOn         = True    ; Turn on right labels
  res@tmYRMinorOn          = False   ; Turn off minor ticks on Y axis
  res@tmYRLabelFontHeightF = 0.015   ; Increase font height R
  res@tmYLLabelFontHeightF = 0.015   ; Increase font height L
  res@tmYRLabelDeltaF      = 2.0     ; Increase space b/w ticks and labels
  res@tmYRLabelJust        = "CenterRight"   ; right-justify labels
  res@xyLineThicknessF     = 2.0   ; Twice as thick

      res@trXMinF          =  182.1
      res@trXMaxF          =  212


  res@trYMinF              =  0.0 
  res@trYMaxF              = 120.0
;  res@tmXBMode               = "Explicit"         ; explicit labels
;  res@tmXBValues             = (/30,60,90,120,150,180,210,240,270,300,332,365/)
;  res@tmXBLabels             = (/"Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"/)
  ;res@tmXBMajorLineColor     = "transparent"
  res@tmXBLabelFontHeightF   = 0.017
  res@tmXBLabelJust        = "CenterRight"   ; right-justify labels
;**************************************************
; create panel 1
;**************************************************

  ; Change y axis string and color for each plot.
  res@tiYAxisString = "13ST  NO~B~2~N~  [ug/m~S~3~N~]"
  res@xyMarkLineModes        = (/"Lines","Markers"/)
  res@xyMarkers              = (/1,1/)   ; (0 none, 1 dot, 3 asterisk)
  res@xyMonoMarkerColor      = False
  res@xyMarkerColors         = (/"Blue","Blue"/)      ; Marker color
  res@xyLineThicknessF       = 2.8
  res@xyLineColors           = (/"red","blue"/)      ; Line color
  res@xyDashPatterns         = (/0,0/) 
  xy1 = gsn_xy(wks1,time2,y1 ,res) ; Draw an XY plot with 2 curves.
  
  ; Second panel
  res@tiYAxisString = "20VE  NO~B~2~N~  [ug/m~S~3~N~]"
  res@xyMarkLineModes        = (/"Lines","Markers"/)
  res@xyMarkers              = (/1,1/)   ; (0 none, 1 dot, 3 asterisk)
  res@xyMonoMarkerColor      = False
  res@xyMarkerColors         = (/"Blue","Blue"/)      ; Marker color
  res@xyLineThicknessF       = 2.8
  res@xyLineColors           = (/"red","blue"/)      ; Line color
  res@xyDashPatterns         = (/0,0/) 

  xy2 = gsn_xy(wks1,time2,y2 ,res) ; Draw an XY plot with 2 curves.

  ; Third panel
  res@tiYAxisString = "21BI  NO~B~2~N~  [ug/m~S~3~N~]"
  res@xyMarkLineModes        = (/"Lines","Markers"/)
  res@xyMarkers              = (/1,1/)   ; (0 none, 1 dot, 3 asterisk)
  res@xyMonoMarkerColor      = False
  res@xyMarkerColors         = (/"Blue","Blue"/)      ; Marker color
  res@xyLineThicknessF       = 2.8
  res@xyLineColors           = (/"red","blue"/)      ; Line color
  res@xyDashPatterns         = (/0,0/) 

  xy3 = gsn_xy(wks1,time2,y3 ,res) ; Draw an XY plot with 2 curves.

  ; Fourth panel
  res@tiYAxisString = "24FL  NO~B~2~N~  [ug/m~S~3~N~]"
  res@xyMarkLineModes        = (/"Lines","Markers"/)
  res@xyMarkers              = (/1,1/)   ; (0 none, 1 dot, 3 asterisk)
  res@xyMonoMarkerColor      = False
  res@xyMarkerColors         = (/"Blue","Blue"/)      ; Marker color
  res@xyLineThicknessF       = 2.8
  res@xyLineColors           = (/"red","blue"/)      ; Line color
  res@xyDashPatterns         = (/0,0/) 

  xy4 = gsn_xy(wks1,time2,y4 ,res) ; Draw an XY plot with 2 curves.


;*****************************************************   
; Manually create and attach legend
;***************************************************** 
  res_text                    = True                  ; text mods desired
  res_text@txFontHeightF      = 0.015                 ; change text size
  res_text@txJust             = "CenterLeft"          ; text justification

  res_lines                   = True                  ; polyline mods desired
  res_lines@gsLineDashPattern = 0.                    ; dotted line
  res_lines@gsLineThicknessF  = 5.                    ; line thicker
  res_lines@gsLineColor       = "red"                 ; line color


   xx = (/183,184/)


    yy = (/ 100,100 /)
    dum1 = gsn_add_polyline(wks1,xy1,xx,yy,res_lines)              ; add polyline
    dum2 = gsn_add_text(wks1,xy1,"CityChem (6h-rav, NO2)",  184.5, 100,res_text); add text

    yy = (/ 85, 85/)
    res_lines@gsLineColor       = "blue"                                 ; change to blue
    res_lines@gsLineDashPattern = 2.                    ; dotted line
    dum7 = gsn_add_polyline(wks1,xy1,xx,yy,res_lines)                ; add polyline
    dum8 = gsn_add_text(wks1,xy1,"observ. (6h-rav, NO2)", 184.5, 85,res_text)       ; add text
  

; Set up resource lists for attaching the plot.
; The res1 will apply to the base plot, and the
; res2 to the plots being attached.  These resources
; lists are *not* for changing things like line color,
; but for changing things like whether the plots
; are maximized, and which axis they are attached on.
;
  res1                     = True
  res2                     = True
  res1@gsnMaximize         = True
  res2@gsnAttachPlotsXAxis = True
;; xy1 will be the base plot.
  amid = gsn_attach_plots(xy1,(/xy2,xy3,xy4/),res1,res2)

  draw(xy1)    ; All four plots will be drawn.
  frame(wks1)



end
