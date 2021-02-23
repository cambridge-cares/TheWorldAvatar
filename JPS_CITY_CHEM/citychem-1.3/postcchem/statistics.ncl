;*************************************************
;    statistics.ncl
;
;*************************************************
; Concepts illustrated:
;   - Create arrays with data
;   - Calculating the simple linear regression via 'regline_stats'
;     Returns same information as 'regline' *and* many additional statistics.
;   - Uses 6.4.0 information to calculate:
;     (a) 95% line drawn with 5 and 95% slope and y-intercept limits
;     (b) 95% mean response
;     (c) 95% confidence interval
;   - Drawing a scatter plot with regression line
;   - Drawing all regression information
;
; statistics.ncl gives statistics parameter set for NO2 for 12 stations
;                and correlation plots
;
;*************************************************
; http://www.ncl.ucar.edu/Document/Functions/Contributed/regline_stats.shtml
; Uses 6.4.0 updates 
;*************************************************
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

     July    = True
     month   = "201307"

;--- Select 1h or 24 averages for the statistics overview file
     ;Hour    = False
     Hour    = True

; Difference Local Time to UTC Time (UT-LT)
     st     = -2 ; LT summer +2 hours ahead

;---CityChem output file ("model")
; File path and name
     filename_mod = "../OUTPUT/statmonihour.nc"
; Number of monitoring stations in output
     nstat  =  20
; Number of hours in output
     ntim   =  744

;---Parameters for Statistics
; Number of monitoring stations to include
     nouts   = 13   ;max 17
; Number of hours for statistics
     ntims   = 744 ; 720
; Number of days for statistics
     daym    = 31 ; 30

;---- end user edit  ------------------------------------


;--- Select pollutant
    poll    = "no2"

;--- Output Filenames
    filepath = "../GRAFICS/"
    fpathstr = (/ filepath,"hh_",poll,"_stats_",month,".txt"  /)
    plotmstr = (/ filepath,"hh_",poll,"_month_",month  /)
    plotdstr = (/ filepath,"hh_",poll,"_daily_",month  /) 
    if (Hour) then
      plothstr = (/ filepath,"station_hh_",poll,"_hour_",month  /) 
    else
      plothstr = (/ filepath,"station_hh_",poll,"_24hr_",month  /) 
    end if
    fiouta = str_concat(fpathstr)
    fmouta = str_concat(plotmstr)
    fdouta = str_concat(plotdstr)
    fhouta = str_concat(plothstr)
;--- Open work stations for figure output
    wks1 = gsn_open_wks("png", fmouta  )
    wks2 = gsn_open_wks("png", fdouta  )

;--- Title string for pollutant
    pollname = "NO~B~2~N~(ug/m~S~3~N~)"



;***********************************************************
;  NO2 in ug/m3 hourly averages
;***********************************************************

    filename = "./obsdat/HH_NO2_monit_2013.csv"


;---Read in file as array of strings so we can parse each line
  
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


;***********************************************************
;  CITYCHEM stats output
;  NO2 in ug/m3 hourly averages
;
;;NETCDF
;***********************************************************

     cstat   =  new((/ntim,nstat,nstat/), float)
     outconc =  new((/ntim,nstat/), float) 
     monitc  =  addfile ( filename_mod, "r")
     pNames  =  getfilevarnames (monitc) ; get names of all variables on file
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


;***********************************************************
;  Combine measurements and model output in one array
;  NO2 in ug/m3 hourly averages
;***********************************************************

;--- Spatial statistics, Monthly
  xmonth  = new((/nouts/),float)
  ymonth  = new((/nouts/),float)

;--- Spatiotemporal statistics, Daily
  xday    = new((/nouts, daym/),float)
  yday    = new((/nouts, daym/),float)
  xdayall = new((/nouts*daym/),float)
  ydayall = new((/nouts*daym/),float)

;--- Temporal statistics, Hourly
  x       = new((/nouts,ntims /),float)
  y       = new((/nouts,ntims /),float)

  bias    = new((/nouts/),float)
  Nobs    = new((/nouts/),integer)
  rse     = new((/nouts/),float)
  xstd    = new((/nouts/),float)
  xavg    = new((/nouts/),float)
  yavg    = new((/nouts/),float)
  skillr  = new((/nouts/),float)
  ratio   = new((/nouts/),float)
  rcof    = new((/nouts/),float)
  rco2    = new((/nouts/),float)
  slop    = new((/nouts/),float)
  yint    = new((/nouts/),float)
  yest    = new((/nouts,ntims/),float)

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
  no2_13  = new((/2,8784/),float)
  no2_14  = new((/2,8784/),float)
  no2_15  = new((/2,8784/),float)
  no2_16  = new((/2,8784/),float)
  no2_17  = new((/2,8784/),float)
 

;--- Observations for one year

  do i= 0,8760-1+st   ; time steps, hourly

      no2_01(1,i) = station_13ST(i-st,0)
      ;if (no2_01(1,i).eq.-99) then
      ;  no2_01(1,i) = avg(station_13ST)
      ;end if
      no2_02(1,i) = station_20VE(i-st,0)
      ;if (no2_02(1,i).eq.-99) then
      ;  no2_02(1,i) = avg(station_20VE)
      ;end if
      no2_03(1,i) = station_61WB(i-st,0)
      ;if (no2_03(1,i).eq.-99) then
      ;  no2_03(1,i) = avg(station_61WB)
      ;end if
      no2_04(1,i) = station_64KS(i-st,0)
      ;if (no2_04(1,i).eq.-99) then
      ;  no2_04(1,i) = avg(station_64KS)
      ;end if
      no2_05(1,i) = station_21BI(i-st,0)
      ;if (no2_05(1,i).eq.-99) then
      ;  no2_05(1,i) = avg(station_21BI)
      ;end if
      no2_06(1,i) = station_24FL(i-st,0)
      ;if (no2_06(1,i).eq.-99) then
      ;  no2_06(1,i) = avg(station_24FL)
      ;end if
      no2_07(1,i) = station_54BL(i-st,0)
      ;if (no2_07(1,i).eq.-99) then
      ;  no2_07(1,i) = avg(station_54BL)
      ;end if
      no2_08(1,i) = station_72FI(i-st,0)
      ;if (no2_08(1,i).eq.-99) then
      ;  no2_08(1,i) = avg(station_72FI)
      ;end if
      no2_09(1,i) = station_27TA(i-st,0)
      ;if (no2_09(1,i).eq.-99) then
      ;  no2_09(1,i) = avg(station_27TA)
      ;end if
      no2_10(1,i) = station_51BF(i-st,0)
      ;if (no2_10(1,i).eq.-99) then
      ;  no2_10(1,i) = avg(station_51BF)
      ;end if
      no2_11(1,i) = station_52NG(i-st,0)
      ;if (no2_11(1,i).eq.-99) then
      ;  no2_11(1,i) = avg(station_52NG)
      ;end if
      no2_12(1,i) = station_74BT(i-st,0)
      ;if (no2_12(1,i).eq.-99) then
      ;  no2_12(1,i) = avg(station_74BT)
      ;end if
      no2_13(1,i) = station_73FW(i-st,0)
      ;if (no2_13(1,i).eq.-99) then
      ;  no2_13(1,i) = avg(station_73FW)
      ;end if
   
  end do



;--- Model output for one month

    do j=0,nstat-1
      do i=0,  24*31-1

        if (j.eq.15)  then    ;13ST
           no2_01(0,i) = outconc(i,j)
        end if
        if (j.eq.18)  then    ;20VE
           no2_02(0,i) = outconc(i,j)
        end if
        if (j.eq.19)  then     ;61WB
           no2_03(0,i) = outconc(i,j)
        end if
        if (j.eq.11)  then     ;64KS
           no2_04(0,i) = outconc(i,j)
        end if
        if (j.eq. 1)  then    ;21BI
           no2_05(0,i) = outconc(i,j)
        end if
        if (j.eq. 7)  then    ;24FL
           no2_06(0,i) = outconc(i,j)
        end if
        if (j.eq. 3)  then     ;54BL
           no2_07(0,i) = outconc(i,j)
        end if
        if (j.eq. 6)  then     ;72FI
           no2_08(0,i) = outconc(i,j)
        end if
;---Background Hamburg stations
        if (j.eq.17)  then     ;27TA
           no2_09(0,i) = outconc(i,j)
        end if
        if (j.eq. 4)  then     ;51BF
           no2_10(0,i) = outconc(i,j)
        end if
        if (j.eq.13)  then     ;52NG
           no2_11(0,i) = outconc(i,j)
        end if
        if (j.eq. 2)  then     ;74BT
           no2_12(0,i) = outconc(i,j)
        end if
        if (j.eq. 5)  then     ;73FW
           no2_13(0,i) = outconc(i,j)
        end if

    end do
  end do


; Shift model data to July (start day-of-year is 182)
     k=182*24

     do i=0, 24*31-1
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
        no2_13(0,k) = no2_13(0,i)
        no2_14(0,k) = no2_14(0,i)
        no2_15(0,k) = no2_15(0,i)
        no2_16(0,k) = no2_16(0,i)
        no2_17(0,k) = no2_17(0,i)
        k=k+1
     end do


; rename array for plotting
     y1  = no2_01
     y2  = no2_02
     y3  = no2_03
     y4  = no2_04
     y5  = no2_05
     y6  = no2_06
     y7  = no2_07
     y8  = no2_08
     y9  = no2_09
     y10 = no2_10
     y11 = no2_11
     y12 = no2_12
     y13 = no2_13
     y14 = no2_14
     y15 = no2_15
     y16 = no2_16
     y17 = no2_17
  
; Shift model data to July (start day-of-year is 182)
      k = 182*24
      do j= 0, ntims -1

;--- Observation
         x(0,j) =  y1(1,k )
         x(1,j) =  y2(1,k ) 
         x(2,j) =  y3(1,k ) 
         x(3,j) =  y4(1,k )
         x(4,j) =  y5(1,k ) 
         x(5,j) =  y6(1,k ) 
         x(6,j) =  y7(1,k ) 
         x(7,j) =  y8(1,k ) 
         x(8,j) =  y9(1,k ) 
         x(9,j) = y10(1,k ) 
         x(10,j)= y11(1,k ) 
         x(11,j)= y12(1,k ) 
         x(12,j)= y13(1,k ) 
         ;x(13,j)= y14(1,k ) 
         ;x(14,j)= y15(1,k ) 
         ;x(15,j)= y16(1,k ) 
         ;x(16,j)= y17(1,k )

;--- Modelling
         y(0,j) =  y1(0,k )
         y(1,j) =  y2(0,k ) 
         y(2,j) =  y3(0,k ) 
         y(3,j) =  y4(0,k )
         y(4,j) =  y5(0,k ) 
         y(5,j) =  y6(0,k ) 
         y(6,j) =  y7(0,k ) 
         y(7,j) =  y8(0,k ) 
         y(8,j) =  y9(0,k ) 
         y(9,j) = y10(0,k ) 
         y(10,j)= y11(0,k ) 
         y(11,j)= y12(0,k ) 
         y(12,j)= y13(0,k ) 
         ;y(13,j)= y14(0,k ) 
         ;y(14,j)= y15(0,k ) 
         ;y(15,j)= y16(0,k ) 
         ;y(16,j)= y17(0,k ) 

         k=k+1

      end do


;--- Missing values of the observations
      x@_FillValue = -99.0


;--- make monthly averages for nouts stations
;--- one value per station
     ; do i=0, nouts-1
     ;     xmonth(i) = 0.0
     ;     ymonth(i) = 0.0
     ;     do j= 0, ntims -1
     ;         xmonth(i) = xmonth(i) + x(i,j)
     ;         ymonth(i) = ymonth(i) + y(i,j)
     ;     end do
     ;     xmonth(i) = xmonth(i)/ntims
     ;     ymonth(i) = ymonth(i)/ntims
     ; end do
      do i=0, nouts-1
          xmonth(i) = avg(x(i,0:ntims-1))
          ymonth(i) = avg(y(i,0:ntims-1))
      end do


;--- make 24-hour averages for nouts stations
;--- max. 31 values per station
      ;do i=0, nouts-1
      ;    day         = 0
      ;    hour        = 0
      ;    xday(i,:) = 0.0
      ;    yday(i,:) = 0.0
      ;    do j= 0, ntims -1
      ;        xday(i,day) = xday(i,day) + x(i,j)
      ;        yday(i,day) = yday(i,day) + y(i,j)   
      ;    
      ;        if ( hour .eq. 23 ) then
      ;           xday(i,day) = xday(i,day)/24
      ;           yday(i,day) = yday(i,day)/24
      ;           day  = day +1
      ;           hour = -1      
      ;         end if
      ;        hour = hour + 1
      ;    end do
      ;end do

      days = ntims/24

      do i=0, nouts-1
        do j = 0, days-1
          day=j
          a = j*24
          b = j*24+23
          xday(i,day) = avg(x(i,a:b))
          yday(i,day) = avg(y(i,a:b))
        end do
      end do

      ;print( xday(1,:) )

;--- collect all 24-hour values in one array
      k   = 0
      do i=0, nouts-1
        do d=0, daym -1
            xdayall(k) = xday(i,d)
            ydayall(k) = yday(i,d)
            k = k +1
        end do
      end do


;--- do the regression MOD vs OBS
     if (Hour) then
;--- hourly values
        rc1     = regline_stats(x(0,0:ntim-1),y(0,0:ntim-1))
        rc2     = regline_stats(x(1,0:ntim-1),y(1,0:ntim-1))
        rc3     = regline_stats(x(2,0:ntim-1),y(2,0:ntim-1))
        rc4     = regline_stats(x(3,0:ntim-1),y(3,0:ntim-1))
        rc5     = regline_stats(x(4,0:ntim-1),y(4,0:ntim-1))
        rc6     = regline_stats(x(5,0:ntim-1),y(5,0:ntim-1))
        rc7     = regline_stats(x(6,0:ntim-1),y(6,0:ntim-1))
        rc8     = regline_stats(x(7,0:ntim-1),y(7,0:ntim-1))
        rc9     = regline_stats(x(8,0:ntim-1),y(8,0:ntim-1))
        rc10    = regline_stats(x(9,0:ntim-1),y(9,0:ntim-1))
        rc11    = regline_stats(x(10,0:ntim-1),y(10,0:ntim-1))
        rc12    = regline_stats(x(11,0:ntim-1),y(11,0:ntim-1))
        rc13    = regline_stats(x(12,0:ntim-1),y(12,0:ntim-1))
        ;rc14    = regline_stats(x(13,0:ntim-1),y(13,0:ntim-1))
        ;rc15    = regline_stats(x(14,0:ntim-1),y(14,0:ntim-1))
        ;rc16    = regline_stats(x(15,0:ntim-1),y(15,0:ntim-1))
        ;rc17    = regline_stats(x(16,0:ntim-1),y(16,0:ntim-1))
     else
;--- daily average
         rcd1    = regline_stats(xday(0,0:daym-1),yday(0,0:daym-1))
         rcd2    = regline_stats(xday(1,0:daym-1),yday(1,0:daym-1))
         rcd3    = regline_stats(xday(2,0:daym-1),yday(2,0:daym-1))
         rcd4    = regline_stats(xday(3,0:daym-1),yday(3,0:daym-1))
         rcd5    = regline_stats(xday(4,0:daym-1),yday(4,0:daym-1))
         rcd6    = regline_stats(xday(5,0:daym-1),yday(5,0:daym-1))
         rcd7    = regline_stats(xday(6,0:daym-1),yday(6,0:daym-1))
         rcd8    = regline_stats(xday(7,0:daym-1),yday(7,0:daym-1))
         rcd9    = regline_stats(xday(8,0:daym-1),yday(8,0:daym-1))
         rcd10   = regline_stats(xday(9,0:daym-1),yday(9,0:daym-1))
         rcd11   = regline_stats(xday(10,0:daym-1),yday(10,0:daym-1))
         rcd12   = regline_stats(xday(11,0:daym-1),yday(11,0:daym-1))
         rcd13   = regline_stats(xday(12,0:daym-1),yday(12,0:daym-1))
         ;rcd14   = regline_stats(xday(13,0:daym-1),yday(13,0:daym-1))
         ;rcd15   = regline_stats(xday(14,0:daym-1),yday(14,0:daym-1))
         ;rcd16   = regline_stats(xday(15,0:daym-1),yday(15,0:daym-1))
         ;rcd17   = regline_stats(xday(16,0:daym-1),yday(16,0:daym-1))
     end if
;--- monthly average (all stations)
       rcmonth = regline_stats(xmonth,ymonth)
;--- 24-hour averages (all stations)
       rcday   = regline_stats(xdayall,ydayall)



;************************************************
;---  PLOTTING SECTION
;************************************************

;---  1) Monthly average MOD versus monthly average OBS, 
;---     all stations in one plot

;--- add two more points to extend to min and max
        xmonth2  = new((/nouts +2/),float)
        xmonth2(0:nouts-1) = xmonth(0:nouts-1)
        xmonth2(nouts)     =  0.0  ;min
        xmonth2(nouts+1)   = 90.0  ;max
        nx   = dimsizes(xmonth2)
        pltarry      = new ( (/8,nx/), typeof(xmonth2))
        pltarry(0,0:nouts-1) = ymonth(0:nouts-1)
        pltarry(1,0:nouts-1) = rcmonth@Yest(0:nouts-1)
;---     one-to-one line
        do i=0, nouts-1
          pltarry(2,i) = xmonth(i)
        end do 
        pltarry(2,nouts)   =  0.0  ;min
        pltarry(2,nouts+1) = 90.0  ;max


;---  2) 24-hour averrage MOD versus 24-hour average OBS,
;---     all stations and days in one plot
;--- add two more points to extend to min and max
        xdayall2 = new((/nouts*daym +2/),float)
        xdayall2(0:nouts*daym-1) = xdayall(0:nouts*daym-1)
        xdayall2(nouts*daym)     =  0.0  ;min
        xdayall2(nouts*daym+1)   = 90.0  ;max
        nxd  = dimsizes(xdayall2)
        pltarryd      = new ( (/8,nxd/), typeof(xdayall2))
        pltarryd(0,0:nouts*daym-1) = ydayall(0:nouts*daym-1)
        pltarryd(1,0:nouts*daym-1) = rcday@Yest(0:nouts*daym-1)
;---     one-to-one line
        do i=0, nouts*daym-1
          pltarryd(2,i) = xdayall(i)
        end do 
        pltarryd(2,nouts*daym)   =  0.0  ;min
        pltarryd(2,nouts*daym+1) = 90.0  ;max
;

;************************************************
;---  REGRESSION PLOTS
;************************************************
   
        res                     = True                   ; plot mods desired
        res@gsnMaximize         = True
        res@gsnDraw             = False
        res@gsnFrame            = False
        res@xyMarkLineModes     = (/"Markers","Lines" \  ; choose which have markers
                               ,"Lines"  ,"Lines" \ 
                               ,"Lines"  ,"Lines" \ 
                               ,"Lines"  ,"Lines" /)
        res@xyMarkers           = 16                     ; choose type of marker 
        res@xyMarkerSizeF       = 0.0085                 ; Marker size (default 0.01)

        res@xyDashPatterns      = 0                      ; solid line 
        res@xyLineThicknesses   = (/1,3,2,2,2,2,1,1/)     
        res@xyLineColors        = (/ "black", "green" \
                               , "blue" , "blue"  \
                               , "red"  , "red"   \
                               , "green", "green" /)

;--- same max value in x- and y-direction
        res@trXMinF             =   0.0
        res@trYMinF             =   0.0

          res@trXMaxF           =   80.0
          res@trYMaxF           =   80.0
          dayx                  =   40
          dayy1                 =   10
          dayy2                 =   5
          txtpos                =   47
          r2pos                 =   3

        res@tmYLFormat          = "f"               
         ; not necessary but nicer labels 

        res@tiYAxisString          = "Mod (CityChem)"
        res@tiXAxisString          = "Obs"

;--- Write annotations into the plot
        res_text                   = True                  ; text mods desired
        res_text@txFontHeightF     = 0.020                 ; change text size
        res_text@txJust            = "CenterLeft"          ; text justification


        res@tiMainString           = "Spatial stats "+pollname+", monthly avg."
        plot1  = gsn_csm_xy (wks1, xmonth2 , pltarry(0:2,:), res)                 ; create plot
        text1  = gsn_add_text(wks1, plot1, "y = "+tostring(rcmonth) + "x + "+tostring(rcmonth@yintercept) , 5, 75,res_text)
        text2  = gsn_add_text(wks1, plot1, "R~S~2~N~  =" +tostring(rcmonth@r2) , 5, 70,res_text)

        draw(plot1)
        frame(wks1)

;;;; 2) 24-hour averrage MOD versus 24-hour average OBS,
;;;;    all stations and days in one plot

        res@tiMainString           = "Spatio-temporal stats "+pollname+", 24h avg."
        plot2 = gsn_csm_xy (wks2, xdayall2 , pltarryd(0:2,:), res)                 ; create plot
        text1  = gsn_add_text(wks2, plot2, "y = "+tostring(rcday) + "x + "+tostring(rcday@yintercept) ,dayx,dayy1, res_text)
        text2  = gsn_add_text(wks2, plot2, "R~S~2~N~  =" +tostring(rcday@r2) ,dayx,dayy2,   res_text)
    
        draw(plot2) 
        frame(wks2)



;************************************************
;---  STATISTICS OVERVIEW SECTION
;************************************************

;--- calculate BIAS as MOD(i) - OBS(i)

        do i=0, nouts-1

          bias(i) = 0.0
; sum up measurements per station
          Nobs(i) = 0
          if (Hour) then
            do j= 0, ntims-1
              if (.not.ismissing(x(i,j)) ) then
                bias(i) = bias(i) + (y(i,j) - x(i,j))
                Nobs(i) = Nobs(i) + 1
              end if
            end do
          else
            do j= 0, daym-1
              if (xday(i,j) .ge. 0.0 ) then
                bias(i) = bias(i) + (yday(i,j) - xday(i,j))
                Nobs(i) = Nobs(i) + 1
              end if
            end do
          end if
          bias(i) = bias(i) / Nobs(i)

        end do
        ;print( Nobs )
        ;print( bias )

      if (Hour) then
;--- extract RSE of nbouts stations:
          rse(0)  = tofloat(rc1@RSE)
          rse(1)  = tofloat(rc2@RSE)
          rse(2)  = tofloat(rc3@RSE)
          rse(3)  = tofloat(rc4@RSE)
          rse(4)  = tofloat(rc5@RSE)
          rse(5)  = tofloat(rc6@RSE)
          rse(6)  = tofloat(rc7@RSE)
          rse(7)  = tofloat(rc8@RSE)
          rse(8)  = tofloat(rc9@RSE)
          rse(9)  = tofloat(rc10@RSE)
          rse(10) = tofloat(rc11@RSE)
          rse(11) = tofloat(rc12@RSE)
          rse(12) = tofloat(rc13@RSE)
          ;rse(13) = tofloat(rc14@RSE)
          ;rse(14) = tofloat(rc15@RSE)
          ;rse(15) = tofloat(rc16@RSE)
          ;rse(16) = tofloat(rc17@RSE)

;--- extract standard deviation of x: Xstd
          xstd(0)  = tofloat(rc1@Xstd)
          xstd(1)  = tofloat(rc2@Xstd)
          xstd(2)  = tofloat(rc3@Xstd)
          xstd(3)  = tofloat(rc4@Xstd)
          xstd(4)  = tofloat(rc5@Xstd)
          xstd(5)  = tofloat(rc6@Xstd)
          xstd(6)  = tofloat(rc7@Xstd)
          xstd(7)  = tofloat(rc8@Xstd)
          xstd(8)  = tofloat(rc9@Xstd)
          xstd(9)  = tofloat(rc10@Xstd)
          xstd(10) = tofloat(rc11@Xstd)
          xstd(11) = tofloat(rc12@Xstd)
          xstd(12) = tofloat(rc13@Xstd)
          ;xstd(13) = tofloat(rc14@Xstd)
          ;xstd(14) = tofloat(rc15@Xstd)
          ;xstd(15) = tofloat(rc16@Xstd)
          ;xstd(16) = tofloat(rc17@Xstd)

          ratio(0) = tofloat(rc1@Ystd)/tofloat(rc1@Xstd)
          ratio(1) = tofloat(rc2@Ystd)/tofloat(rc2@Xstd)
          ratio(2) = tofloat(rc3@Ystd)/tofloat(rc3@Xstd)
          ratio(3) = tofloat(rc4@Ystd)/tofloat(rc4@Xstd)
          ratio(4) = tofloat(rc5@Ystd)/tofloat(rc5@Xstd)
          ratio(5) = tofloat(rc6@Ystd)/tofloat(rc6@Xstd)
          ratio(6) = tofloat(rc7@Ystd)/tofloat(rc7@Xstd)
          ratio(7) = tofloat(rc8@Ystd)/tofloat(rc8@Xstd)
          ratio(8) = tofloat(rc9@Ystd)/tofloat(rc9@Xstd)
          ratio(9) = tofloat(rc10@Ystd)/tofloat(rc10@Xstd)
          ratio(10)= tofloat(rc11@Ystd)/tofloat(rc11@Xstd)
          ratio(11)= tofloat(rc12@Ystd)/tofloat(rc12@Xstd)
          ratio(12)= tofloat(rc13@Ystd)/tofloat(rc13@Xstd)
          ;ratio(13)= tofloat(rc14@Ystd)/tofloat(rc14@Xstd)
          ;ratio(14)= tofloat(rc15@Ystd)/tofloat(rc15@Xstd)
          ;ratio(15)= tofloat(rc16@Ystd)/tofloat(rc16@Xstd)
          ;ratio(16)= tofloat(rc17@Ystd)/tofloat(rc17@Xstd)
     else
;--- extract RSE of nbouts stations:
          rse(0)  = tofloat(rcd1@RSE)
          rse(1)  = tofloat(rcd2@RSE)
          rse(2)  = tofloat(rcd3@RSE)
          rse(3)  = tofloat(rcd4@RSE)
          rse(4)  = tofloat(rcd5@RSE)
          rse(5)  = tofloat(rcd6@RSE)
          rse(6)  = tofloat(rcd7@RSE)
          rse(7)  = tofloat(rcd8@RSE)
          rse(8)  = tofloat(rcd9@RSE)
          rse(9)  = tofloat(rcd10@RSE)
          rse(10) = tofloat(rcd11@RSE)
          rse(11) = tofloat(rcd12@RSE)
          rse(12) = tofloat(rcd13@RSE)
          ;rse(13) = tofloat(rcd14@RSE)
          ;rse(14) = tofloat(rcd15@RSE)
          ;rse(15) = tofloat(rcd16@RSE)
          ;rse(16) = tofloat(rcd17@RSE)

;--- extract standard deviation of x: Xstd
           xstd(0)  = tofloat(rcd1@Xstd)
           xstd(1)  = tofloat(rcd2@Xstd)
           xstd(2)  = tofloat(rcd3@Xstd)
           xstd(3)  = tofloat(rcd4@Xstd)
           xstd(4)  = tofloat(rcd5@Xstd)
           xstd(5)  = tofloat(rcd6@Xstd)
           xstd(6)  = tofloat(rcd7@Xstd)
           xstd(7)  = tofloat(rcd8@Xstd)
           xstd(8)  = tofloat(rcd9@Xstd)
           xstd(9)  = tofloat(rcd10@Xstd)
           xstd(10) = tofloat(rcd11@Xstd)
           xstd(11) = tofloat(rcd12@Xstd)
           xstd(12) = tofloat(rcd13@Xstd)
           ;xstd(13) = tofloat(rcd14@Xstd)
           ;xstd(14) = tofloat(rcd15@Xstd)
           ;xstd(15) = tofloat(rcd16@Xstd)
           ;xstd(16) = tofloat(rcd17@Xstd)

           ratio(0) = tofloat(rcd1@Ystd)/tofloat(rcd1@Xstd)
           ratio(1) = tofloat(rcd2@Ystd)/tofloat(rcd2@Xstd)
           ratio(2) = tofloat(rcd3@Ystd)/tofloat(rcd3@Xstd)
           ratio(3) = tofloat(rcd4@Ystd)/tofloat(rcd4@Xstd)
           ratio(4) = tofloat(rcd5@Ystd)/tofloat(rcd5@Xstd)
           ratio(5) = tofloat(rcd6@Ystd)/tofloat(rcd6@Xstd)
           ratio(6) = tofloat(rcd7@Ystd)/tofloat(rcd7@Xstd)
           ratio(7) = tofloat(rcd8@Ystd)/tofloat(rcd8@Xstd)
           ratio(8) = tofloat(rcd9@Ystd)/tofloat(rcd9@Xstd)
           ratio(9) = tofloat(rcd10@Ystd)/tofloat(rcd10@Xstd)
           ratio(10)= tofloat(rcd11@Ystd)/tofloat(rcd11@Xstd)
           ratio(11)= tofloat(rcd12@Ystd)/tofloat(rcd12@Xstd)
           ratio(12)= tofloat(rcd13@Ystd)/tofloat(rcd13@Xstd)
           ;ratio(13)= tofloat(rcd14@Ystd)/tofloat(rcd14@Xstd)
           ;ratio(14)= tofloat(rcd15@Ystd)/tofloat(rcd15@Xstd)
           ;ratio(15)= tofloat(rcd16@Ystd)/tofloat(rcd16@Xstd)
           ;ratio(16)= tofloat(rcd17@Ystd)/tofloat(rcd17@Xstd)

    end if

    ;print(rse)
    ;print(xstd)


;--- calculate SKILLr

       do i=0, nouts-1
         skillr(i) = rse(i) / xstd(i)
       end do

      if (Hour) then
;--- extract average of x: Xavg
           xavg(0)  = tofloat(rc1@Xavg)
           xavg(1)  = tofloat(rc2@Xavg)
           xavg(2)  = tofloat(rc3@Xavg)
           xavg(3)  = tofloat(rc4@Xavg)
           xavg(4)  = tofloat(rc5@Xavg)
           xavg(5)  = tofloat(rc6@Xavg)
           xavg(6)  = tofloat(rc7@Xavg)
           xavg(7)  = tofloat(rc8@Xavg)
           xavg(8)  = tofloat(rc9@Xavg)
           xavg(9)  = tofloat(rc10@Xavg)
           xavg(10) = tofloat(rc11@Xavg)
           xavg(11) = tofloat(rc12@Xavg)
           xavg(12) = tofloat(rc13@Xavg)
           ;xavg(13) = tofloat(rc14@Xavg)
           ;xavg(14) = tofloat(rc15@Xavg)
           ;xavg(15) = tofloat(rc16@Xavg)
           ;xavg(16) = tofloat(rc17@Xavg)
;--- extract average of y: Yavg
           yavg(0)  = tofloat(rc1@Yavg)
           yavg(1)  = tofloat(rc2@Yavg)
           yavg(2)  = tofloat(rc3@Yavg)
           yavg(3)  = tofloat(rc4@Yavg)
           yavg(4)  = tofloat(rc5@Yavg)
           yavg(5)  = tofloat(rc6@Yavg)
           yavg(6)  = tofloat(rc7@Yavg)
           yavg(7)  = tofloat(rc8@Yavg)
           yavg(8)  = tofloat(rc9@Yavg)
           yavg(9)  = tofloat(rc10@Yavg)
           yavg(10) = tofloat(rc11@Yavg)
           yavg(11) = tofloat(rc12@Yavg)
           yavg(12) = tofloat(rc13@Yavg)
           ;yavg(13) = tofloat(rc14@Yavg)
           ;yavg(14) = tofloat(rc15@Yavg)
           ;yavg(15) = tofloat(rc16@Yavg)
           ;yavg(16) = tofloat(rc17@Yavg)
       else
;--- extract average of x: Xavg
           xavg(0)  = tofloat(rcd1@Xavg)
           xavg(1)  = tofloat(rcd2@Xavg)
           xavg(2)  = tofloat(rcd3@Xavg)
           xavg(3)  = tofloat(rcd4@Xavg)
           xavg(4)  = tofloat(rcd5@Xavg)
           xavg(5)  = tofloat(rcd6@Xavg)
           xavg(6)  = tofloat(rcd7@Xavg)
           xavg(7)  = tofloat(rcd8@Xavg)
           xavg(8)  = tofloat(rcd9@Xavg)
           xavg(9)  = tofloat(rcd10@Xavg)
           xavg(10) = tofloat(rcd11@Xavg)
           xavg(11) = tofloat(rcd12@Xavg)
           xavg(12) = tofloat(rcd13@Xavg)
           ;xavg(13) = tofloat(rcd14@Xavg)
           ;xavg(14) = tofloat(rcd15@Xavg)
           ;xavg(15) = tofloat(rcd16@Xavg)
           ;xavg(16) = tofloat(rcd17@Xavg)

;--- extract average of y: Yavg
           yavg(0)  = tofloat(rcd1@Yavg)
           yavg(1)  = tofloat(rcd2@Yavg)
           yavg(2)  = tofloat(rcd3@Yavg)
           yavg(3)  = tofloat(rcd4@Yavg)
           yavg(4)  = tofloat(rcd5@Yavg)
           yavg(5)  = tofloat(rcd6@Yavg)
           yavg(6)  = tofloat(rcd7@Yavg)
           yavg(7)  = tofloat(rcd8@Yavg)
           yavg(8)  = tofloat(rcd9@Yavg)
           yavg(9)  = tofloat(rcd10@Yavg)
           yavg(10) = tofloat(rcd11@Yavg)
           yavg(11) = tofloat(rcd12@Yavg)
           yavg(12) = tofloat(rcd13@Yavg)
           ;yavg(13) = tofloat(rcd14@Yavg)
           ;yavg(14) = tofloat(rcd15@Yavg)
           ;yavg(15) = tofloat(rcd16@Yavg)
           ;yavg(16) = tofloat(rcd17@Yavg)
      end if

      if (Hour) then
;--- extract correlation coefficient: r
           rcof(0)  = tofloat(rc1@r)
           rcof(1)  = tofloat(rc2@r)
           rcof(2)  = tofloat(rc3@r)
           rcof(3)  = tofloat(rc4@r)
           rcof(4)  = tofloat(rc5@r)
           rcof(5)  = tofloat(rc6@r)
           rcof(6)  = tofloat(rc7@r)
           rcof(7)  = tofloat(rc8@r)
           rcof(8)  = tofloat(rc9@r)
           rcof(9)  = tofloat(rc10@r)
           rcof(10) = tofloat(rc11@r)
           rcof(11) = tofloat(rc12@r)
           rcof(12) = tofloat(rc13@r)
           ;rcof(13) = tofloat(rc14@r)
           ;rcof(14) = tofloat(rc15@r)
           ;rcof(15) = tofloat(rc16@r)
           ;rcof(16) = tofloat(rc17@r)
;--- extract correlation coefficient: r2
           rco2(0)  = tofloat(rc1@r2)
           rco2(1)  = tofloat(rc2@r2)
           rco2(2)  = tofloat(rc3@r2)
           rco2(3)  = tofloat(rc4@r2)
           rco2(4)  = tofloat(rc5@r2)
           rco2(5)  = tofloat(rc6@r2)
           rco2(6)  = tofloat(rc7@r2)
           rco2(7)  = tofloat(rc8@r2)
           rco2(8)  = tofloat(rc9@r2)
           rco2(9)  = tofloat(rc10@r2)
           rco2(10) = tofloat(rc11@r2)
           rco2(11) = tofloat(rc12@r2)
           rco2(12) = tofloat(rc13@r2)
           ;rco2(13) = tofloat(rc14@r2)
           ;rco2(14) = tofloat(rc15@r2)
           ;rco2(15) = tofloat(rc16@r2)
           ;rco2(16) = tofloat(rc17@r2)
      else
;--- extract correlation coefficient: r
           rcof(0)  = tofloat(rcd1@r)
           rcof(1)  = tofloat(rcd2@r)
           rcof(2)  = tofloat(rcd3@r)
           rcof(3)  = tofloat(rcd4@r)
           rcof(4)  = tofloat(rcd5@r)
           rcof(5)  = tofloat(rcd6@r)
           rcof(6)  = tofloat(rcd7@r)
           rcof(7)  = tofloat(rcd8@r)
           rcof(8)  = tofloat(rcd9@r)
           rcof(9)  = tofloat(rcd10@r)
           rcof(10) = tofloat(rcd11@r)
           rcof(11) = tofloat(rcd12@r)
           rcof(12) = tofloat(rcd13@r)
           ;rcof(13) = tofloat(rcd14@r)
           ;rcof(14) = tofloat(rcd15@r)
           ;rcof(15) = tofloat(rcd16@r)
           ;rcof(16) = tofloat(rcd17@r)

;--- extract correlation coefficient: r2
           rco2(0)  = tofloat(rcd1@r2)
           rco2(1)  = tofloat(rcd2@r2)
           rco2(2)  = tofloat(rcd3@r2)
           rco2(3)  = tofloat(rcd4@r2)
           rco2(4)  = tofloat(rcd5@r2)
           rco2(5)  = tofloat(rcd6@r2)
           rco2(6)  = tofloat(rcd7@r2)
           rco2(7)  = tofloat(rcd8@r2)
           rco2(8)  = tofloat(rcd9@r2)
           rco2(9)  = tofloat(rcd10@r2)
           rco2(10) = tofloat(rcd11@r2)
           rco2(11) = tofloat(rcd12@r2)
           rco2(12) = tofloat(rcd13@r2)
           ;rco2(13) = tofloat(rcd14@r2)
           ;rco2(14) = tofloat(rcd15@r2)
           ;rco2(15) = tofloat(rcd16@r2)
           ;rco2(16) = tofloat(rcd17@r2)
      end if


      if (Hour) then
;--- extract y-axis intercept: yintercept
           yint(0)  = tofloat(rc1@yintercept)
           yint(1)  = tofloat(rc2@yintercept)
           yint(2)  = tofloat(rc3@yintercept)
           yint(3)  = tofloat(rc4@yintercept)
           yint(4)  = tofloat(rc5@yintercept)
           yint(5)  = tofloat(rc6@yintercept)
           yint(6)  = tofloat(rc7@yintercept)
           yint(7)  = tofloat(rc8@yintercept)
           yint(8)  = tofloat(rc9@yintercept)
           yint(9)  = tofloat(rc10@yintercept)
           yint(10) = tofloat(rc11@yintercept)
           yint(11) = tofloat(rc12@yintercept)
           yint(12) = tofloat(rc13@yintercept)
           ;yint(13) = tofloat(rc14@yintercept)
           ;yint(14) = tofloat(rc15@yintercept)
           ;yint(15) = tofloat(rc16@yintercept)
           ;yint(16) = tofloat(rc17@yintercept)
;--- extract slope: b(1)
           slop(0)  = tofloat(rc1@b(1))
           slop(1)  = tofloat(rc2@b(1))
           slop(2)  = tofloat(rc3@b(1))
           slop(3)  = tofloat(rc4@b(1))
           slop(4)  = tofloat(rc5@b(1))
           slop(5)  = tofloat(rc6@b(1))
           slop(6)  = tofloat(rc7@b(1))
           slop(7)  = tofloat(rc8@b(1))
           slop(8)  = tofloat(rc9@b(1))
           slop(9)  = tofloat(rc10@b(1))
           slop(10) = tofloat(rc11@b(1))
           slop(11) = tofloat(rc12@b(1))
           slop(12) = tofloat(rc13@b(1))
           ;slop(13) = tofloat(rc14@b(1))
           ;slop(14) = tofloat(rc15@b(1))
           ;slop(15) = tofloat(rc16@b(1))
           ;slop(16) = tofloat(rc17@b(1))
      else
;--- extract y-axis intercept: yintercept
           yint(0)  = tofloat(rcd1@yintercept)
           yint(1)  = tofloat(rcd2@yintercept)
           yint(2)  = tofloat(rcd3@yintercept)
           yint(3)  = tofloat(rcd4@yintercept)
           yint(4)  = tofloat(rcd5@yintercept)
           yint(5)  = tofloat(rcd6@yintercept)
           yint(6)  = tofloat(rcd7@yintercept)
           yint(7)  = tofloat(rcd8@yintercept)
           yint(8)  = tofloat(rcd9@yintercept)
           yint(9)  = tofloat(rcd10@yintercept)
           yint(10) = tofloat(rcd11@yintercept)
           yint(11) = tofloat(rcd12@yintercept)
           yint(12) = tofloat(rcd13@yintercept)
           ;yint(13) = tofloat(rcd14@yintercept)
           ;yint(14) = tofloat(rcd15@yintercept)
           ;yint(15) = tofloat(rcd16@yintercept)
           ;yint(16) = tofloat(rcd17@yintercept)
;--- extract slope: b(1)
           slop(0)  = tofloat(rcd1@b(1))
           slop(1)  = tofloat(rcd2@b(1))
           slop(2)  = tofloat(rcd3@b(1))
           slop(3)  = tofloat(rcd4@b(1))
           slop(4)  = tofloat(rcd5@b(1))
           slop(5)  = tofloat(rcd6@b(1))
           slop(6)  = tofloat(rcd7@b(1))
           slop(7)  = tofloat(rcd8@b(1))
           slop(8)  = tofloat(rcd9@b(1))
           slop(9)  = tofloat(rcd10@b(1))
           slop(10) = tofloat(rcd11@b(1))
           slop(11) = tofloat(rcd12@b(1))
           slop(12) = tofloat(rcd13@b(1))
           ;slop(13) = tofloat(rcd14@b(1))
           ;slop(14) = tofloat(rcd15@b(1))
           ;slop(15) = tofloat(rcd16@b(1))
           ;slop(16) = tofloat(rcd17@b(1))
      end if


     print( "write list of statistics for stations")
     print ( Hour )


;--- print list of statistics for stations

;--- write statistics for nouts stations to ascii file

     slabel = (/ "13ST","20VE","61WB","64KS","21BI","24FL","54BL","72FI","27TA","51BF","52NG","74BT","73FW" /)
     header = (/"station  xavg    yavg     bias     rse     corr-r   corr-r2   skill   ratio" /)
     hlist = [/header/]
     write_table( fiouta , "w", hlist, "%s")

     do n=0, nouts-1

         xlist = [/ slabel(n), xavg(n), yavg(n), bias(n), rse(n), rcof(n), rco2(n), skillr(n), ratio(n)  /] 

         write_table( fiouta , "a", xlist, "%s %8.3f %8.3f %8.3f %8.3f %8.3f %8.3f %8.3f %8.3f"  )

     end do

;--- write time series for nouts stations to ascii file

     ftpathstr = (/ filepath,"hh_",poll,"_timeseries_",month,".txt"  /)
     ftouta = str_concat(ftpathstr)

     header2 = (/ "13STo   13STm   20VEo   20VEm   61WBo   61WBm   64KSo   64KSm   21BIo   21BIm   24FLo   24FLm   54BLo   54BLm   72FIo   72FIm   27TAo   27TAm   51BFo   51BFm   52NGo   52NGm   74BTo   74BTm   73FWo   73FWm" /)
     hlist2 = [/header2/]
     write_table( ftouta , "w", hlist2, "%s")

     do j= 0, ntims-1

           xlist = [/ x(0,j),y(0,j),  x(1,j),y(1,j),  x(2,j),y(2,j),  x(3,j),y(3,j),  \
                      x(4,j),y(4,j),  x(5,j),y(5,j),  x(6,j),y(6,j),  x(7,j),y(7,j),  \
                      x(8,j),y(8,j),  x(9,j),y(9,j),  x(10,j),y(10,j),  x(11,j),y(11,j),  x(12,j),y(12,j)    /] 

           write_table( ftouta , "a", xlist, "%8.3f %8.3f %8.3f %8.3f %8.3f %8.3f %8.3f %8.3f %8.3f %8.3f %8.3f %8.3f %8.3f %8.3f %8.3f %8.3f %8.3f %8.3f %8.3f %8.3f %8.3f %8.3f %8.3f %8.3f %8.3f %8.3f"  )

     end do

     end

;;;; END statistics.ncl
