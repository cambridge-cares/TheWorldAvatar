;----------------------------------------------------------------------
; create_map.ncl
;
;       - Creates a lat-lon map of CityChem 2D concentration output
;       - User can edit concentration range to display
;       - Plots map of the receptor point raster (100x100 m^2)
;       - Adds the water body shape as polygon
;       - Adds the model concentrations at stations as circles
;
;
;----------------------------------------------------------------------
; Inspired by the following examples from NCL website:
;   http://www.ncl.ucar.edu/Applications/Scripts/unique_8.ncl
;   http://www.ncl.ucar.edu/Applications/Scripts/station_2.ncl
;   https://www.ncl.ucar.edu/Applications/Scripts/shapefiles_1.ncl
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


;---- User Edit Section ---------------------------------

;**  select scenario
    scenario      = ""
    ;scenario      = "reference"

;**  simulation period
    month        = "201307" 
    ;month        = "201301"

;**  arcgis shapefile provided
    shape        = True
    ;shape        = False

;**  select species
    species       = "no2"
    ;species       = "no"
    ;species       = "o3"
    ;species       = "pm25"

;**  map grafics title
     ftitle        = "CityChem NO~B~2~N~ Hamburg, Jul 2013 " + scenario
      ;ftitle        = "CityChem NO Hamburg, Jul 2013 " + scenario
      ;ftitle        = "CityChem O~B~3~N~ Hamburg, Jul 2013 " + scenario
      ;ftitle        = "CityChem PM~B~2.5~N~ Hamburg, Jul 2013 " + scenario

;**  concentration level in map plot
;**     try: (maxlev-minlev)/stepl >12
    minlev = 5.0
    ;minlev =  4.0    ;o3
    ;minlev =  2.0    ;pm25

    maxlev = 70.0    ;no2
    ;maxlev = 90.0    ;no
    ;maxlev = 56.0    ;o3
    ;maxlev = 20.0    ;pm25

    stepl  =  5.0    ;no2; o3
    ;stepl  =  8.0    ;no
    ;stepl  =  2.0    ;pm25
 
;**  receptor output path
    pointfilepath   = "../OUTPUT/"

;**  station output path
    monitorfilepath = "../OUTPUT/"

;**  map grafics path   
    fplotpath       = "../GRAFICS/" 

;** receptor points
    receptorfile = "../INPUT/other/receptor_stations_raster.txt"

;** Origin Hamburg
;** X   551750 ...  580750
;** Y  5919656 ... 5957656
    utzo = "32N"
    xmin =  551750
    ymin = 5919656

;---- end user edit  ------------------------------------

;**  map grafics colorbar
;**  needs to have more than 100 colors;
    ;colormap = "BlAqGrYeOrReVi200"   ;best
    colormap = "NCV_jet"
    ;colormap = "MPL_jet"
    ;colormap = "NCV_bright"

;---- Setup of domain   ---------------------------------
;--------------------------------------------------------

    cmax = 30
    pmax = 300
    nstat= 20
    rmax = 90000 + nstat
    dx   = 1000.
    dy   = 1000.
    xmax =  xmin + cmax*dx
    ymax =  ymin + cmax*dy

;---- Define wkn and arrays  ----------------------------
;--------------------------------------------------------

;** Define some arrays

    lat2d  = new((/cmax,cmax/),  float)
    lon2d  = new((/cmax,cmax/),  float)
    conc   = new((/cmax,cmax/),  float)
    concn  = new((/cmax,cmax/),  float)

    latf2d = new((/pmax,pmax/),  float)
    lonf2d = new((/pmax,pmax/),  float)
    cnew   = new((/pmax,pmax/),  float)

    crep   = new((/rmax/),       float)  
    latr2d = new((/pmax,pmax/),  float)
    lonr2d = new((/pmax,pmax/),  float)
    crnet  = new((/pmax,pmax/),  float)
    crnew  = new((/pmax,pmax/),  float)

    cstat    = new((/nstat,nstat/), float) 
    stat_lat = new((/nstat/),    float)  
    stat_lon = new((/nstat/),    float)  
    stat_cr  = new((/nstat/),    float) 
    stat_col = new((/nstat/),   string) 
    colors   = new((/nstat/),    float) 

; Define file names
    pointstr       = (/ pointfilepath,   "statrecp.nc" /)
    monitstr       = (/ monitorfilepath, "statmoni.nc" /)
    fplotstr       = (/ fplotpath,    "hamburg_map_",species,"_",scenario,"_", month /)
             
    pointfilename  = str_concat(pointstr)
    monitfilename  = str_concat(monitstr)
    fplotname      = str_concat(fplotstr)


;** open a workstation
    wtype          = "png"
    wtype@wkWidth  = 1800     ; Increase size for a slightly
    wtype@wkHeight = 1800     ; better looking PNG.
    wks            = gsn_open_wks( wtype, fplotname )
    gsn_define_colormap(wks, colormap) ; choose colormap

;---- Build UTM grids        ----------------------------
;--------------------------------------------------------

;** build utm grid
;** UTM input grid for 30x30 cells, 1km
;** UTM plot  grid for 300x300 = 90 000 cells, 100m

;** UTM East-West direction
    utmx  = fspan( xmin, xmax , cmax)
    utmxf = fspan( xmin, xmax , pmax)

;** UTM North-South direction
    utmy  = fspan( ymin, ymax, cmax)
    utmyf = fspan( ymin, ymax, pmax)

;** now define x- and y- coordinate
;** with the grid coordinates in lat-lon

    do i = 0, cmax -1
       do j = 0, cmax -1

         utmxy  = (/ utmx(j), utmy(i) /)
         utmxy@grid_zone = utzo
         latlon = utm2latlon( utmxy, 2 )     ;WGS_84_DATUM
         lat2d(i,j) = latlon(0)
         lon2d(i,j) = latlon(1)

       end do
    end do

    do i = 0, pmax -1
       do j = 0, pmax -1

         utmxyf = (/ utmxf(j), utmyf(i) /)
         utmxyf@grid_zone = utzo
         latlonf = utm2latlon(utmxyf,2)     ;WGS_84_DATUM
         latf2d(i,j) = latlonf(0)
         lonf2d(i,j) = latlonf(1)

       end do
    end do



;--------------------------------------------------------
;---- Read receptor raster grid -------------------------

;** read in file as array of strings so we can parse each line
    ncols=1
    ;; three header lines
    lines_mod = readAsciiTable(receptorfile,ncols,"string",3)

    delim_val = "	"

    xcol      =  str_get_field(lines_mod,2,delim_val)
    ycol      =  str_get_field(lines_mod,3,delim_val)

    xpoint    = tofloat( xcol(:,0) )
    ypoint    = tofloat( ycol(:,0) )


;--------------------------------------------------------
;---- Read receptor conc. output ------------------------
;--------------------------------------------------------

;** define receptors coordinates in lat-lon grid
;** start at point 21

    k=20
    do i = 0, pmax -1
       do j = 0, pmax -1

         utmxyr = (/ xpoint(k), ypoint(k) /)
         utmxyr@grid_zone = utzo
         latlonr = utm2latlon(utmxyr,2)     ;WGS_84_DATUM
         latr2d(i,j) = latlonr(0)
         lonr2d(i,j) = latlonr(1)
;; ascii        crnew(i,j)  = crep(k)
         k=k+1

       end do
    end do

        ;  print ( min(lonr2d) )

;--------------------------------------------------------
;---- NetCDF FORMAT          ----------------------------

      pointc    = addfile ( pointfilename, "r")
      pNames = getfilevarnames (pointc) ; get names of all variables on file

      ;print(pNames)
      ;exit

      if (species.eq."no2") then
         crnet    = pointc->NO2(time|:,j|:,i|:)
      else if (species.eq."no") then
         crnet    = pointc->NO(time|:,j|:,i|:)
      else if (species.eq."o3") then
         crnet    = pointc->O3(time|:,j|:,i|:)
      else if (species.eq."hcho") then
         crnet    = pointc->HCHO(time|:,j|:,i|:)
      else if (species.eq."pm25") then
         crnet    = pointc->PM25(time|:,j|:,i|:)
      else if (species.eq."so2") then
         crnet    = pointc->SO2(time|:,j|:,i|:)
      end if
      end if
      end if
      end if
      end if
      end if

        scfr      = tofloat(crnet@scale_factor)
        offr      = tofloat(crnet@add_offset)

;**  netcdf grid output

      do j=0, pmax-1
        do i=0, pmax-1

           crnew(i,j) = crnet(i,j) * scfr(0)  + offr(0) 

        end do 
      end do

;--------------------------------------------------------
;---- NetCDF FORMAT          ----------------------------

      monitc   = addfile ( monitfilename, "r")

      if (species.eq."no2") then
         cstat    = monitc->NO2(time|:,j|:,i|:)
      else if (species.eq."no") then
         cstat    = monitc->NO(time|:,j|:,i|:)
      else if (species.eq."o3") then
         cstat    = monitc->O3(time|:,j|:,i|:)
      else if (species.eq."hcho") then
         cstat    = monitc->HCHO(time|:,j|:,i|:)
      else if (species.eq."pm25") then
         cstat    = monitc->PM25(time|:,j|:,i|:)
      else if (species.eq."so2") then
         cstat    = monitc->SO2(time|:,j|:,i|:)
      end if
      end if
      end if
      end if
      end if
      end if

      ;no 'packing'
      scfs      =  tofloat(cstat@scale_factor)   ; 1.0
      offs      =  tofloat(cstat@add_offset)     ; 0.0

 
      k=0
      do j=0, nstat-1
        do i=0, nstat-1

           if (i.eq.j) then
              stat_cr(k) = cstat(j,i) * scfs(0)  + offs(0)
              k=k+1
           end if

        end do 
      end do


;---- Choose color map       ----------------------------
;--------------------------------------------------------

      cmap = read_colormap_file(colormap)

;************************************************
; create plot
;************************************************

    res                        = True               ; plot mods desired
    res@tiMainString           = ftitle
    res@cnFillOn               = True               ; color fill
    res@lbLabelBarOn           = False              ; Will turn off the label bar. 
    res@cnFillPalette          = cmap(2:,:)         ; subset the color map  (skips the first color)
    res@cnLinesOn              = False              ; no contour lines
    res@cnLineLabelsOn         = False              ; no contour labels
    res@cnInfoLabelOn          = False              ; no contour info label
    res@pmTickMarkDisplayMode  = "Always"           ; turn on tickmarks
    res@tiMainFontHeightF      = 0.020              ; smaller title
    res@tiMainOffsetYF         = -0.005             ; move title down
    res@gsnAddCyclic           = False              ; regional data
    res@mpFillOn               = True
    res@mpLimitMode            = "Corners"          ; choose region of map

; Plot viewport
    res@vpXF                   = 0.14               ; left edge, default 0.2
    res@vpHeightF              = 1.5                ; viewport default: 0.6
    res@vpWidthF               = 0.83               ; viewport default: 0.6

    res@gsnDraw                = False              ; don't draw yet
    res@gsnFrame               = False              ; don't advance frame yet


    res@cnLevelSelectionMode   = "ManualLevels" 
    res@cnMinLevelValF         =  minlev
    res@cnMaxLevelValF         =  maxlev
    res@cnLevelSpacingF        =  stepl


    res@tfDoNDCOverlay         = True                ; must be there
    

;---- Colorbar resource      ----------------------------
;--------------------------------------------------------

; contour levels
    nlev   = tointeger( (maxlev-minlev)/ stepl )
    cnLevels = fspan( minlev, maxlev , nlev+1)
      ;print( cnLevels )

      cmap = read_colormap_file(colormap)

;** Get nice spacing through color map for marker colors

    getvalues wks
        "wkColorMapLen" : clen     ; number of colors in color map
    end getvalues

      ;print( clen )

; colors_from_map should be as many colors as cnLevels

    narr  = dimsizes(cnLevels)
    nstep = (clen-2)/narr
    nstep = nstep+1


    colors_from_map = ispan(2,clen-1,nstep)

      ;print( colors_from_map)

; label bar resources 
    res_lb                    = True
    res_lb@vpWidthF           = 0.50             ; location
    res_lb@vpHeightF          = 0.06             ; " " 
    res_lb@lbPerimOn          = True             ; Turn off perimeter.
    res_lb@lbPerimFillColor   = "White"
    res_lb@lbOrientation      = "Horizontal"     ; Default is vertical.
    res_lb@lbLabelAlignment   = "InteriorEdges"  ; Default is "BoxCenters".

    res_lb@lbFillColors       =  colors_from_map  ; cmap(2:,:)       ; Colors for boxes.

    res_lb@lbMonoFillPattern  = True             ; Fill them all solid.
    res_lb@lbLabelFontHeightF = 0.014            ; label font height
    res_lb@lbTitleString      = "ug/m~S~3~N~"    ; title
    res_lb@lbTitlePosition    = "Bottom"         ; location of title
    res_lb@lbTitleFontHeightF = 0.012            ; title font height
    gsn_labelbar_ndc (wks, dimsizes(cnLevels),""+cnLevels, 0.33, 0.18, res_lb)  



;---- Receptor conc. map     ----------------------------
;--------------------------------------------------------

; CITYCHEM grid cells, each 100m
    latmin = 0
    latmax = pmax
    lonmin = 0
    lonmax = pmax

       res@mpLeftCornerLatF    = latr2d( latmin, lonmax-1 )
       res@mpLeftCornerLonF    = min( lonr2d )
       res@mpRightCornerLatF    = max( latr2d )
       res@mpRightCornerLonF   = lonr2d( latmax-1, lonmax-1 )
   
       contour = gsn_csm_contour_map(wks, crnew(latmin:latmax-1,lonmin:lonmax-1), res)


      if (shape) then

;---- Add water body shape   ----------------------------
;--------------------------------------------------------
;** Attach the shapefile polylines using files read.

         waterbody_shp_name    = "./shapes/HH_water_selection_citychem.shp"
         f = addfile(waterbody_shp_name,"r")

         lnres             = True
         pltres            = True   ; Basic overlay plot options
         pltres@PanelPlot  = True   ; Tells wrf_map_overlays not to remove overlays
         pltres@gsFillColor     = "slategray3"   ;"slateblue1"
         pltres@gsFillOpacityF  = 0.6
         lnres@gsLineColor      = "black"     ;"slateblue4"
         lnres@gsLineThicknessF = 5.0
         lnres@gsLineOpacityF   = 1.0
         outline  = gsn_add_shapefile_polylines(wks, contour, waterbody_shp_name, lnres)
         polywa   =  gsn_add_shapefile_polygons(wks, contour, waterbody_shp_name, pltres)

;---- Add port area shape   ----------------------------
;--------------------------------------------------------
;** Attach the shapefile polylines using files read.

         harbor_shp_name    = "./shapes/HH_portarea_citychem.shp"
         g = addfile( harbor_shp_name,"r")

         ln2res                  = True
         ln2res@gsLineColor      = "white"     ;"slateblue4"
         ln2res@gsLineThicknessF = 5.0
         ln2res@gsLineOpacityF   = 1.0
         outline2  = gsn_add_shapefile_polylines(wks, contour, harbor_shp_name, ln2res)


;** Draw map and shapes
         draw(contour)       ; This will draw the map and the shapefile outlines.

;** For printing wks without station points
;;       frame(wks)       ; Advance the frame

      end if   ; end shape


;---- Station concentrations ----------------------------
;--------------------------------------------------------

;** Add the station points

     do k=0,nstat-1
        stat_utmxy = (/ xpoint(k), ypoint(k) /)
        stat_utmxy@grid_zone = utzo
        stat_latlon = utm2latlon(stat_utmxy,2)     ;WGS_84_DATUM
        stat_lat(k) = stat_latlon(0)
        stat_lon(k) = stat_latlon(1)
;; ascii       stat_cr(k)  = crep(k)
        stat_col(k) = "black"
     end do

     ;print( stat_lon)
     ;print( stat_lat)
     ;print(stat_cr)

;** Connect station concentration to color_from_map

;** Default: max of color map
     colors(:) = max( colors_from_map )

     do k=0,nstat-1

         conclev         = minlev
         color_not_found = True

         if ( stat_cr(k).le.conclev ) then

            colors(k) = colors_from_map(0)             
            color_not_found = False

         else

            if (color_not_found) then
;** loop over concentration increments 
              do m=1, dimsizes(colors_from_map)-1
                if ( (stat_cr(k).gt.conclev) .and. (stat_cr(k).le.conclev+stepl) )   then

                   colors(k)  =  colors_from_map( m )
                   color_not_found = False

                end if

                conclev = conclev + stepl
              end do

            end if
         end if

     end do

       ;print( stat_cr )
       ;print( colors )


;---- Add station as circles ----------------------------
;--------------------------------------------------------
;** Attach the station markers
;**    next step would be to scale the color index according to the concentration value crep

       mkres                     =  True
       mkres@gsFillColor         = "Background"
       mkres@gsLineThicknessF    = 8.0          ; thickness of line
       mkres@gsMarkerSizeF       = 8.0          ; marker size
       mkres@gsMarkerThicknessF  = 8.0
       hollow_circle             = NhlNewMarker(wks,"R",37,0.,0.,1.,1.,0.)
       filled_circle             = NhlNewMarker(wks,"Z",37,0.,0.,1.,1.,0.)

       do k=0,nstat-1

           mkres@gsMarkerColor = colors(k)
           mkres@gsMarkerIndex = filled_circle
           gsn_polymarker(wks, contour, stat_lon(k),stat_lat(k), mkres)

           mkres@gsMarkerColor = 0
           mkres@gsMarkerIndex = hollow_circle
           gsn_polymarker(wks, contour, stat_lon(k),stat_lat(k), mkres)

       end do


   frame(wks)       ; Advance the frame

end
