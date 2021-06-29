/**
 * Prototype Module for map visualization, make cluster an option
 */

//part no written by me-----------------------------------------------------------------------------------//
var infowindow = null;
var globalKMLEventinfoWindowHtml;
var selectedId;

var colorMap = ['#99f', '#f99', '#9f9', '#f9f', '#39f'];


const toggleDisplay = elemId => {
    let x = document.getElementById(elemId);
    if (x.style.display !== 'block') {
        x.style.display = 'block';
    } else {
        x.style.display = 'none';
    }
};

$("#readme-button").click(function () {
    toggleDisplay("readme-text");
});

document.addEventListener("click", function (evt) {
    var readmeButtonElement = document.getElementById('readme-button'),
        readmeTextElement = document.getElementById('readme-text'),
        targetElement = evt.target;  // clicked element

    if (targetElement == readmeButtonElement || targetElement == readmeTextElement) {
        return; //readme-button or readme-text is clicked. do nothing.
    }

    if (readmeTextElement.style.display === 'block') {
        readmeTextElement.style.display = 'none';
    }
});

//-----------------------------------------------------------------------------------//

/*Map with Popup markers**/
function PopupMap(options) {
    //var googleMap;//the map entity
    this.curPath = window.location.href;
    console.log("curpath: " + this.curPath);
    this.mergeOptions(options);

    /**
     * A raw device map of name - SVG Path
     * @type {{load: {path: string, scale: number}, solarcellfarm: {path: string, scale: number}, battery: {path: string, scale: number}, dieselgenerators: {path: string, scale: number}, marineturbinegenerators: {path: string, scale: number}, windturbinegenerators: {path: string, scale: number}, rectifier: {path: string, scale: number}, powerinverter: {path: string, scale: number}, transformer: {path: string, scale: number}, c: {path: string, scale: number}, v: {path: string, scale: number}, p: {path: string, scale: number}, va: {path: string, scale: number}, r: {path: string, scale: number}, e: {path: string, scale: number}, t: {path: string, scale: number}, s: {path: string, scale: number}, m: {path: string, scale: number}}}
     */
    var deviceMapRaw = {

        "nuclear": {
            url: "/images/radiation.png",
            scale: 0.3
        },

        "powerload": {
            "path": "M -2 -151 L -2 -151 L 148 -1 L -2 149 L -152 -1 Z"
            , "scale": 0.3

        },
        //"photovoltaicgenerator": {
        "solargen": {
            "path": "M 250 150 L 400 150 L 550 150 L 550 250 L 400 250 L 400 150 L 250 150 L 250 250 L 400 250 L 400 350 L 550 350 L 550 250 L 400 250 L 250 250 L 250 350 L 400 350 "
            , "scale": 0.3

        },
        "battery": {
            "path": "M 250 150 L 400 150 Q 450 150 450 200 L 450 250 Q 450 300 400 300 L 250 300 Q 200 300 200 250 L 200 200 Q 200 150 250 150 "
            , "scale": 0.3

        },
        "dieselgen": {
            //"fossilfuelgenerator": {
            "path": "M 250 200 L 450 200 L 500 350 L 200 350 L 250 200 "
            , "scale": 0.3

        },
        "tidalgen": {
            //"tidalgenerator": {
            "path": "M 400 300 Q 350 200 400 100 L 400 100 Q 450 200 400 300 Q 500 250 600 300 Q 500 350 400 300 Q 450 400 400 500 Q 350 400 400 300 Q 300 350 200 300 Q 300 250 400 300 "
            , "scale": 0.3

        },
        "windgen": {
            //"windgenerator": {
            "path": "M 400 300 Q 450 150 400 50 Q 350 150 400 300 Q 250 350 200 450 Q 350 400 400 300 Q 550 350 600 450 Q 450 400 400 300 "
            , "scale": 0.3

        },
        "rec": {
            //"rectifier": {
            "path": "M 400 150 L 250 300 L 400 450 L 550 300 L 400 150 L 400 450 L 250 300 L 550 300 "
            , "scale": 0.15

        },
        "inv": {
            //"inverter": {
            "path": "M 300 150 L 250 200 L 300 250 L 350 200 L 300 150 L 350 200 L 400 150 L 450 200 L 400 250 L 350 200 L 400 250 L 450 300 L 400 350 L 350 300 L 400 250 L 350 300 L 300 350 L 250 300 L 300 250 L 350 300 "
            , "scale": 0.15

        },
        "substn": {
            //"substation": {
            "path": "M 100 300 L 100 50 L 200 50 L 200 300 L 300 500 L 0 500 L 100 300 L 250 400 L 0 500 L 300 500 L 50 400 L 200 300 L 100 300 L 200 200 L 200 300 L 100 200 L 200 100 L 200 200 L 100 100 L 200 50 L 200 100 L 100 50 L 50 100 L 100 100 L 250 100 L 200 50 L 250 100 L 200 100 L 250 150 L 50 150 L 100 100 "
            , "scale": 0.3
        },
        "c": {
            "path": "M 1,44 45,1 69,22 40,78 1,44Z",
            "scale": 1
        },
        "v": {
            "path": "M 13 14 22 8 29 8 30 15 25 25 17 32 09 31 08 24 13 14Z",
            "scale": 2
        },
        "p": {
            "path": "M 35 14 17 35 13 23 10 24 8 23 5 22 3 20 0 18 0 16 0 12 0 9 2 5 5 2 9 0 13 1 17 2 20 6 22 8 23 11 22 13 35 14Z",
            "scale": 1
        },
        "va": {
            "path": "M 20 16 18 27 10 19 20 16ZM 23 5 31 12 20 16 23 5Z",
            "scale": 2
        },
        "r": {
            "path": "M 17 15 17 16 20 13 20 12 18 14 15 10 21 3 27 10 28 16 25 22 18 25 13 23 7 17 14 11 18 14 17 15Z",
            "scale": 3
        },
        "e": {
            "path": "M 13 29 36 5 58 26 43 42 46 27 32 30 43 17 27 33 44 29 41 44 58 26 65 33 64 34 42 57 13 29Z",
            "scale": 1
        },
        "t": {
            "path": "M 86 118 87 121 88 124 87 128 83 133 78 137 74 139 70 139 67 139 86 118ZM 86 118 57 110 48 84 86 118ZM 48 84 48 84 48 83 48 84ZM 57 110 67 139 28 104 27 104 27 104 26 103 25 101 24 98 25 95 26 91 27 88 31 84 35 81 40 80 44 81 46 83 48 84 30 104 57 110Z",
            "scale": 1
        },
        "s": {
            "path": "M 15 2 34 17 9 30 15 2Z",
            "scale": 2
        },
        "mix": {
            "path": "M 5 34 36 18 24 51 5 34Z",
            "scale": 1
        },
        "m": {
            "path": "M 5 34 36 18 24 51 5 34Z",
            "scale": 1
        }
    };
    this.deviceMap = (function initDeviceMap() {
        let deviceMap = new Map();
        for (let device of Object.keys(deviceMapRaw)) {
            console.log("loading device: " + device);
            deviceMap.set(device, deviceMapRaw[device]);
        }

        return deviceMap;

    })();
    this.colorMap = new ColorMap();
    //bind initmap to global
    animatedLines = [];
    window.initMap = this.initMap.bind(this);
}



PopupMap.prototype = {
    /**
     * Merge options
     * {useCluster}
     * @param options
     */
    mergeOptions: function (options) {
        Object.assign(this, options)

    },
    Branch: function (coors, vols, thickness, type, name) {
        this.name = name;
        this.vols = vols;
        this.thickness = thickness;
        this.type = type;
        this.coors = coors;
    },
    point: function (lat, lng) {
        this.lat = lat;
        this.lng = lng;
    },

    /*main func: initMap*/
    /**
     * init Google map
     */
    setCenter: function (latlng) {
        this.googleMap.setCenter(latlng);

    },
    initMap: function () {
        var self = this;
        console.log("init map:" + self.curPath)
        //initiate map, set center on Jurong
        var jurong = { lat: 1.2624421, lng: 103.7007045 };
        if (this.center) {
            var mcenter = this.center;
        } else {
            mcenter = jurong;
        }
        this.googleMap = new google.maps.Map(document.getElementById('map'), {
            zoom: 7,
            center: mcenter,

        });
        map = this.googleMap;
        initMapOverlay(map, lines);
        console.log("request to " + self.curPath + "/coordinates")
        $.ajax({
            url: self.curPath + "/coordinates",
            method: "GET",
            async: true,
            success: function (pps) {
                console.log('check status of ajax')
                self.updateMarkers(pps, pps);
            },
            error: function () {
                console.log("Can not get location")
            }
        });
        /*****
            $.ajax({
                url: self.curPath + "/:uri",
                method: "GET",
                async: true,
                success: function (pps) {
                     console.log('check status of ajax')
                     self.updateMarkers(pps, pps);
                },
                error: function () {
                    console.log("Can not get uri")
                }
            });
        */
        animatedLines = []
    },

    drawLines: function (data) {
        var map = this.googleMap;
        var self = this;
        var lines = [];
        var agenturl = prefix + '/JPS_POWSYS/ENVisualization/createLineJS'
        var kmlurl = createUrlForAgent(scenario, agenturl, data);
        console.log('my kmlurl = ' + kmlurl);
        var request = $.ajax({
            url: kmlurl,
            type: 'GET',
            async: true,
            contentType: 'application/json; charset=utf-8'
        });

        request.done(function (data) {
            var obj0 = JSON.parse(data).result;
            var colorMap = ['#99f', '#f99', '#9f9', '#f9f', '#39f'];
            var size = obj0.length;
            console.log("size=" + size);
            var x;
            for (x = 0; x < size; x++) {
                var obj = JSON.parse(obj0[x]);
                var point0 = new self.point(obj.coors[0].lat, obj.coors[0].lng);
                var point1 = new self.point(obj.coors[1].lat, obj.coors[1].lng);
                var temparr = [point0, point1];
                var vol0 = obj.vols[0];

                var vol1 = obj.vols[1];
                var temparrvol = [vol0, vol1];
                var line = new self.Branch(temparr, temparrvol, obj.thickness, obj.type, obj.name);
                // console.log("linename="+obj.name);
                // console.log("linepoint0long="+temparr[0].lng);

                lines.push(line);
            }
            //console.log('lines',lines);


            for (var index in lines) { //this is focused on drawing lines

                var _line = lines[index];
                var _name = _line['name'];
                var _type = _line['type'];
                var _path = _line['coors'];
                var _thickness = _line['thickness'];
                var lineSymbol = {
                    path: google.maps.SymbolPath.FORWARD_OPEN_ARROW,
                    scale: 2,
                    strokeColor: '#333'
                };


                if (_type === 'distribute') {
                    var line = new google.maps.Polyline({
                        path: _path,
                        strokeWeight: _thickness,
                        strokeColor: colorMap[_thickness - 3],
                        icons: [{
                            icon: lineSymbol,
                            offset: '100%'
                        }],
                        map: map,
                        title: _name
                    });

                    google.maps.InfoWindow.prototype.opened = false;

                    line.addListener('click', function (lineEvent) {
                        console.log(lineEvent.latLng.toString());
                        var content = constructLineMenu(this.title, function (_content) {
                            infoWindow = new google.maps.InfoWindow({
                                content: _content
                            });

                            marker = new google.maps.Marker({
                                position: lineEvent.latLng,
                                map: map,
                            });
                            marker.addListener('click', function () {
                                infoWindow.open(map, marker);
                            });
                            marker.addListener('dblclick', function () {
                                infoWindow.close();
                                marker.setMap(null);
                                marker = null;
                            });
                        });


                    });
                    self.animateCircle(line, 1);
                    animatedLines.push(line);
                }
                else if (_type === 'transformer') {
                    var transformer = new google.maps.Circle({
                        strokeColor: '#00ff00',
                        strokeOpacity: 0.8,
                        strokeWeight: 2,
                        fillColor: '#00ff00',
                        fillOpacity: 0.55,
                        map: this.googleMap,
                        center: _path[0],
                        radius: 30,
                        title: _name
                    });

                    transformer.addListener('click', function () {
                        var that = this;

                        var content = constructLineMenu(this.title, function (_content) {
                            console.log('content', _content);
                            infowindow.setContent(_content);
                            infowindow.open(this.googleMap, that);

                        });
                        console.log(content);

                    });

                    animatedLines.push(line);
                }





            }
        });

        request.fail(function (jqXHR, textStatus, errorThrown) {
            alert("The files that needed to be created do not exist. Hit the Run Button again to try again. ");
            console.log(errorThrown);
        });
    },

    animateCircle: function (line, timeOut) {
        var count = 0;
        window.setInterval(function () {
            count = (count + 1) % 200;

            var icons = line.get('icons');
            icons[0].offset = (count / 2) + '%';
            line.set('icons', icons);
        }, timeOut);
    },

    /**
    update markers 
    @parans:
    pps: coordinates to draw
    definedPopUpAttrPair: map of attributes to appear in popups for markers
    ***/
    updateMarkers: function (pps, definedPopUpAttrPair) {
        var self = this;
        self.clearMarkers();
        //Update display
        self.coordinates = pps;
        self.setMarkers(pps, definedPopUpAttrPair);
        if (self.useCluster) {
            self.setCluster();
        }
        // self.drawLines();
    },


    /*marker with popup**/
    /**
     * test type string for its device type
     * @param datum
     * @returns {null}
     */
    getIconByType: function (type, highlight) {
        //switch, type, if not found ,extract name, still no, use default

        if (!type) {
            return null;
        }
        console.log("looking for " + type)

        console.log("Highlighted: ", highlight);

        //loop type through all in json map
        let shape = this.deviceMap.get(type.toLowerCase());
        console.log("got shape: " + shape)
        if (shape) {
            if (highlight) {
                shape["strokeColor"] = "yellow";
                shape["strokeWeight"] = 4;
            }
            else {
                shape["strokeColor"] = "black";
                shape["strokeWeight"] = 2;

            }
            shape["fillColor"] = this.colorMap.returnColor(shape);
            shape["fillOpacity"] = 1;
        }
        return shape;
    },
    /**
     * Format attribute name-value pair into a table
     * @param attrPairs
     * @returns {string}
     */
    formatContent: function (attrPairs, uri) {
        let thead = "<table id='table" + uri + "' class='table-attr'>";
        thead += "<tr><th style=\"text-align:center\">Attribute</th><th style=\"text-align:center\">Value</th></tr>";
        // let allAttrPairs = getPowerPlantAttr(getPowerPlantAttrQueryStr(uri));
        $.each(attrPairs, function (key, value) {
            if (key == 'location') {
                console.log('key is:' + key);
                $.each(value, function (key2, value2) {
                    console.log('key2 is:' + key2);
                    console.log('value2 is:' + value2);
                    thead += "<tr><td style=\"text-align:center\">" + key2 + "</td><td style=\"text-align:center\">" + value2 + "</td></tr>";
                });
            } else if (key == 'uri') {
                let res = value.split("#");
                console.log(res[0]);
                console.log(res[1]);
                console.log(res.length);
                if (res.length === 2) {
                    thead += "<tr><td style=\"text-align:center\">Name</td><td style=\"text-align:center\">" + res[1] + "</td></tr>";
                } else {
                    thead += "<tr><td style=\"text-align:center\">uri</td><td style=\"text-align:center\">" + value + "</td></tr>";
                }
            } else {
                thead += "<tr><td style=\"text-align:center\">" + key + "</td><td style=\"text-align:center\">" + value + "</td></tr>";
            }
        });
        thead += "</table>";
        if (this.editable) {//add a submit button and a msg box
            let id = "btn" + uri;
            thead += "<button id='" + id + "' class='btn btn-default'>Submit</button>";
            thead += "<p id='err-msg-panel-" + uri + "'></p>";
        }
        return thead;
    },
    /**
     * Set markers and bind event for each marker
     * @param pps
     */
    setMarkers: function (pps, attrPairs) {
        var self = this;
        self.markers = {};
        if (!pps || pps.constructor !== Array) {
            return;
        }
        pps.forEach(function (pp) {
            let muri = pp.uri;
            //check type to determine what icon to use

            var highlight = false;

            if (muri === 'http://www.jparksimulator.com/kb/sgp/jurongisland/biodieselplant3/E-301.owl' || muri === 'http://www.jparksimulator.com/kb/sgp/jurongisland/biodieselplant2/E-601008.owl') {
                highlight = true;
            }


            let icon = self.getIconByType(pp.type, highlight);




            /*console.log("drawing type: " + icon)
            console.log("Drawing for "+muri+" at N"+pp.location.lat)
            console.log("Drawing for "+muri+" at E"+pp.location.lng)*/

            let marker = new google.maps.Marker({
                position: { lat: pp.location.Latitude, lng: pp.location.Longitude },
                icon: icon,
                map: self.googleMap
            });

            marker.changeColor = function (hex) {

                let icon = marker.getIcon();
                icon.fillColor = hex;
                marker.setIcon(icon);
            }
            marker.changeScale = function (scale) {

                let icon = marker.getIcon();
                icon.scale = scale;
                marker.setIcon(icon);
            }
            marker.getColor = function () {

                let icon = marker.getIcon();
                console.log(icon.fillColor)
                return icon.fillColor;
            }
            marker.getScale = function () {

                let icon = marker.getIcon();
                console.log(icon.scale)
                return icon.scale;
            }

            function scaleInterp(start, end, factor) {
                return (end - start) * factor + start;
            }
            marker.blinkAnimation = function () {
                let startColor = self.hex2rgb(marker.getColor());
                let endColor = self.hex2rgb("#FFFC94");
                let mInterpolate = self.interpolateColor.bind({}, startColor, endColor);
                let startScale = marker.getScale(), endScale = startScale * 1.5;
                let animationTime = 1000;
                let step = 10, stepCount = 0, factorStep = 1 / (step - 1), stepper = 1;
                if ("animationOngoing" in marker) {
                    return;
                }
                marker.animationOngoing = true;
                let colorTimer = window.setInterval(function () {
                    let color2 = self.rgb2hex(mInterpolate(factorStep * stepCount));
                    marker.changeColor(color2);
                    marker.changeScale(scaleInterp(startScale, endScale, factorStep * stepCount));
                    stepCount += stepper;
                    if (stepCount >= step - 1) {
                        stepper = -1;
                    }
                    if (stepCount < 0) {
                        console.log("stop animation")
                        delete marker.animationOngoing;
                        clearInterval(colorTimer);
                    }

                }, animationTime / step);
            };
            /**timer for determine double or single click*/
            marker.timer = 0;
            marker.sgclickPrevent = false;
            //bind single click listener
            marker.addListener('click', $.proxy(function (e) {//open a popup window

                marker.timer = setTimeout(function () {
                    if (!marker.sglclickPrevent) {
                        if (pp) {
                            var attributeArray = {
                                uri: pp['uri'], location: pp['location'],
                                Commissioned_year: pp['built_year'],
                                Annual_Generation: pp['Annual_Generation'],
                                Owner: pp['Owner'],
                                Located_country: pp['Country'],
                                Generation_Technology: pp['Generation_Technology'],                              
                                CO2_Emission: pp['CO2_Emission'],
                                Country: pp['Country'],
                                Designed_Capacity: pp['Designed_Capacity']
                                
                            };
                            self.formatPopup(attributeArray, pp['uri'], marker);
                        } else {
                            self.openEditablePopupNet(muri, marker);
                        }
                    }
                    marker.sglclickPrevent = false;
                }, 200);

            }, marker));


            /*double click listener*/
            marker.addListener('dblclick', function (e) {//open file
                clearTimeout(marker.timer);
                marker.sglclickPrevent = true;
                window.open(pp.uri);
            })
            self.markers[muri] = marker;
        });
    },

    /**
    clean all markers
    ***/
    clearMarkers: function () {
        console.log(this.markers);
        let self = this;
        if (!self.markers || Object.keys(self.markers).length < 1) {
            return;
        }
        for (marker of Object.values(self.markers)) {
            marker.setMap(null);
            console.log(marker.title, marker.icon);
            marker = null;
        }
        self.markers = {}
        if (self.markerCluster) {
            self.markerCluster.clearMarkers();
        }
    },

    getMarker: function (key) {
        return key in this.markers ? this.markers[key] : null;
    },
    /**
     *  when a marker is clicked, open a popup
     * @param muri
     * @param marker
     */
    openEditablePopupNet: function (muri, marker) {
        var self = this;
        console.log('test');
        //grabs
        $.ajax({
            url: window.location.origin + "/getAttrList",
            method: "POST",
            data: JSON.stringify({ uri: muri }),
            contentType: "application/json; charset=utf-8",
            success: function (attrPairs) {
                self.formatPopup(attrPairs, muri, marker);
            }
        });
    },

    /**
    draw a popup window for a marker
    @params:
    attrPairs: attrs to appear in the pouup
    muri: uri of marker as id
    marker: the marker object
    ***/
    formatPopup: function (attributeArray, muri, marker) {
        const self = this;
        if (infowindow != null) {
            infowindow.close();
        }
        infowindow = new google.maps.InfoWindow({
            content: self.formatContent(attributeArray, muri)
        });
        if (self.editable) {//only define click handler when this map is editable
            google.maps.event.addListener(infowindow, 'domready', function () {
                let submitId = "#" + jq("btn" + muri);
                let errMsgBox = $("#" + jq("err-msg-panel-" + muri));
                let infobox = $("#" + jq("table" + muri))
                let mattrs = attributeArray
                let modifications = {};


                //***input event handler for popup window**************//
                infobox.on('input', 'input', function () {//when user makes input
                    console.log("input changed");

                    self.cleanMsg(errMsgBox);
                    let el = $(this), value = el.val();
                    if (value === "") {
                        return;
                    }
                    //validate this value
                    let attrid = el.attr("id");
                    let nameArr = attrid.split('#');
                    let name = nameArr[nameArr.length - 1];
                    console.log("attr " + attrid + " " + value);

                    let copyed = getAttrPairFromName(name);
                    // if (copyed.datatype && validateInput(value, copyed.datatype)) {
                    //=>Add this value to modificaition list
                    console.log(copyed)
                    copyed['oldvalue'] = copyed['value'];
                    copyed['value'] = value;
                    modifications[name] = copyed;
                    //} else {//=>opps, type err, inform user
                    //    self.displayMsg(errMsgBox, "Wrong datatype.", "warning");
                    // }
                });
                //submit event handler for popup window**************//
                $(document).on('click', submitId, function () {
                    if (Object.keys(modifications).length < 1) {//do nothing if no modific
                        console.log('no change');
                        infowindow.close();
                        google.maps.event.clearInstanceListeners(infowindow);
                        infoWindow = null;
                        return;
                    }
                    console.log(modifications);
                    console.log("submit btn clicked");
                    let updateQs = constructUpdate(muri, Object.values(modifications));
                    //send ajax to update
                    let uris = [];
                    for (let i = 0; i < updateQs.length; i++) {
                        uris.push(muri);
                    }
                    console.log("sent updates: ");
                    outputUpdate([uris, updateQs], function (data) {//success callback
                        console.log("OUTPUT UPDATED");
                        infowindow.close();
                        google.maps.event.clearInstanceListeners(infowindow);
                        infowindow = null;
                    }, function () {//err callback
                        self.displayMsg(errMsgBox, "Can not update to server", "danger")
                        infowindow.close();
                        google.maps.event.clearInstanceListeners(infowindow);
                        infowindow = null;
                    });
                });

                function getAttrPairFromName(name) {
                    let searched = mattrs.filter((item) => {
                        return item.name === name;
                    });
                    return searched.length > 0 ? searched[0] : null;
                }
            });
        }

        infowindow.open(self.googleMap, marker);

    },

    /*marker Cluster**/
    setCluster: function () {
        this.markerCluster = new MarkerClusterer(this.googleMap, Object.values(this.markers),
            {
                imagePath: 'https://developers.google.com/maps/documentation/javascript/examples/markerclusterer/m'
                , maxZoom: 18
            });
    },

    /*Color interpolating animation**/
    hex2rgb: function (hex) {
        var result = /^#?([a-f\d]{2})([a-f\d]{2})([a-f\d]{2})$/i.exec(hex);
        return result ? [
            parseInt(result[1], 16),
            parseInt(result[2], 16),
            parseInt(result[3], 16)
        ] : null;
    },
    rgb2hex: function (rgb) {
        return "#" + ((1 << 24) + (rgb[0] << 16) + (rgb[1] << 8) + rgb[2]).toString(16).slice(1);
    },
    interpolateColor: function (color1, color2, factor) {
        //if (arguments.length < 3) { factor = 0.5; }
        var result = color1.slice();
        for (var i = 0; i < 3; i++) {
            result[i] = Math.round(result[i] + factor * (color2[i] - color1[i]));
        }
        return result;
    },

    /*Trans line animation****/
    /**
     * take in a list of coordinates, draw animated lines between them
     * @param list
     */
    drawAnimatedLines: function (list) {
        list.forEach((line) => {

            console.log(line[0])
            console.log(line[1])
            console.log("Draw line: " + hwMap.coordinates[line[0]])
            hwMap.drawAnimatedLine([hwMap.coordinates[line[0] - 1]["location"], hwMap.coordinates[line[1] - 1].location]);

        });
    },
    /**
         * Draw a single animated line
         * @param vertexs
         */
    drawAnimatedLine: function (vertexs) {
        var lineSymbol = {
            path: "M 0 0 L 0 0 L 0 50 ",
            scale: 0.1,
            strokeColor: '#cc0015',
            strokeWeight: 10
        };

        var line = new google.maps.Polyline({
            path: vertexs,
            icons: [{
                icon: lineSymbol,
                offset: '100%'
            }, {
                icon: lineSymbol,
                offset: '100%'
            },
            {
                icon: lineSymbol,
                offset: '100%'
            },
            {
                icon: lineSymbol,
                offset: '100%'
            },
            {
                icon: lineSymbol,
                offset: '100%'
            }, {
                icon: lineSymbol,
                offset: '100%'
            }

            ],
            map: this.googleMap
        });
        this.animatedLines = this.animatedLines ? this.animatedLines : [];
        this.animatedLines.push(line);
        this.animateLine(line)
    },

    clearAnimatedLines: function () {
        animatedLines.forEach((line) => {
            line.setMap(null);
        })

        this.animatedLines = []
    },
    animateLine: function (line) {
        var count = 0, step = 20;
        window.setInterval(function () {
            count = (count + 1) % 100;

            var icons = line.get('icons');
            let iconId = 0;

            for (let iconId = 0; iconId < icons.length; iconId++) {
                icons[iconId].offset = (count + step * iconId) % 100 + '%';

            }



            line.set('icons', icons);
        }, 40);
    },

    /*Msg***/
    msgTemplate: function (msg, type) {
        return "<p class='alert alert-" + type + "'>" + msg + "</p>";
    },
    displayMsg: function (panel, msg, type) {
        this.cleanMsg(panel);
        panel.append(this.msgTemplate(msg, type));

    },
    cleanMsg: function (panel) {
        panel.html("");
    }
};
/*Color map****/
function ColorMap() {
    this.typecolorMap = new Map();
    this.colors = ["#1CE6FF", "#FF34FF", "#FF4A46", "#008941", "#006FA6", "#A30059",
        "#FFDBE5", "#7A4900", "#0000A6", "#63FFAC", "#B79762", "#004D43", "#8FB0FF", "#997D87",
        "#5A0007", "#809693", "#FEFFE6", "#1B4400", "#4FC601", "#3B5DFF", "#4A3B53", "#FF2F80",
        "#61615A", "#BA0900", "#6B7900", "#00C2A0", "#FFAA92", "#FF90C9", "#B903AA", "#D16100",
        "#DDEFFF", "#000035", "#7B4F4B", "#A1C299", "#300018", "#0AA6D8", "#013349", "#00846F",
        "#372101", "#FFB500", "#C2FFED", "#A079BF", "#CC0744", "#C0B9B2", "#C2FF99", "#001E09",
        "#00489C", "#6F0062", "#0CBD66", "#EEC3FF", "#456D75", "#B77B68", "#7A87A1", "#788D66",
        "#885578", "#FAD09F", "#FF8A9A", "#D157A0", "#BEC459", "#456648", "#0086ED", "#886F4C",
        "#34362D", "#B4A8BD", "#00A6AA", "#452C2C", "#636375", "#A3C8C9", "#FF913F", "#938A81",
        "#575329", "#00FECF", "#B05B6F", "#8CD0FF", "#3B9700", "#04F757", "#C8A1A1", "#1E6E00",
        "#7900D7", "#A77500", "#6367A9", "#A05837", "#6B002C", "#772600", "#D790FF", "#9B9700",
        "#549E79", "#201625", "#72418F", "#BC23FF", "#99ADC0", "#3A2465", "#922329",
        "#5B4534", "#FDE8DC", "#404E55", "#0089A3", "#CB7E98", "#A4E804", "#324E72", "#6A3A4C"];
    this.count = 0;

}
ColorMap.prototype.returnColor = function (type) {
    if (this.typecolorMap.has(type)) {
        return this.typecolorMap.get(type);
    } else {
        let color = this.colors[this.count++];
        this.typecolorMap.set(type, color);
        return color;
    }

}
/*Utility**/
/**
* Jquery encoder, underscore chars not accepted by jquery selector
* @param myid
* @returns {void|XML|string|*}
*/
function jq(myid) {
    console.log(myid)
    return myid.replace(/(:|\.|\[|\]|,|=|@|\/)/g, "\\$1");

}

