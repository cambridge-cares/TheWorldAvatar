/**
 * Created by Shaocong on 9/7/2017.
 * Module for map visualization, make cluster an option
 */

function PopupMap(options) {
    //var googleMap;//the map entity

    this.curPath = window.location.href;
    console.log("curpath: " + this.curPath);
    this.mergeOptions(options);

    /**
    this.deviceMap = (function initDeviceMap() {
        //TODO: add this to backend
        let deviceMap = new Map();
        $.getJSON("JSON/DevicePathMap.json", function (data) {
            for (let device of Object.keys(data)) {
                console.log("loading device: " + device);
                deviceMap.set(device, data[device]);
            }

        });

        return deviceMap;

    })();
    this.colorMap = new ColorMap();
    //bind initmap to global
    window.initMap = this.initMap.bind(this);
}
***/

    var deviceMapRaw = {
        "load": {
            "path": "M -2 -151 L -2 -151 L 148 -1 L -2 149 L -152 -1 Z"
            ,"scale": 0.1

        },
        "solarcellfarm": {
            "path": "M 250 150 L 400 150 L 550 150 L 550 250 L 400 250 L 400 150 L 250 150 L 250 250 L 400 250 L 400 350 L 550 350 L 550 250 L 400 250 L 250 250 L 250 350 L 400 350 "
            ,"scale": 0.1

        },
        "battery": {
            "path": "M 250 150 L 400 150 Q 450 150 450 200 L 450 250 Q 450 300 400 300 L 250 300 Q 200 300 200 250 L 200 200 Q 200 150 250 150 "
            ,"scale": 0.1

        },
        "dieselgenerators": {
            "path": "M 250 200 L 450 200 L 500 350 L 200 350 L 250 200 "
            ,"scale": 0.1

        },
        "marineturbinegenerators": {
            "path": "M 400 300 Q 350 200 400 100 L 400 100 Q 450 200 400 300 Q 500 250 600 300 Q 500 350 400 300 Q 450 400 400 500 Q 350 400 400 300 Q 300 350 200 300 Q 300 250 400 300 "
            ,"scale": 0.1

        },
        "windturbinegenerators": {
            "path": "M 400 300 Q 450 150 400 50 Q 350 150 400 300 Q 250 350 200 450 Q 350 400 400 300 Q 550 350 600 450 Q 450 400 400 300 "
            ,"scale": 0.1

        },
        "rectifier": {
            "path": "M 400 150 L 250 300 L 400 450 L 550 300 L 400 150 L 400 450 L 250 300 L 550 300 "
            ,"scale": 0.1

        },
        "powerinverter": {
            "path": "M 300 150 L 250 200 L 300 250 L 350 200 L 300 150 L 350 200 L 400 150 L 450 200 L 400 250 L 350 200 L 400 250 L 450 300 L 400 350 L 350 300 L 400 250 L 350 300 L 300 350 L 250 300 L 300 250 L 350 300 "
            ,"scale": 0.1

        },
        "transformer": {
            "path": "M 100 300 L 100 50 L 200 50 L 200 300 L 300 500 L 0 500 L 100 300 L 250 400 L 0 500 L 300 500 L 50 400 L 200 300 L 100 300 L 200 200 L 200 300 L 100 200 L 200 100 L 200 200 L 100 100 L 200 50 L 200 100 L 100 50 L 50 100 L 100 100 L 250 100 L 200 50 L 250 100 L 200 100 L 250 150 L 50 150 L 100 100 "
            ,"scale": 0.1
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
        "m": {
            "path": "M 5 34 36 18 24 51 5 34Z",
            "scale": 1
        }
    };
    this.deviceMap = (function initDeviceMap() {
        //TODO: add this to backend
        let deviceMap = new Map();
            for (let device of Object.keys(deviceMapRaw)) {
                console.log("loading device: " + device);
                deviceMap.set(device, deviceMapRaw[device]);
            }

        return deviceMap;

    })();
    this.colorMap = new ColorMap();
    //bind initmap to global
    window.initMap = this.initMap.bind(this);
}


PopupMap.prototype = {
    /**
     * Merge options
     * {useCluster}
     * @param options
     */
    mergeOptions: function (options) {
        this.useCluster = options.useCluster || false;
        this.editable = options.editable || false;

    },
    /**
     * test type string for its device type
     * @param datum
     * @returns {null}
     */
    getIconByType: function (type) {
        //switch, type, if not found ,extract name, still no, use default

        if (!type) {
            return null;
        }
        console.log("looking for " + type)
        //loop type through all in json map
        let shape = this.deviceMap.get(type.toLowerCase());
        console.log("got shape: " + shape)
        if (shape) {
            shape["fillColor"] = this.colorMap.returnColor(shape);
            shape["strokeColor"] = "black";
            shape["strokeWeight"] = 2;
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
        let thead = "<table class='table-attr'>";
        attrPairs.forEach((pair) => {
            if (this.editable) {
                thead += "<tr><td>" + pair.name + "</td><td><input id='" + uri + "#" + pair.name + "' type='text' value='" + pair.value + "'>" + "</td><td>" + pair.unit + "</td></tr>";

            } else {
                thead += "<tr><td>" + pair.name + "</td><td>" + pair.value + "</td></tr>";
            }
        })

        thead += "</table>";
        if (this.editable) {//add a submit button and a msg box
            let id = "btn" + uri;
            thead += "<button id='" + id + "' class='btn btn-default'>Submit</button>";
            thead += "<div id='err-msg-panel-" + uri + "'></div>";
        }

        // console.log(thead)
        return thead;

    },
    /**
     * Set markers and bind event for each marker
     * @param pps
     */
    setMarkers: function (pps) {
        var self = this;
        self.markers = {};
         pps.forEach(function (pp) {
            let muri = pp.uri;
            //check type to determine what icon to use
            let icon = self.getIconByType(pp.type);

            console.log("drawing type: " + icon)
             console.log("Drawing for "+muri+" at N"+pp.location.lat)
            let marker = new google.maps.Marker({
                position: {lat: pp.location.lat, lng: pp.location.lng},
                icon: icon,
                map: self.googleMap
            });

            marker.changeColor = function(hex){

                let icon = marker.getIcon();
                icon.fillColor = hex;
                marker.setIcon(icon);
            }
             marker.changeScale = function(scale){

                 let icon = marker.getIcon();
                 icon.scale = scale;
                 marker.setIcon(icon);
             }
             marker.getColor = function(){

                 let icon = marker.getIcon();
                 console.log(icon.fillColor)
                return icon.fillColor ;
             }
             marker.getScale = function(){

                 let icon = marker.getIcon();
                 console.log(icon.scale)
                 return icon.scale ;
             }

             function scaleInterp(start, end, factor) {
                 return (end - start) * factor + start;
             }
            marker.blinkAnimation = function () {
                let startColor = self.hex2rgb(marker.getColor());
                let endColor = self.hex2rgb("#FFFC94");
                let mInterpolate = self.interpolateColor.bind({}, startColor, endColor);
                let startScale = marker.getScale(), endScale = startScale*1.5;
                let animationTime= 1000;
                let step = 10, stepCount = 0, factorStep = 1/(step - 1), stepper = 1;
                if("animationOngoing" in marker){
                    return;
                }
                marker.animationOngoing = true;
                let colorTimer = window.setInterval(function() {
                    let color2 = self.rgb2hex(mInterpolate(factorStep * stepCount));
                    marker.changeColor(color2);
                    marker.changeScale(scaleInterp(startScale, endScale, factorStep * stepCount));
                    stepCount+=stepper;
                    if(stepCount >=step -1){
                        stepper = -1;
                    }
                    console.log(stepCount);
                    if(stepCount < 0){
                        console.log("stop animation")
                        delete marker.animationOngoing;
                        clearInterval(colorTimer);
                    }

                }, animationTime/step);
            };
            /**timer for determine double or single click*/
            marker.timer = 0;
            marker.sgclickPrevent = false;
            //bind single click listener
            marker.addListener('click', $.proxy(function (e) {//open a popup window

                marker.timer = setTimeout(function () {
                    if (!marker.sglclickPrevent) {
                        self.clickAction(muri, marker);
                    }
                    marker.sglclickPrevent = false;
                }, 200);

            }, marker));
//TODO: input validation(type)


            /*double click listener*/
            marker.addListener('dblclick', function (e) {//open file
                //TODO: timeout to prevent miss-interpretation
                clearTimeout(marker.timer);
                marker.sglclickPrevent = true;
                window.open(pp.uri);
            })
            self.markers[muri] = marker;
        });
    },

    hex2rgb  : function (hex) {
        var result = /^#?([a-f\d]{2})([a-f\d]{2})([a-f\d]{2})$/i.exec(hex);
        return result ? [
            parseInt(result[1], 16),
            parseInt(result[2], 16),
            parseInt(result[3], 16)
        ] : null;
    },
    
    rgb2hex : function (rgb) {
        return "#" + ((1 << 24) + (rgb[0] << 16) + (rgb[1] << 8) + rgb[2]).toString(16).slice(1);
    },
    interpolateColor : function(color1, color2, factor) {
        //if (arguments.length < 3) { factor = 0.5; }
        var result = color1.slice();
        for (var i=0;i<3;i++) {
            result[i] = Math.round(result[i] + factor*(color2[i]-color1[i]));
        }
        return result;
    },
    getMarker: function (key) {
      return key in this.markers? this.markers[key] : null ;
    },
    clickAction: function(muri, marker) {
        var self = this;
        $.ajax({
        url: window.location.origin + "/getAttrList",
        method: "POST",
        data: JSON.stringify({uri: muri}),
        contentType: "application/json; charset=utf-8",
        success: function (attrPairs) {
            var infowindow = new google.maps.InfoWindow({
                content: self.formatContent(attrPairs, muri)
            });
            if (self.editable) {//only define click handler when this map is editable
                google.maps.event.addListener(infowindow, 'domready', function () {
                    let submitId = "#" + jq("btn" + muri);
                    let errMsgBox = $("#" + jq("err-msg-panel-" + muri));

                    let modifications = {};

                    $(document).on('input', 'input', function () {//when user makes input
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
                        if (validateInput(value, copyed.datatype)) {
                            //=>Add this value to modificaition list
                            copyed['oldvalue'] = copyed['value'];
                            copyed['value'] = value;
                            modifications[name] = copyed;
                        } else {//=>opps, type err, inform user
                            self.displayMsg(errMsgBox, "Wrong datatype.", "warning");
                        }
                    });
                    $(document).on('click', submitId, function () {
                        if(Object.keys(modifications).length < 1){//do nothing if no modific
                            return;
                        }
                        console.log(modifications);
                        console.log("submit btn clicked")
                        let updateQs = constructUpdate(muri, Object.values(modifications));
                        //send ajax to update
                        let uris = [];
                        for (let i = 0; i < updateQs.length; i++) {
                            uris.push(muri);
                        }
                        console.log("sent updates: ");console.log(updateQs);console.log(uris);
                        outputUpdate(uris, updateQs, function (data) {//success callback
                            infowindow.close();
                        }, function () {//err callback
                            self.displayMsg(errMsgBox, "Can not update to server", "danger")

                        });
                    });
                });
            }

            infowindow.open(self.googleMap, marker);

            function getAttrPairFromName(name) {
                let searched = attrPairs.filter((item) => {
                    return item.name === name;
                });
                return searched.length > 0 ? searched[0] : null;
            }
            function validateInput(newValue, type) {
                switch (type) {
                    case "string":
                        return typeof  newValue === "string";
                    case "decimal":
                    case "float":
                    case "integer":
                        return !isNaN(parseFloat(newValue)) && isFinite(newValue);
                    default:
                        console.log("type " + type + " is unknown");
                }
            }

        }
    });
},

    msgtemplate : function (msg, type) {
        return "<p class='alert alert-" + type + "'>" + msg + "</p>";
    },
    displayMsg: function(panel, msg, type) {
    //TODO: swithc type
    this.cleanMsg(panel);
    panel.append(this.msgtemplate(msg, type));

},
    cleanMsg: function(panel) {
    panel.html("");
},


setCluster: function () {
        this.markerCluster = new MarkerClusterer(this.googleMap, Object.values(this.markers),
            {imagePath: 'https://developers.google.com/maps/documentation/javascript/examples/markerclusterer/m'});
    },

    /**
     * init Google map
     */
    initMap: function () {
        var self = this;
        console.log("init map:" + self.curPath)
        //initiate map, set center on Jurong
        var jurong = {lat: 1.276, lng: 103.677};
        this.googleMap = new google.maps.Map(document.getElementById('map'), {
            zoom: 14
            , center: jurong
        });

        console.log("request to " + self.curPath + "/coordinates")
        //Send ajax request to backend to retreive data
        $.ajax({
            url: self.curPath + "/coordinates",
            method: "GET",
            //  contentType: "application/json; charset=utf-8",
            success: function (pps) {
                //Update display
                self.setMarkers(pps);
                if (self.useCluster) {
                    self.setCluster();
                }
            },
            error: function () {
                console.log("Can not get location")
            }
        });
    },


};


function jq(myid) {
    console.log(myid)
    return myid.replace(/(:|\.|\[|\]|,|=|@|\/)/g, "\\$1");

}
function ColorMap() {
    this.typecolorMap = new Map();
    this.colors = [ "#1CE6FF", "#FF34FF", "#FF4A46", "#008941", "#006FA6", "#A30059",
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














