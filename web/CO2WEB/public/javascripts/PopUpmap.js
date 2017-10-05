/**
 * Created by Shaocong on 9/7/2017.
 * Module for map visualization, make cluster an option
 */

function PopupMap(options) {
    //var googleMap;//the map entity

    this.curPath = window.location.href;
    console.log("curpath: " + this.curPath);
    this.mergeOptions(options);
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
            let marker = new google.maps.Marker({
                position: {lat: pp.location.lat, lng: pp.location.lng},
                icon: icon,
                map: self.googleMap
            });
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
        console.log("!!!!!!!!!!!!!!!!!!!!!")
        console.log(this.markers);
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
    this.colors = ["#FFFF00", "#1CE6FF", "#FF34FF", "#FF4A46", "#008941", "#006FA6", "#A30059",
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
        "#549E79", "#FFF69F", "#201625", "#72418F", "#BC23FF", "#99ADC0", "#3A2465", "#922329",
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














