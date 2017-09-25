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
            shape["scale"] = 0.1;
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
    formatContent: function (attrPairs) {
        let thead = "<table class='table-attr'>";
        attrPairs.forEach((pair) => {
            thead += "<tr><td>" + pair.name + "</td><td>" + pair.value + "</td></tr>";
        })

        thead += "</table>";
        // console.log(thead)
        return thead;

    },
    /**
     * Set markers and bind event for each marker
     * @param pps
     */
    setMarkers: function (pps) {
        var self = this;
        this.markers = pps.map(function (pp) {
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
                        clickAction();
                    }
                    marker.sglclickPrevent = false;
                }, 200);

                function clickAction() {
                    $.ajax({
                        url: window.location.origin + "/getAttrList",
                        method: "POST",
                        data: JSON.stringify({uri: muri}),
                        contentType: "application/json; charset=utf-8",
                        success: function (attrPairs) {
                            var infowindow = new google.maps.InfoWindow({
                                content: self.formatContent(attrPairs)
                            });
                            infowindow.open(self.googleMap, marker);
                        }
                    });
                }
            }, marker));

            /*double click listener*/
            marker.addListener('dblclick', function (e) {//open file
                //TODO: timeout to prevent miss-interpretation
                clearTimeout(marker.timer);
                marker.sglclickPrevent = true;
                window.open(pp.uri);
            })
            return marker;
        });
    },
    setCluster : function () {
        this.markerCluster = new MarkerClusterer(this.googleMap, this.markers,
             {imagePath: 'https://developers.google.com/maps/documentation/javascript/examples/markerclusterer/m'});
    },

    /**
     * init Google map
     */
    initMap: function () {
        var self = this;
        console.log("init map:"+self.curPath)
        //initiate map, set center on Jurong
        var jurong = {lat: 1.276, lng: 103.677};
        this.googleMap = new google.maps.Map(document.getElementById('map'), {
            zoom: 14
            , center: jurong
        });

        //Send ajax request to backend to retreive data
        $.ajax({
            url: self.curPath + "/coordinates",
            method: "GET",
            //  contentType: "application/json; charset=utf-8",
            success: function (pps) {
                //Update display
                self.setMarkers(pps);
                 if(self.useCluster){
                     self.setCluster();
                 }
            },
            error: function () {
                console.log("Can not get location")
            }
        });
    }


};


function ColorMap() {
    this.typecolorMap = new Map();
    this.colors = ["#000000", "#FFFF00", "#1CE6FF", "#FF34FF", "#FF4A46", "#008941", "#006FA6", "#A30059",
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














