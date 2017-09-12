/**
 * Created by Shaocong on 9/7/2017.
 */
var googleMap;

var curPath = window.location.href;
function initMap() {
    console.log("init map")
    //initiate map, set center on Jurong
    var jurong = {lat: 1.276, lng: 103.677};
    googleMap = new google.maps.Map(document.getElementById('map'), {
        zoom: 14
        , center: jurong
    });

    //Send request to backend
    $.ajax({
        url: curPath+"/coordinates",
        method:"GET",
        //  contentType: "application/json; charset=utf-8",
        success: function (pps) {
            //Update display

            //console.log(pps);
            var markers  = pps.map(function (pp) {
                console.log(typeof pp.location.lng)
                let marker =  new google.maps.Marker({
                    position : {lat: pp.location.lng, lng:pp.location.lat},
                    map:googleMap
                });
                
                marker.addListener('click', function (e) {
                    window.open(pp.uri);
                })
                return marker;
            })
            var markerCluster = new MarkerClusterer(googleMap, markers,
                {imagePath: 'https://developers.google.com/maps/documentation/javascript/examples/markerclusterer/m'});



        },
        error : function () {
            console.log("Can not get location")
        }
    });
}

