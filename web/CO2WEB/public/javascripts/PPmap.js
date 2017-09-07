/**
 * Created by Shaocong on 9/7/2017.
 */
var googleMap;


function initMap() {
    console.log("init map")
    //initiate map, set center on Jurong
    var jurong = {lat: 1.276, lng: 103.677};
    googleMap = new google.maps.Map(document.getElementById('map'), {
        zoom: 14
        , center: jurong
    });

}
