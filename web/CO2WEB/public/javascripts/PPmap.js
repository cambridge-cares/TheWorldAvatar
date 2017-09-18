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
                let muri = pp.uri;
                let marker =  new google.maps.Marker({
                    position : {lat: pp.location.lng, lng:pp.location.lat},
                    map:googleMap
                });


                marker.timer = 0;
                marker.sgclickPrevent = false;
                
                marker.addListener('click', $.proxy(function (e) {//open a popup window
//todo:A https://developers.google.com/maps/documentation/javascript/examples/infowindow-simple?hl=zh-cn
                    marker.timer = setTimeout(function() {
                        if (!marker.sglclickPrevent) {
                            clickAction();
                        }
                        marker.sglclickPrevent = false;
                    }, 200);

                    function clickAction() {
                        $.ajax({
                            url: window.location.origin+"/getAttrList",
                            method:"POST",
                            data:JSON.stringify({uri: muri}),
                            contentType: "application/json; charset=utf-8",
                            success:function (attrPairs) {
                                var infowindow = new google.maps.InfoWindow({
                                    content: formatContent(attrPairs)
                                });
                                infowindow.open(googleMap, marker);
                            }
                        });
                    }


                },marker));

                marker.addListener('dblclick', function (e) {//open file
                    //TODO: timeout to prevent miss-interpretation
                    clearTimeout(marker.timer);
                    marker.sglclickPrevent = true;
                    window.open(pp.uri);
                })
                return marker;
            });
            var markerCluster = new MarkerClusterer(googleMap, markers,
                {imagePath: 'https://developers.google.com/maps/documentation/javascript/examples/markerclusterer/m'});



        },
        error : function () {
            console.log("Can not get location")
        }
    });

    function formatContent(attrPairs) {

        let thead = "<table class='table-attr'>";
        attrPairs.forEach((pair)=>{
            thead+="<tr><td>"+pair.name+"</td><td>"+pair.value+"</td></tr>";
        })

        thead+="</table>";
        console.log(thead)
        return thead;

    }

}

