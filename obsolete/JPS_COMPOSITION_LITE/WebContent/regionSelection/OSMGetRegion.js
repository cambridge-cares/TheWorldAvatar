var osmb = new OSMBuildings({
    container: 'map',
    position: { latitude: 52.52000, longitude: 13.41000 },
    zoom: 16,
    minZoom: 15,
    maxZoom: 22
});


let regionCoords = []; // Contains two coordinates

osmb.addMapTiles(
    'https://{s}.tiles.mapbox.com/v3/osmbuildings.kbpalbpk/{z}/{x}/{y}.png',
    {
        attribution: '© Data <a href="http://openstreetmap.org/copyright/">OpenStreetMap</a> · © Map <a href="http://mapbox.com">Mapbox</a>'
    }
);

osmb.addGeoJSONTiles('http://{s}.data.osmbuildings.org/0.2/anonymous/tile/{z}/{x}/{y}.json');

osmb.on('pointerdown', function(e) {

	console.log('Pointer Down',osmb.getPosition());
    if(regionCoords.length === 2){
        // Send out the request
        // get two Xs
        let x_max = Math.max(regionCoords[0].longitude,regionCoords[1].longitude);
        let x_min = Math.min(regionCoords[0].longitude,regionCoords[1].longitude);
        let y_max = Math.max(regionCoords[0].latitude,regionCoords[1].latitude);
        let y_min = Math.min(regionCoords[0].latitude,regionCoords[1].latitude);
        regionCoords = [];
        console.log('Points: ', y_min,x_min,y_max,x_max);
        /*13.42463347925127
        52.51659231193872
        13.412767386569385
        52.506680420545095
        */
        send(52.506680420545095,13.412767386569385,52.51659231193872,13.42463347925127);
    }
    else{
        regionCoords.push(osmb.getPosition());
    }




     //send(osmb.getPosition().longitude, osmb.getPosition().latitude);
});


var hostname = window.location.host.replace('/MapRegionSelection','');
var executionChain = window.location.href.split("data=")[1];


function send(y_min,x_min,y_max,x_max) {
    let regionInSemanticJSON =  generateSemanticRegion(y_min,x_min,y_max,x_max);

    var myUrl = 'http://' + 'localhost:8080' + '/JPS_COMPOSITION/ServiceExecutionEndpoint?executionChain=' + executionChain + '&value=' + encodeURIComponent(regionInSemanticJSON);
    //var myUrl = 'http://' + 'localhost:8080' + '/JPS/GetBuildingListFromRegion?&value=' + encodeURIComponent(regionInSemanticJSON);

    console.log(myUrl);
    var request = $.ajax({
        url: myUrl,
        type: 'GET',
        contentType: 'application/json; charset=utf-8'
    });

    request.done(function (data) {

        let theUrl = "http://" + hostname + "/JPS/?value=" +  encodeURIComponent(data) + "lat=52.076146&lon=4.309961&zoom=14.5&tilt=0.0&rotation=0.6";
        document.getElementById('frame').src = theUrl;
        var win = window.open(theUrl, '_blank');
        win.focus();
    });

    request.fail(function (jqXHR, textStatus) {

    });
}


const regionTemplate = '{ \n' +
    '  "http://test.com/lowerPoint" : { \n' +
    '    "http://test.com/Property/hasY" : [ { \n' +
    '      "type" : "literal" ,\n' +
    '      "value" : "{0}" ,\n' +
    '      "datatype" : "http://www.w3.org/2001/XMLSchema#string"\n' +
    '    }\n' +
    '     ] ,\n' +
    '    "http://www.w3.org/1999/02/22-rdf-syntax-ns#type" : [ { \n' +
    '      "type" : "literal" ,\n' +
    '      "value" : "http://test.com/ontology/Point"\n' +
    '    }\n' +
    '     ] ,\n' +
    '    "http://test.com/Property/hasX" : [ { \n' +
    '      "type" : "literal" ,\n' +
    '      "value" : "{1}" ,\n' +
    '      "datatype" : "http://www.w3.org/2001/XMLSchema#string"\n' +
    '    }\n' +
    '     ]\n' +
    '  }\n' +
    '   ,\n' +
    '  "http://test.com/upperPoint" : { \n' +
    '    "http://test.com/Property/hasY" : [ { \n' +
    '      "type" : "literal" ,\n' +
    '      "value" : "{2}" ,\n' +
    '      "datatype" : "http://www.w3.org/2001/XMLSchema#string"\n' +
    '    }\n' +
    '     ] ,\n' +
    '    "http://www.w3.org/1999/02/22-rdf-syntax-ns#type" : [ { \n' +
    '      "type" : "literal" ,\n' +
    '      "value" : "http://test.com/ontology/Point"\n' +
    '    }\n' +
    '     ] ,\n' +
    '    "http://test.com/Property/hasX" : [ { \n' +
    '      "type" : "literal" ,\n' +
    '      "value" : "{3}" ,\n' +
    '      "datatype" : "http://www.w3.org/2001/XMLSchema#string"\n' +
    '    }\n' +
    '     ]\n' +
    '  }\n' +
    '   ,\n' +
    '  "http://test.com/aRegionInstance" : { \n' +
    '    "http://test.com/Property/upperPoint" : [ { \n' +
    '      "type" : "uri" ,\n' +
    '      "value" : "http://test.com/upperPoint"\n' +
    '    }\n' +
    '     ] ,\n' +
    '    "http://test.com/Property/lowerPoint" : [ { \n' +
    '      "type" : "uri" ,\n' +
    '      "value" : "http://test.com/lowerPoint"\n' +
    '    }\n' +
    '     ] ,\n' +
    '    "http://www.w3.org/1999/02/22-rdf-syntax-ns#type" : [ { \n' +
    '      "type" : "literal" ,\n' +
    '      "value" : "http://test.com/ontology/Region"\n' +
    '    }\n' +
    '     ] ,\n' +
    '    "http://test.com/Property/referenceSystem" : [ { \n' +
    '      "type" : "literal" ,\n' +
    '      "value" : "EPSG:4326"\n' +
    '    }\n' +
    '     ]\n' +
    '  }\n' +
    '}';


String.prototype.format = function (args) {
    var str = this;
    return str.replace(String.prototype.format.regex, function(item) {
        var intVal = parseInt(item.substring(1, item.length - 1));
        var replace;
        if (intVal >= 0) {
            replace = args[intVal];
        } else if (intVal === -1) {
            replace = "{";
        } else if (intVal === -2) {
            replace = "}";
        } else {
            replace = "";
        }
        return replace;
    });
};
String.prototype.format.regex = new RegExp("{-?[0-9]+}", "g");

function generateSemanticRegion(y_min,x_min,y_max,x_max){
    return regionTemplate.format([y_min, x_min, y_max, x_max]);
}