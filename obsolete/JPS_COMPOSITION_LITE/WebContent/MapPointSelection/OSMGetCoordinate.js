var osmb = new OSMBuildings({
    container: 'map',
    position: { latitude: 52.52000, longitude: 13.41000 },
    zoom: 16,
    minZoom: 15,
    maxZoom: 22
});

osmb.addMapTiles(
    'https://{s}.tiles.mapbox.com/v3/osmbuildings.kbpalbpk/{z}/{x}/{y}.png',
    {
        attribution: '© Data <a href="http://openstreetmap.org/copyright/">OpenStreetMap</a> · © Map <a href="http://mapbox.com">Mapbox</a>'
    }
);

osmb.addGeoJSONTiles('http://{s}.data.osmbuildings.org/0.2/anonymous/tile/{z}/{x}/{y}.json');

osmb.on('pointerdown', function(e) {
     send(osmb.getPosition().longitude, osmb.getPosition().latitude);
});


var hostname = window.location.href.split("?")[0].replace('osm.html', '').replace('/MapPointSelection','');
var executionChain = window.location.href.split("data=")[1];


function send(lon, lat) {
    console.log('triggered');
    var data = '{"http://www.theworldavatar.com/OntoEIP/OntoCAPE/OntoCAPE/upper_level/coordinate_system.owl#Coordinate":{ "http://www.theworldavatar.com/OntoCAPE/OntoCAPE/supporting_concepts/space_and_time/space_and_time_extended.owl#space_and_time_extended:hasProjectedCoordinate_y":[{"type":"literal","value":' + lat + '}],"http://www.theworldavatar.com/OntoCAPE/OntoCAPE/supporting_concepts/space_and_time/space_and_time_extended.owl#space_and_time_extended:hasProjectedCoordinate_x":[{"type":"literal","value":' + lon + '}]}}'
    var myUrl = hostname + 'ServiceExecutionEndpoint?executionChain=' + executionChain + '&value=' + encodeURIComponent(data);
    var request = $.ajax({
        url: myUrl,
        type: 'GET',
        contentType: 'application/json; charset=utf-8'
    });

    request.done(function (data) {
        $('#result').text(data);
        console.log('data', data)
        document.getElementById('frame').src = hostname +  "/OutputVisualization/visualizationOfWeather001.html?value=" +  data;

    });

    request.fail(function (jqXHR, textStatus) {
        // your failure code here
    });
}