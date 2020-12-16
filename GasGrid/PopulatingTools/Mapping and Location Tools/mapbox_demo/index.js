// TO MAKE THE MAP APPEAR YOU MUST
// ADD YOUR ACCESS TOKEN FROM
// https://account.mapbox.com

mapboxgl.accessToken = 'pk.eyJ1IjoidG9tc2F2YWdlIiwiYSI6ImNraXFhd3M5cDFzcWYycWxidW9xczllMTQifQ.RwsluDajLawc0joMBeimag';
var map = new mapboxgl.Map({
container: 'map',
style: 'mapbox://styles/mapbox/light-v10', // stylesheet location
center: [-3, 54.5], // starting position [lng, lat]
zoom: 4, // starting zoom
pitch: 0,
bearing:0,
});


// Loading 3D terrain
map.on('load', function () {
map.addSource('mapbox-dem', {
    'type': 'raster-dem',
    'url': 'mapbox://mapbox.mapbox-terrain-dem-v1',
    'tileSize': 512,
    'maxzoom': 14
});


var layers = map.getStyle().layers;
 
var labelLayerId;
for (var i = 0; i < layers.length; i++) {
    if (layers[i].type === 'symbol' && layers[i].layout['text-field']) {
        labelLayerId = layers[i].id;
        break;
    }
}


// add the DEM source as a terrain layer with exaggerated height


terrain_exaggeration = 1.5;
map.setTerrain({ 'source': 'mapbox-dem', 'exaggeration': terrain_exaggeration});

// add a sky layer that will show when the map is highly pitched
map.addLayer({
    'id': 'sky',
    'type': 'sky',
    'paint': {
    'sky-type': 'atmosphere',
    'sky-atmosphere-sun': [0.0, 0.0],
    'sky-atmosphere-sun-intensity': 15
    }   
});


// map.addSource('National Transmission System', {
// type: 'geojson',
// data: 'https://tom-savage.co.uk/assets/geojson/pipe_network.geojson'
// });

// map.addLayer({
// 'id': 'National Transmission System',
// 'type': 'line',
// 'source': 'National Transmission System',
// 'layout': {
// 'line-join': 'round',
// 'line-cap': 'round'
// },
// 'paint': {
// 'line-color': '#319BFF',
// 'line-width': 3
// }
// });



// This section is concerned with plotting the geojson file
map.addSource('Contour', {
    type: 'geojson',
    data: 'https://tom-savage.co.uk/assets/geojson/contour.geojson'
});

map.addLayer({
    'id': 'Contour-Fill',
    'type': 'fill',
    'source': 'Contour',
    'layout': {},
    'paint': {
    'fill-color': {
        type: 'identity',
        property: 'fill',
        },
    'fill-opacity': {
        type: 'identity',
        property: 'fill-opacity'
        }
    }
});

// Loading 3D buildings when zoomed in close enough 
map.addLayer({
    'id': '3d-buildings',
    'source': 'composite',
    'source-layer': 'building',
    'filter': ['==', 'extrude', 'true'],
    'type': 'fill-extrusion',
    'minzoom': 15,
    'paint': {
        'fill-extrusion-color': '#aaa',
        
        // use an 'interpolate' expression to add a smooth transition effect to the
        // buildings as the user zooms in
        'fill-extrusion-height': [
        'interpolate',
        ['linear'],
        ['zoom'],
        15,
        0,
        15.05,
        ['get', 'height']
        ],
        'fill-extrusion-base': [
        'interpolate',
        ['linear'],
        ['zoom'],
        15,
        0,
        15.05,
        ['get', 'min_height']
        ],
        'fill-extrusion-opacity': 0.6
        }
    },
labelLayerId
);

});