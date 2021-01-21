
var light = true;
title = document.getElementById('title');
but1 = document.getElementById('reset');
but2 = document.getElementById('but');
sidebox = document.getElementById('sidecontent');
sidebox_lsoa = document.getElementById('side_lsoa');
sidebox_msoa = document.getElementById('side_msoa');
map_style = document.getElementById('map')



mapboxgl.accessToken = 'pk.eyJ1IjoidG9tc2F2YWdlIiwiYSI6ImNram01MDFvczI5aHUyeG83N21obzBrcWsifQ.T_j8LAT55g3kuwCQFlNPuA';
var map = new mapboxgl.Map({
    container: 'map',
    style: 'mapbox://styles/mapbox/dark-v9',
    zoom: 4.9,
    center: [-3, 54.25],

});

function toggleSidebar(id) {
    var elem = document.getElementById(id);
    var classes = elem.className.split(' ');
    var collapsed = classes.indexOf('collapsed') !== -1;

    var padding = {};

    if (collapsed) {
        // Remove the 'collapsed' class from the class list of the element, this sets it back to the expanded state.
        classes.splice(classes.indexOf('collapsed'), 1);
        padding[id] = 300; // In px, matches the width of the sidebars set in .sidebar CSS class
    }
    else {
        padding[id] = 0;
        // Add the 'collapsed' class to the class list of the element
        classes.push('collapsed');

    }

    // Update the class list on the element
    elem.className = classes.join(' ');
}

var lsoahoveredStateId = null;
var msoahoveredStateId = null;
var hoveredStateId = null;
map.on('load', function () {



    map.addSource('local-super', {
        type: 'vector',
        url: 'mapbox://tomsavage.82bu6k2e',

    });
    map.addLayer({
        'id': 'lsoa-data',
        'type': 'fill',
        'source': 'local-super',
        'source-layer': 'LSOA',
        'layout': {},
        'paint': {
            'fill-color': '#C4C4C4',
            'fill-opacity': [
                'case',
                ['boolean', ['feature-state', 'hover'], false],
                0.6,
                0
            ]
        }
    });


    map.on('mousemove', 'lsoa-data', function (e) {

        sidebox_lsoa.textContent ='LSOA: '+e.features[0].properties.LSOA11NM;
        if (e.features.length > 0) {
            if (lsoahoveredStateId) {

                map.setFeatureState(
                    { source: 'local-super', sourceLayer: 'LSOA', id: lsoahoveredStateId },
                    { hover: false }

                );
            }
            lsoahoveredStateId = e.features[0].id;
            map.setFeatureState(
                { source: 'local-super', sourceLayer: 'LSOA', id: lsoahoveredStateId },
                { hover: true }
            );
        }
    });

    // When the mouse leaves the state-fill layer, update the feature state of the
    // previously hovered feature.
    map.on('mouseleave', 'lsoa-data', function () {
        sidebox_lsoa.textContent = '';
        if (lsoahoveredStateId) {
            map.setFeatureState(
                { source: 'local-super', sourceLayer: 'LSOA', id: lsoahoveredStateId },
                { hover: false }
            );
        }
        lsoahoveredStateId = null;
    });

    map.addSource('middle-super', {
        type: 'vector',
        url: 'mapbox://tomsavage.27ykfxd0',

    });
    map.addLayer({
        'id': 'msoa-data',
        'type': 'fill',
        'source': 'middle-super',
        'source-layer': 'MSOA',
        'layout': {},
        'paint': {
            'fill-color': '#C4C4C4',
            'fill-opacity': [
                'case',
                ['boolean', ['feature-state', 'hover'], false],
                0.4,
                0
            ]
        }
    });


    map.on('mousemove', 'msoa-data', function (e) {

        sidebox_msoa.textContent ='MSOA: '+e.features[0].properties.MSOA11NM;
        if (e.features.length > 0) {
            if (msoahoveredStateId) {

                map.setFeatureState(
                    { source: 'middle-super', sourceLayer: 'MSOA', id: msoahoveredStateId },
                    { hover: false }

                );
            }
           msoahoveredStateId = e.features[0].id;
            map.setFeatureState(
                { source: 'middle-super', sourceLayer: 'MSOA', id: msoahoveredStateId },
                { hover: true }
            );
        }
    });

    // When the mouse leaves the state-fill layer, update the feature state of the
    // previously hovered feature.
    map.on('mouseleave', 'msoa-data', function () {
        sidebox_msoa.textContent = '';
        if (msoahoveredStateId) {
            map.setFeatureState(
                { source: 'middle-super', sourceLayer: 'MSOA', id: msoahoveredStateId },
                { hover: false }
            );
        }
   msoahoveredStateId = null;
    });

    // add a sky layer that will show when the map is highly pitched
    map.addLayer({
        'id': 'sky',
        'type': 'sky',
        'paint': {
            'sky-type': 'gradient',
            'sky-gradient': ["interpolate", ["linear"], ["sky-radial-progress"], 0.8, "#191A1A"]
        }
    });

    document.getElementById('reset').addEventListener('click', function () {
        toggleSidebar('right');
        map.flyTo({
            center: [-3, 54.25],
            curve: 2,
            speed: 1,
            pitch: 0,
            zoom: 5,
            bearing: 0
        });
    });

    thumb_loc = 'https://tom-savage.co.uk/assets/images/pressure2.png';

    map.loadImage(
        thumb_loc,
        function (error, image) {
            if (error) throw error;
            map.addImage('custom-marker', image);

            map.addSource('terminals', {
                'type': 'geojson',
                'data': 'https://tom-savage.co.uk/assets/geojson/terminals.geojson'
            });
            term_name_color = '#C4C4C4'
            term_outline_color = '#202028'
            // Add a symbol layer
            map.addLayer({
                'id': 'terminals',
                'type': 'symbol',
                'source': 'terminals',
                'layout': {

                    'icon-image': 'custom-marker',
                    // get the title name from the source's "name" property
                    'text-field': ['format', ['get', 'name'], { 'font-scale': 0.85 }],
                    'text-font': [
                        'Open Sans Semibold',
                        'Arial Unicode MS Bold'
                    ],
                    'text-offset': [0, 1.25],
                    'text-anchor': 'top'
                },
                'paint': {
                    'text-color': term_name_color,
                    'text-halo-color': term_outline_color,
                    'text-halo-width': 1
                }
            });
        }
    );


    var layers = map.getStyle().layers;
    var labelLayerId;
    for (var i = 0; i < layers.length; i++) {
        if (layers[i].type === 'symbol' && layers[i].layout['text-field']) {
            labelLayerId = layers[i].id;
            break;
        }
    }




    map.addSource('National Transmission System', {
        type: 'geojson',
        data: 'https://tom-savage.co.uk/assets/geojson/pipe_network.geojson'
    });
    map.addSource('OHL', {
        type: 'geojson',
        data: 'https://tom-savage.co.uk/assets/geojson/OHL.geojson'
    });
    map.addSource('LDZ', {
        type: 'geojson',
        data: 'https://tom-savage.co.uk/assets/geojson/local_dist_zones.geojson'
    });

    pipe_color = '#c3e1e8';

    map.addLayer({
        'id': 'National Transmission System',
        'type': 'line',
        'source': 'National Transmission System',
        'layout': {
            'line-join': 'round',
            'line-cap': 'round'
        },
        'paint': {
            'line-color': pipe_color,
            'line-opacity': 0.75,
            'line-width': 2
        }
    });


    map.addLayer({
        'id': 'LocalDistributionZones',
        'type': 'fill',
        'source': 'LDZ',
        'layout': {},
        'paint': {
            'fill-color': '#C4C4C4',
            'fill-opacity': [
                'case',
                ['boolean', ['feature-state', 'hover'], false],
                0.3,
                0
            ]
        }
    });


    map.on('mousemove', 'LocalDistributionZones', function (e) {
        sidebox.textContent = e.features[0].properties.Name + ' Local Distribution Zone';
        if (e.features.length > 0) {
            if (hoveredStateId) {
                map.setFeatureState(
                    { source: 'LDZ', id: hoveredStateId },
                    { hover: false }

                );
            }
            hoveredStateId = e.features[0].id;
            map.setFeatureState(
                { source: 'LDZ', id: hoveredStateId },
                { hover: true }
            );
        }
    });

    // When the mouse leaves the state-fill layer, update the feature state of the
    // previously hovered feature.
    map.on('mouseleave', 'LocalDistributionZones', function () {
        sidebox.textContent = '';
        if (hoveredStateId) {
            map.setFeatureState(
                { source: 'LDZ', id: hoveredStateId },
                { hover: false }
            );
        }
        hoveredStateId = null;
    });

    line_color = '#EBEBB9';

    map.addLayer({
        'id': 'Overhead Electrical Lines',
        'type': 'line',
        'source': 'OHL',
        'layout': {
            'line-join': 'round',
            'line-cap': 'round'
        },
        'paint': {
            'line-color': line_color,
            'line-opacity': 0.75,
            'line-width': 2
        }
    });




    map.on('click', 'terminals', function (e) {


        toggleSidebar('right');
        var coordinates = e.features[0].geometry.coordinates;
        map.flyTo({
            center: coordinates,
            curve: 1.9,
            speed: 1.2,
            pitch: 45,
            zoom: 16,
            bearing: Math.random() * 360
        });

    });


    map.on('mouseenter', 'terminals', function () {
        map.getCanvas().style.cursor = 'pointer';
    });

    map.on('mouseleave', 'terminals', function () {
        map.getCanvas().style.cursor = '';
    });




    // map.addSource('Pipe Buffer', {
    //     type: 'geojson',
    //     data: 'https://tom-savage.co.uk/assets/geojson/pipe_buffer.geojson'
    // });

    // map.addLayer({
    //     'id': 'Pipe Extrusion',
    //     'type': 'fill-extrusion',
    //     'source': 'Pipe Buffer',
    //     'paint': {
    //         // See the Mapbox Style Specification for details on data expressions.
    //         // https://docs.mapbox.com/mapbox-gl-js/style-spec/#expressions

    //         // Get the fill-extrusion-color from the source 'color' property.
    //         'fill-extrusion-color': pipe_color,

    //         // Get fill-extrusion-height from the source 'height' property.
    //         'fill-extrusion-height': 15,

    //         // Get fill-extrusion-base from the source 'base_height' property.
    //         'fill-extrusion-base': 0,

    //         // Make extrusions slightly opaque for see through indoor walls.
    //         'fill-extrusion-opacity': 1
    //     }
    // });

    // Loading 3D buildings when zoomed in close enough 
    map.addLayer(
        {
            'id': '3d-buildings',
            'source': 'composite',
            'source-layer': 'building',
            'filter': ['==', 'extrude', 'true'],
            'type': 'fill-extrusion',
            'minzoom': 15,
            'paint': {
                'fill-extrusion-color': '#919191',

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
                    ['get', 'max_height']
                ],
                'fill-extrusion-opacity': 0.2
            }
        },
        labelLayerId
    );


});

// enumerate ids of the layers
var toggleableLayerIds = ['National Transmission System', 'Overhead Electrical Lines'];

// set up the corresponding toggle button for each layer
for (var i = 0; i < toggleableLayerIds.length; i++) {
    var id = toggleableLayerIds[i];

    var link = document.createElement('a');
    link.href = '#';
    link.className = 'active';
    link.textContent = id;

    link.onclick = function (e) {
        var clickedLayer = this.textContent;
        e.preventDefault();
        e.stopPropagation();

        var visibility = map.getLayoutProperty(clickedLayer, 'visibility');

        // toggle layer visibility by changing the layout object's visibility property
        if (visibility === 'visible') {
            map.setLayoutProperty(clickedLayer, 'visibility', 'none');
            this.style.backgroundColor = '#C4C4C4'
            this.style.color = '#191a1a'
        } else {
            map.setLayoutProperty(clickedLayer, 'visibility', 'visible');
            this.style.color = '#C4C4C4'
            this.style.backgroundColor = '#191a1a'

        }
    };

    var layers = document.getElementById('menu');
    layers.appendChild(link);
}