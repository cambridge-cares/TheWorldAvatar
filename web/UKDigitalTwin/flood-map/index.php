<!DOCTYPE html>
<html>

<head>
	<title>Flood Visualisation</title>
	<meta charset="utf-8">
	
	<!-- External JS and CSS -->
    <script src='https://api.mapbox.com/mapbox-gl-js/v2.3.0/mapbox-gl.js'></script>
	<script src='https://ajax.googleapis.com/ajax/libs/jquery/3.5.1/jquery.min.js'></script>
	<link href='https://api.tiles.mapbox.com/mapbox-gl-js/v2.3.0/mapbox-gl.css' rel='stylesheet' />
	<link href="https://www.w3schools.com/w3css/4/w3.css" rel="stylesheet">

	<!--
	Remote CMCL JS and CSS are loaded from another file that is populated when building a Docker Image;
	this is because the imports differ for development and production environments, so a different
	version of 'head.html' can be generated depending on the target environment.

	If running locally (i.e. without Docker), copy 'head-dev.html' to 'head.html' temporarily.
	-->
	<?php include 'head.html'; ?>
	

	<!-- Local JS and CSS -->
	<script src='flood-vis.js'></script>
	<script src='data/crop/land-use-dict.js'></script>
	<link href='style.css' rel='stylesheet' />
</head>

<body>
	<div id='map'></div>
	<div id="tiltShift"></div>
	<div id="controlsParent"></div>

	<script>
		var currentLayers = [];
		var currentGasTerminalIcon = "triangle-down-black";
		var currentGasOfftakeIcon = "triangle-up-black";

		// Add map controls
		document.getElementById("controlsParent").innerHTML = getControls();

		// Initial state of side-panel (from side-panel.js)
		initialiseSidePanel(document.getElementById("map"));
        resetSidePanel();

		// Show loading icon whilst stuff spins up
        var loadingHTML = `
            <br><br><br><br><br>
            <img width='150' src='loading.gif'></img>
            <br>
            <span style='color: grey; font-style: italic;'>Loading data, please wait...</span>
        `;
		appendSidePanelText(loadingHTML);

		// Override default bird camera defined in mapbox-controls.js
			overrideDefaultBird({
			curve: 1.9,
			speed: 1.6,
			zoom: 8.25,
			pitch: 0.0,
			bearing: 0.0,
			center: [0.26986, 52.68604]
		});

		// Override default pitch camera defined in mapbox-controls.js
		overrideDefaultPitch({
			curve: 1.9,
			speed: 1.6,
			zoom: 9.0,
			pitch: 65,
			bearing: -30,
			center: [0.15557, 52.73462]
		});

		// Override default map options defined in mapbox-controls.js
		overrideDefaultMap({
			container: "map",
			style: "mapbox://styles/mapbox/light-v10?optimize=true",
			center: [0.26986, 52.68604],
			zoom: 8.25,
			pitch: 0.0,
			bearing: 0.0
		});

		// Initialise the MapBox map
		mapboxgl.accessToken = 'pk.eyJ1IjoiY21jbGlubm92YXRpb25zIiwiYSI6ImNrbGdqa3RoNDFnanIyem1nZXR3YzVhcmwifQ.hVk983r6YYlmFE8kSMbzhA';
		var map = new mapboxgl.Map(getDefaultMapOptions());

		// Initialise MapBox popup for mouse-overs
		var popup =  popup = new mapboxgl.Popup({
			closeButton: false,
			closeOnClick: false
		});

		// Change some layer colors depending on selected style
		var inner_text = '#3E3E3E';
		var outer_text = '#FFFFFF';
		var pipe_color = '#B1B1B1';
	
		function terrainCallback(terrainType) {
			if(terrainType === "light") {
                inner_text = '#3E3E3E';
		        outer_text = '#FFFFFF'; 
				pipe_color = '#B1B1B1';

				currentGasOfftakeIcon = "triangle-up-black";
				currentGasTerminalIcon = "triangle-down-black";
				
			} else {
                inner_text = '#FFFFFF';
		        outer_text = '#000000'; 
				pipe_color = '#F6F6F4';

				currentGasOfftakeIcon = "triangle-up-white";
				currentGasTerminalIcon = "triangle-down-white";
			}
			
			console.log(currentLayers);
		}		

		function cameraCallback() {
			popup.remove();
			resetSidePanel();
		}

		function layerCallback(layerID, enabled) {
			map.setLayoutProperty(
				layerID,
				"visibility",
				(enabled ? "visible" : "none")
			);
	
			if(enabled && !currentLayers.includes(layerID)) {
				currentLayers.push(layerID);
			} else if (!enabled && currentLayers.includes(layerID)) {
				currentLayers.splice(currentLayers.indexOf(layerID), 1);
			}
		}

		// Setup map with mapbox-controls.js
		setup(map, cameraCallback, terrainCallback, layerCallback);
	
		// On initial load...
			map.on("load", function() {
			resetSidePanel();

			updateLegend(currentLayers);
			var powerLegendButton = document.getElementById("power-button");
			powerLegendButton.click();
		});

		map.on('style.load', function() {
			console.log("INFO: New style has been loaded.");
			refresh();

			// Load an images from an external URL.
			map.loadImage(
				'legend/gas/triangle-up-black.png',
				(error, image) => { map.addImage('triangle-up-black', image); }
			);
			map.loadImage(
				'legend/gas/triangle-up-white.png',
				(error, image) => { map.addImage('triangle-up-white', image);}
			);
			map.loadImage(
				'legend/gas/triangle-down-black.png',
				(error, image) => { map.addImage('triangle-down-black', image);}
			);
			map.loadImage(
				'legend/gas/triangle-down-white.png',
				(error, image) => { map.addImage('triangle-down-white', image);}
			);
			
			// ============ DATA SOURCES ===========	
			// Add the crop data
			map.addSource('crops', {
				type: 'vector',
				url: 'mapbox://cmclinnovations.aauu9r35',
			});

			// Add the powerplant data
				map.addSource('power', {
				type: 'geojson',
				data: 'data/power/power.geojson'
			});

			// Add the gas data
			map.addSource('gas_pipes', {
				type: 'geojson',
				data: 'data/gas/pipe_network.geojson'
			});
			map.addSource('gas_offtakes', {
				type: 'geojson',
				data: 'data/gas/offtakes.geojson',
			});
			map.addSource('gas_terminals', {
				type: 'geojson',
				data: 'data/gas/terminals.geojson'
			});

			// Add the flood data
			map.addSource('flood', {
				type: 'geojson',
				data: 'data/flood/flood.geojson'
			});
			// ============ DATA SOURCES ===========	

			
			// ============ LOCATION ICONS ===========	
			// Crop locations
			map.addLayer({
				"id": "crop_icons",
				"source": "crops",
				"source-layer": "data",
				"type": "circle",
				'layout': {
					'visibility': currentLayers.includes("crop_icons") ? 'visible' : 'none'
				},
				"paint": {
					"circle-opacity": 0.65,
					"circle-radius": [
						"interpolate",
						["linear"],
						["zoom"],
						10,
						3,
						20,
						15
					],
					"circle-color": crop_colors
				}
			});

			// Powerplant circles
			map.addLayer({
				'id': 'power_icons',
				'type': 'circle',
				'source': 'power',
				'layout': {
					'visibility': currentLayers.includes("power_icons") ? 'visible' : 'none'
				},
				'paint': {
					'circle-radius': ["+", 3, ["*", 0.85, ["ln", ["to-number", ["get", "capacity"]]]]],
					'circle-color': ["get", "marker-color"],
					'circle-stroke-width': 1,
					'circle-stroke-color': inner_text,
					"circle-opacity": 1
				}
			});

			// Gas symbols
            map.addLayer({
                'id': 'pipe_icons',
                'type': 'line',
                'source': 'gas_pipes',
                'layout': {
					'visibility': currentLayers.includes("pipe_icons") ? 'visible' : 'none',
                    'line-join': 'round',
                    'line-cap': 'round'
                },
                'paint': {
                    'line-color': pipe_color,
                    'line-width': 4
                }
            });
			map.addLayer({
				'id': 'gas_offtake_icons',
				'type': 'symbol',
				'source': 'gas_offtakes',
				'layout': {
					'visibility': currentLayers.includes("gas_offtake_icons") ? 'visible' : 'none',
					'icon-image': currentGasOfftakeIcon,
					'icon-size': 0.5,
					'icon-allow-overlap': true
                }
			});
			map.addLayer({
				'id': 'gas_terminal_icons',
				'type': 'symbol',
				'source': 'gas_terminals',
				'layout': {
					'visibility': currentLayers.includes("gas_terminal_icons") ? 'visible' : 'none',
					'icon-image': currentGasTerminalIcon,
					'icon-size': 0.8,
					'icon-allow-overlap': true
                }
			});

			// Flood area
			map.addLayer({
				'id': 'flood_area_fill',
				'type': 'fill',
				'source': 'flood', 
				'layout': {
					'visibility': currentLayers.includes("flood_area_fill") ? 'visible' : 'none'
				},
				'paint': {
					'fill-color': '#0080ff',
					'fill-opacity': 0.33
				}
			});
			map.addLayer({
				'id': 'flood_area_outline',
				'type': 'line',
				'source': 'flood', 
				'layout': {
					'visibility': currentLayers.includes("flood_area_outline") ? 'visible' : 'none'
				},
				'paint': {
					'line-color': '#0080ff',
					'line-width': 1
				}
			});

			// ============ LOCATION ICONS ===========	

			// ============ LOCATION LABELS ===========	
			// No crop labels, too many locations.

			// Powerplant labels
			addLabels(
				map, 
				"power_labels",
				"power",
				inner_text, 
				outer_text,
				"name",
				currentLayers.includes("power_labels") ? 'visible' : 'none'
			);

			// Gas labels
			addLabels(
				map,
				"gas_terminal_labels",
				"gas_terminals",
				inner_text,
				outer_text,
				"name",
				currentLayers.includes("gas_terminal_labels") ? 'visible' : 'none'
			);
			addLabels(
				map,
				"gas_offtake_labels",
				"gas_offtakes",
				inner_text,
				outer_text,
				"Offtake Point (License Name)",
				currentLayers.includes("gas_offtake_labels") ? 'visible' : 'none'
			);

			// No flood label, they're polygons
			// ============ LOCATION LABELS ===========	

			// ============ EVENTS ============
			addMouseEffects(map, "power_icons");
			addMouseEffects(map, "gas_terminal_icons");
			addMouseEffects(map, "gas_offtake_icons");
			addMouseEffects(map, "crop_icons");
			// ============ EVENTS ============

			// Ensure the flood layer is topmost
			map.moveLayer("flood_area_fill");
			map.moveLayer("flood_area_outline");
		});

		// Register the layers 
		registerLayer("Flood Areas", ["flood_area_fill", "flood_area_outline"], null, true);
		currentLayers.push("flood_area_fill");
		currentLayers.push("flood_area_outline");

		registerLayer("Power Generation", ["power_icons", "power_labels"], null, true);
		currentLayers.push("power_icons");
		currentLayers.push("power_labels");

		registerLayer("Gas Grid", ["gas_terminal_icons", "gas_terminal_labels", "gas_offtake_icons", "gas_offtake_labels", "pipe_icons"], null, false);
		registerLayer("Crop Map", ["crop_icons"], null, false);

		// Build the layer selection tree
		buildLayerTree("checkbox");
	</script>

</body>

</html>