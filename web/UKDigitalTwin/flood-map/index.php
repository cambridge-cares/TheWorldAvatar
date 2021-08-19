<!DOCTYPE html>
<html>

<head>
	<title>Flood Risk Visualisation</title>
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
			center: [0.24214, 52.99018]
		});

		// Override default pitch camera defined in mapbox-controls.js
		overrideDefaultPitch({
			curve: 1.9,
			speed: 1.6,
			zoom: 8.25,
			pitch: 65,
			bearing: -30,
			center: [0.21077, 52.97009]
		});

		// Override default map options defined in mapbox-controls.js
		overrideDefaultMap({
			container: "map",
			style: "mapbox://styles/mapbox/light-v10?optimize=true",
			center: [0.24214, 52.99018],
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
	
		// Register "Flood" layers
		registerLayer("Fluvial Models", ["flood_fluvial"], "Flood Zone 3", true);
		registerLayer("Tidal Models", ["flood_tidal"], "Flood Zone 3", true);
		registerLayer("Fluvial/Tidal Models", ["flood_both"], "Flood Zone 3", true);
		currentLayers.push("flood_fluvial");
		currentLayers.push("flood_tidal");
		currentLayers.push("flood_both");

		// Register "At Risk Assets" layers
		registerLayer("Power Generation", ["flooded_power_icons", "flooded_power_labels"], "At Risk Assets", true);
		currentLayers.push("flooded_power_icons");
		currentLayers.push("flooded_power_labels");

		registerLayer("Gas Terminals", ["flooded_gas_terminal_icons", "flooded_gas_terminal_labels"], "At Risk Assets", true);
		registerLayer("Gas Offtakes", ["flooded_gas_offtake_icons", "flooded_gas_offtake_labels"], "At Risk Assets", true);
		registerLayer("Gas Pipes", ["flooded_pipe_icons"], "At Risk Assets", true);
		currentLayers.push("flooded_gas_terminal_icons");
		currentLayers.push("flooded_gas_terminal_labels");
		currentLayers.push("flooded_gas_offtake_icons");
		currentLayers.push("flooded_gas_offtake_labels");
		currentLayers.push("flooded_pipe_icons");

		registerLayer("Crops", ["flooded_crop_icons"], "At Risk Assets", true);
		currentLayers.push("flooded_crop_icons");

		// Register "All Assets" layers
		registerLayer("Power Generation", ["all_power_icons", "all_power_labels"], "All Assets", false);

		registerLayer("Gas Terminals", ["all_gas_terminal_icons", "all_gas_terminal_labels"], "All Assets", false);
		registerLayer("Gas Offtakes", ["all_gas_offtake_icons", "all_gas_offtake_labels"], "All Assets", false);
		registerLayer("Gas Pipes", ["all_pipe_icons"], "All Assets", false);

		registerLayer("Crops", ["cambridgeshire_crop_icons", "norfolk_crop_icons", "suffolk_crop_icons"], "All Assets", false);

		// Build the layer selection tree
		buildLayerTree("checkbox");
		$("ul.checktree").checktree();

		// On initial load...
		map.on("load", function() {
			resetSidePanel();

			updateLegend(currentLayers);
			var powerLegendButton = document.getElementById("flood-button");
			powerLegendButton.click();
		});

		map.on('style.load', function() {
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
			map.addSource('cambridgeshire_crops', {
				type: 'vector',
				url: 'mapbox://cmclinnovations.2r3dmaiz',
			});
			map.addSource('norfolk_crops', {
				type: 'vector',
				url: 'mapbox://cmclinnovations.aauu9r35',
			});
			map.addSource('suffolk_crops', {
				type: 'vector',
				url: 'mapbox://cmclinnovations.cbyd44fh',
			});
			map.addSource('flooded_crops', {
				type: 'vector',
				url: 'mapbox://cmclinnovations.8yd3a9pu',
			});

			// Add the powerplant data
			map.addSource('all_power', {
				type: 'geojson',
				data: 'data/power/all_power.geojson'
			});
			map.addSource('flooded_power', {
				type: 'geojson',
				data: 'data/power/flooded_power.geojson'
			});

			// Add the gas data
			map.addSource('all_gas_pipes', {
				type: 'geojson',
				data: 'data/gas/all_pipe_network.geojson'
			});
			map.addSource('all_gas_offtakes', {
				type: 'geojson',
				data: 'data/gas/all_offtakes.geojson',
			});
			map.addSource('all_gas_terminals', {
				type: 'geojson',
				data: 'data/gas/all_terminals.geojson'
			});
			map.addSource('flooded_gas_pipes', {
				type: 'geojson',
				data: 'data/gas/flooded_pipe_network.geojson'
			});
			map.addSource('flooded_gas_offtakes', {
				type: 'geojson',
				data: 'data/gas/flooded_offtakes.geojson',
			});
			map.addSource('flooded_gas_terminals', {
				type: 'geojson',
				data: 'data/gas/flooded_terminals.geojson'
			});

			// Add the flood data
			map.addSource('flood', {
				type: 'vector',
				url: 'mapbox://cmclinnovations.c94vzz6r',
				// type: 'geojson',
				// data: 'data/flood/flood.geojson'
			});
			// ============ DATA SOURCES ===========	

			
			// ============ LOCATION ICONS ===========	
			// Crop locations
			map.addLayer({
				"id": "cambridgeshire_crop_icons",
				"source": "cambridgeshire_crops",
				"source-layer": "data",
				"type": "circle",
				'layout': {
					'visibility': currentLayers.includes("cambridgeshire_crop_icons") ? 'visible' : 'none'
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
			map.addLayer({
				"id": "norfolk_crop_icons",
				"source": "norfolk_crops",
				"source-layer": "data",
				"type": "circle",
				'layout': {
					'visibility': currentLayers.includes("norfolk_crops") ? 'visible' : 'none'
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
			map.addLayer({
				"id": "suffolk_crop_icons",
				"source": "suffolk_crops",
				"source-layer": "data",
				"type": "circle",
				'layout': {
					'visibility': currentLayers.includes("suffolk_crop_icons") ? 'visible' : 'none'
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
			map.addLayer({
				"id": "flooded_crop_icons",
				"source": "flooded_crops",
				"source-layer": "flood_crops",
				"type": "circle",
				'layout': {
					'visibility': currentLayers.includes("flooded_crop_icons") ? 'visible' : 'none'
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
				'id': 'all_power_icons',
				'type': 'circle',
				'source': 'all_power',
				'layout': {
					'visibility': currentLayers.includes("all_power_icons") ? 'visible' : 'none'
				},
				'paint': {
					'circle-radius': ["+", 3, ["*", 0.85, ["ln", ["to-number", ["get", "capacity"]]]]],
					'circle-color': ["get", "marker-color"],
					'circle-stroke-width': 1,
					'circle-stroke-color': inner_text,
					"circle-opacity": 1
				}
			});
			map.addLayer({
				'id': 'flooded_power_icons',
				'type': 'circle',
				'source': 'flooded_power',
				'layout': {
					'visibility': currentLayers.includes("flooded_power_icons") ? 'visible' : 'none'
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
                'id': 'all_pipe_icons',
                'type': 'line',
                'source': 'all_gas_pipes',
                'layout': {
					'visibility': currentLayers.includes("all_pipe_icons") ? 'visible' : 'none',
                    'line-join': 'round',
                    'line-cap': 'round'
                },
                'paint': {
                    'line-color': pipe_color,
                    'line-width': 4
                }
            });
			map.addLayer({
				'id': 'all_gas_offtake_icons',
				'type': 'symbol',
				'source': 'all_gas_offtakes',
				'layout': {
					'visibility': currentLayers.includes("all_gas_offtake_icons") ? 'visible' : 'none',
					'icon-image': currentGasOfftakeIcon,
					'icon-size': 0.5,
					'icon-allow-overlap': true
                }
			});
			map.addLayer({
				'id': 'all_gas_terminal_icons',
				'type': 'symbol',
				'source': 'all_gas_terminals',
				'layout': {
					'visibility': currentLayers.includes("all_gas_terminal_icons") ? 'visible' : 'none',
					'icon-image': currentGasTerminalIcon,
					'icon-size': 0.8,
					'icon-allow-overlap': true
                }
			});
			map.addLayer({
                'id': 'flooded_pipe_icons',
                'type': 'line',
                'source': 'flooded_gas_pipes',
                'layout': {
					'visibility': currentLayers.includes("flooded_pipe_icons") ? 'visible' : 'none',
                    'line-join': 'round',
                    'line-cap': 'round'
                },
                'paint': {
                    'line-color': pipe_color,
                    'line-width': 4
                }
            });
			map.addLayer({
				'id': 'flooded_gas_offtake_icons',
				'type': 'symbol',
				'source': 'flooded_gas_offtakes',
				'layout': {
					'visibility': currentLayers.includes("flooded_gas_offtake_icons") ? 'visible' : 'none',
					'icon-image': currentGasOfftakeIcon,
					'icon-size': 0.5,
					'icon-allow-overlap': true
                }
			});
			map.addLayer({
				'id': 'flooded_gas_terminal_icons',
				'type': 'symbol',
				'source': 'flooded_gas_terminals',
				'layout': {
					'visibility': currentLayers.includes("flooded_gas_terminal_icons") ? 'visible' : 'none',
					'icon-image': currentGasTerminalIcon,
					'icon-size': 0.8,
					'icon-allow-overlap': true
                }
			});

			// Flood area
			map.addLayer({
				'id': 'flood_tidal',
				'type': 'fill',
				'source': 'flood', 
				"source-layer": "flood",
				'layout': {
					'visibility': currentLayers.includes("flood_tidal") ? 'visible' : 'none'
				},
				'paint': {
					'fill-color': floodFilter,
					'fill-opacity': 0.3
				},
				'filter': ["==", "type", "Tidal Models"]
			});
			map.addLayer({
				'id': 'flood_fluvial',
				'type': 'fill',
				'source': 'flood', 
				"source-layer": "flood",
				'layout': {
					'visibility': currentLayers.includes("flood_fluvial") ? 'visible' : 'none'
				},
				'paint': {
					'fill-color': floodFilter,
					'fill-opacity': 0.3
				},
				'filter': ["==", "type", "Fluvial Models"]
			});
			map.addLayer({
				'id': 'flood_both',
				'type': 'fill',
				'source': 'flood', 
				"source-layer": "flood",
				'layout': {
					'visibility': currentLayers.includes("flood_both") ? 'visible' : 'none'
				},
				'paint': {
					'fill-color': floodFilter,
					'fill-opacity': 0.3
				},
				'filter': ["==", "type", "Fluvial / Tidal Models"]
			});

			// ============ LOCATION ICONS ===========	

			// ============ LOCATION LABELS ===========	
			// No crop labels, too many locations.

			// Powerplant labels
			addLabels(
				map, 
				"all_power_labels",
				"all_power",
				inner_text, 
				outer_text,
				"name",
				currentLayers.includes("all_power_labels") ? 'visible' : 'none'
			);
			addLabels(
				map, 
				"flooded_power_labels",
				"flooded_power",
				inner_text, 
				outer_text,
				"name",
				currentLayers.includes("flooded_power_labels") ? 'visible' : 'none'
			);

			// Gas labels
			addLabels(
				map,
				"all_gas_terminal_labels",
				"all_gas_terminals",
				inner_text,
				outer_text,
				"name",
				currentLayers.includes("all_gas_terminal_labels") ? 'visible' : 'none'
			);
			addLabels(
				map,
				"all_gas_offtake_labels",
				"all_gas_offtakes",
				inner_text,
				outer_text,
				"Offtake Point (License Name)",
				currentLayers.includes("all_gas_offtake_labels") ? 'visible' : 'none'
			);
			addLabels(
				map,
				"flooded_gas_terminal_labels",
				"flooded_gas_terminals",
				inner_text,
				outer_text,
				"name",
				currentLayers.includes("flooded_gas_terminal_labels") ? 'visible' : 'none'
			);
			addLabels(
				map,
				"flooded_gas_offtake_labels",
				"flooded_gas_offtakes",
				inner_text,
				outer_text,
				"Offtake Point (License Name)",
				currentLayers.includes("flooded_gas_offtake_labels") ? 'visible' : 'none'
			);

			// No flood label, they're polygons
			// ============ LOCATION LABELS ===========	

			// ============ EVENTS ============
			addMouseEffects(map, "all_power_icons");
			addMouseEffects(map, "flooded_power_icons");

			addMouseEffects(map, "all_gas_terminal_icons");
			addMouseEffects(map, "all_gas_offtake_icons");
			addMouseEffects(map, "flooded_gas_terminal_icons");
			addMouseEffects(map, "flooded_gas_offtake_icons");

			addMouseEffects(map, "cambridgeshire_crop_icons");
			addMouseEffects(map, "norfolk_crop_icons");
			addMouseEffects(map, "suffolk_crop_icons");
			addMouseEffects(map, "flooded_crop_icons");
			// ============ EVENTS ============
		});

		
	</script>

</body>

</html>