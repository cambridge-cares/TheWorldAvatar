<!DOCTYPE html>
<html>

<head>
	<title>Digital Twin Example</title>
	<meta charset="utf-8">

	<link href='https://api.tiles.mapbox.com/mapbox-gl-js/v2.3.0/mapbox-gl.css' rel='stylesheet' />
	<link href="https://www.w3schools.com/w3css/4/w3.css" rel="stylesheet">

	<script src='https://api.mapbox.com/mapbox-gl-js/v2.3.0/mapbox-gl.js'></script>
	<script src='https://ajax.googleapis.com/ajax/libs/jquery/3.5.1/jquery.min.js'></script>
	<script src="https://cdn.jsdelivr.net/npm/moment@2.29.1"></script>
	<script src="https://cdnjs.cloudflare.com/ajax/libs/Chart.js/3.5.1/chart.js"></script>
	<script src="https://cdn.jsdelivr.net/npm/chartjs-adapter-moment@1.0.0"></script>


	<link href='./css/digital_twin_style.css' rel='stylesheet' />
	<link href="./css/style.css" rel="stylesheet"> 

	<script src='./js/digital_twin_manager.js'></script>
	<script src='./js/digital_twin_module.js'></script>
	<script src='./js/digital_twin_controls.js'></script>
	<script src='./js/digital_twin_tree.js'></script>
	<script src='./js/digital_twin_side_panel.js'></script>
	<script src='./js/digital_twin_time_series.js'></script>

	<script src='./js/data_registry.js'></script>
	<script src='./js/data_handler.js'></script>
	<script src='./js/mapbox_utils.js'></script>
	<script src='./js/layer_handler.js'></script>
	<script src='./js/layer_constants.js'></script>

</head>

<body>
	<!-- Element the MapBox map will be added to -->
	<div id='map'></div>
	
	<!-- Element the map controls will be added to -->
	<div id="controlsParent"></div>

	<!-- Non-module JS block, variables in here are global-->
	<script>
		let manager = null;
	</script>

	<!-- Module JS block, variables are local (i.e. cannot access from HTML elements) -->
	<script type="module">

		// Initialise a DigitalTwinManager  (and store as a global variable)
		manager = new DigitalTwinManager();

		// Load all the metadata
		var dataRegistry = new DigitalTwinDataRegistry();
		dataRegistry.loadMetaData("http://localhost/data/new/overall-meta.json", startUp);


		// Will run once metadata is loaded asynchronously
		function startUp() {
			
			// Create the MapBox map
			let map = manager.createMap(
				"map", 
				dataRegistry.overallMeta["apiKey"],
				dataRegistry.overallMeta["defaultCenter"],
				dataRegistry.overallMeta["defaultZoom"]
			);

			// Every time a MapBox changes style (i.e. Terrain), it will remove all data sources and layers.
			// This means that we have to listen for this event (below) and re-add sources and re-apply layers.
			var loadedOnce = false;

			map.on('style.load', function() {
				console.log("INFO: A new style has been loaded.");

				// 
				var dataHandler = new DigitalTwinDataHandler(dataRegistry, map);
				dataHandler.addFixedSources();
				dataHandler.addAdditionalSources(["scenario-0", "time-0"]);

				//
				var layerHandler = new DigitalTwinLayerHandler(dataRegistry, map);
				layerHandler.addFixedLayers();
				layerHandler.addAdditionalLayers(["scenario-0", "time-0"]);

				// Load all modules
				//manager.loadModules();

				// On first load only
				if(!loadedOnce) {
					// Create a legend built from the now loaded modules.
					//DT.sidePanelHandler.buildLegend();
					
					// Register the default state of the side panel
					//DT.sidePanelHandler.storeDefault();

					//var firstLegend = document.getElementsByClassName("w3-button tablink")[0];
					//if(firstLegend != null) firstLegend.click();

					loadedOnce;
				}
			});
		}


		

		

		// // Build the layer tree
		// manager.buildControls("./data/layer-tree.json");
		
		// // Import your custom modules
		// import { CambridgeshirePointsModule } from "./js/cambridgeshire-points.js";
		// import { CambridgeshireRegionModule } from "./js/cambridgeshire-regions.js";

		// // Add these modules to the manager
		// // Note that data will be added in the same order as the modules.
		// manager.addModule(new CambridgeshireRegionModule());
		// manager.addModule(new CambridgeshirePointsModule());

		// // Load the time series data into memory whilst the rest of
		// // the visualisation is loading
		// DT.timeSeriesHandler = new DigitalTwinTimeSeries();
		// DT.timeSeriesHandler.readFile("orchards-layer", "daily-data", "./data/orchards-time-series.json")

		// // Example side panel title
		// DT.sidePanelHandler.setTitle("Default Title");

		// // Example side panel content
		// var defaultContent = `
		// 	<div style="height: 100%; border: 1px solid grey;">
		// 		<p>Here is some introductory text, this should be used to detail the overall purpose of the
		// 		visualisation, and perhaps tell the user how they can interact with it.</p>
		// 		<p>Lorem ipsum dolor sit amet, consectetur adipiscing elit. Proin ut magna sed velit dictum varius.
		// 		Vestibulum magna orci, pulvinar eget quam ut, vestibulum pretium justo. Vivamus ut dignissim sem.</p>
		// 		<p>Quisque ut cursus dolor. Fusce vel dui luctus dolor dapibus porttitor. Vestibulum pharetra tortor dolor,
		// 		sit amet varius ex rhoncus convallis. Donec commodo bibendum ligula eget vulputate. Cras a velit suscipit,
		// 		vulputate est sodales, vulputate dui.</p>
		// 		<p>Lorem ipsum dolor sit amet, consectetur adipiscing elit. Proin ut magna sed velit dictum varius.
		// 		Vestibulum magna orci, pulvinar eget quam ut, vestibulum pretium justo. Vivamus ut dignissim sem.</p>
		// 		<p>Quisque ut cursus dolor. Fusce vel dui luctus dolor dapibus porttitor. Vestibulum pharetra tortor dolor,
		// 		sit amet varius ex rhoncus convallis. Donec commodo bibendum ligula eget vulputate. Cras a velit suscipit,
		// 		vulputate est sodales, vulputate dui.</p>
		// 	</div>`;
		// DT.sidePanelHandler.setContent(defaultContent);

		// // Example legend content
		// DT.sidePanelHandler.setLegend(`
		// 	<div style="border: 1px solid grey;">
		// 		<br><br>
		// 		<p>This area is designed to hold a number of legends. Each layer (as shown to 
		// 		the user in the layer tree) should have its own legend to explain the data
		// 		on the map. This could be dynamically generated HTML, or an image file.</p>
		// 		<br><br>
		// 	</div>
		// `);

		// // Example footer content
		// DT.sidePanelHandler.setFooter(`-
		// 	<div style="border: 1px solid grey;">
		// 		Here is some footer content.
		// 	</div>
		// `);


		// Fire on MapBox style change.
		
	</script>

</body>

</html>