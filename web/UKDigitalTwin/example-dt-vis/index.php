<!DOCTYPE html>
<html>

<head>
	<title>Digital Twin Example</title>
	<meta charset="utf-8">

	<!--
		JS and CSS files are loaded from another file that is populated when building a Docker Image;
		this is because the imports differ for development and production environments, so a different
		version of 'head.html' can be generated depending on the target environment.

		If running locally (without Docker), copy 'head-dev.html' or 'head-prod.html' to 'head.html' temporarily.
	-->
	<?php include 'head.html'; ?>

	<!-- Visualisation specific JS and CSS -->
	<!-- ANY NON-DIGITALTWINMODULE SCRIPTS GO HERE -->
	<script src='./digital_twin_time_series.js'></script>

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

		// Initialise a DT manager (and store as a global variable)
		manager = new DigitalTwinManager();

		// Create the MapBox map
		// Be careful not to commit your MapBox API token here!
		let map = manager.createMap(
			"map", 
			"pk.eyJ1Ijoiam1hcnNkZW4iLCJhIjoiY2ttZzNqM3IxM2JyYzJ2bndzZnIxeG1lciJ9.uwb9ZnBO3vWssvcBsXcFeA",
			[0.15282, 52.32136],
			9
		);

		// Build the layer tree
		manager.buildControls("./data/layer-tree.json");
		
		// Import your custom modules
		import { CambridgeshirePointsModule } from "./js/cambridgeshire-points.js";
		import { CambridgeshireRegionModule } from "./js/cambridgeshire-regions.js";

		// Add these modules to the manager
		// Note that data will be added in the same order as the modules.
		manager.addModule(new CambridgeshireRegionModule());
		manager.addModule(new CambridgeshirePointsModule());

		// Load the time series data into memory whilst the rest of
		// the visualisation is loading
		DT.timeSeriesHandler = new DigitalTwinTimeSeries();
		DT.timeSeriesHandler.readFile("orchard-data", "./data/orchards-time-series.json")

		// Example side panel title
		DT.sidePanelHandler.setTitle("Default Title");

		// Example side panel content
		var defaultContent = `
			<div style="height: 100%; border: 1px solid grey;">
				<p>Here is some introductory text, this should be used to detail the overall purpose of the
				visualisation, and perhaps tell the user how they can interact with it.</p>
				<p>Lorem ipsum dolor sit amet, consectetur adipiscing elit. Proin ut magna sed velit dictum varius.
				Vestibulum magna orci, pulvinar eget quam ut, vestibulum pretium justo. Vivamus ut dignissim sem.</p>
				<p>Quisque ut cursus dolor. Fusce vel dui luctus dolor dapibus porttitor. Vestibulum pharetra tortor dolor,
				sit amet varius ex rhoncus convallis. Donec commodo bibendum ligula eget vulputate. Cras a velit suscipit,
				vulputate est sodales, vulputate dui.</p>
				<p>Lorem ipsum dolor sit amet, consectetur adipiscing elit. Proin ut magna sed velit dictum varius.
				Vestibulum magna orci, pulvinar eget quam ut, vestibulum pretium justo. Vivamus ut dignissim sem.</p>
				<p>Quisque ut cursus dolor. Fusce vel dui luctus dolor dapibus porttitor. Vestibulum pharetra tortor dolor,
				sit amet varius ex rhoncus convallis. Donec commodo bibendum ligula eget vulputate. Cras a velit suscipit,
				vulputate est sodales, vulputate dui.</p>
			</div>`;
		DT.sidePanelHandler.setContent(defaultContent);

		// Example legend content
		DT.sidePanelHandler.setLegend(`
			<div style="border: 1px solid grey;">
				<br><br>
				<p>This area is designed to hold a number of legends. Each layer (as shown to 
				the user in the layer tree) should have its own legend to explain the data
				on the map. This could be dynamically generated HTML, or an image file.</p>
				<br><br>
			</div>
		`);

		// Example footer content
		DT.sidePanelHandler.setFooter(`
			<div style="border: 1px solid grey;">
				Here is some footer content.
			</div>
		`);

		// Fire on MapBox style change.
		// Every time a MapBox changes style (i.e. Terrain), it will remove all data sources and layers.
		// This means that we have to listen for this event (below) and re-add sources and re-apply layers.
		var loadedOnce = false;

		map.on('style.load', function() {
			console.log("INFO: A new style has been loaded.");

			// Load all modules
			manager.loadModules();

			// Create a legend built from the now loaded modules.
			if(!loadedOnce) {
				DT.sidePanelHandler.buildLegend();
				
				var firstLegend = document.getElementsByClassName("w3-button tablink")[0];
				if(firstLegend != null) firstLegend.click();

				loadedOnce;
			}
		});
	</script>

</body>

</html>