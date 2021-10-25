<!DOCTYPE html>
<html>

<head>
	<title>Digital Twin Example</title>
	<meta charset="utf-8">

	<!-- External CSS -->
	<link href='https://api.tiles.mapbox.com/mapbox-gl-js/v2.3.0/mapbox-gl.css' rel='stylesheet' />
	<link href="https://www.w3schools.com/w3css/4/w3.css" rel="stylesheet">

	<!-- External JS -->
	<script src='https://api.mapbox.com/mapbox-gl-js/v2.3.0/mapbox-gl.js'></script>
	<script src='https://ajax.googleapis.com/ajax/libs/jquery/3.5.1/jquery.min.js'></script>

	<!--
	<script src="https://cdn.jsdelivr.net/npm/moment@2.29.1"></script>
	<script src="https://cdnjs.cloudflare.com/ajax/libs/Chart.js/3.5.1/chart.js"></script>
	<script src="https://cdn.jsdelivr.net/npm/chartjs-adapter-moment@1.0.0"></script>
	-->

	<!-- Local CSS -->
	<link href='./css/digital_twin_style.css' rel='stylesheet' />
	<link href="./css/style.css" rel="stylesheet"> 
	<link href="./css/jsonview.bundle.css" rel="stylesheet"> 
	<link href="https://use.fontawesome.com/releases/v5.2.0/css/all.css" rel="stylesheet"  integrity="sha384-hWVjflwFxL6sNzntih27bfxkr27PmbbK/iSvJ+a4+0owXq79v+lsFkW54bOGbiDQ" crossorigin="anonymous">

	<!-- Local JS -->
	<script src='./js/manager.js'></script>
	<script src='./js/data_registry.js'></script>
	<script src='./js/data_handler.js'></script>
	<script src='./js/layer_handler.js'></script>
	<script src='./js/control_handler.js'></script>
	<script src='./js/panel_handler.js'></script>
	<script src='./js/interaction_handler.js'></script>
	<script src='./js/jsonview.bundle.js'></script>

	<!--<script src='./js/digital_twin_time_series.js'></script>-->
</head>

<body>
	<!-- Element the MapBox map will be added to -->
	<div id='map'></div>
	
	<!-- Element the map controls will be added to -->
	<div id="controlsContainer"></div>

	<!-- Non-module JS block, variables in here are global-->
	<script>
		let manager = null;
	</script>

	<!-- Module JS block, variables are local (i.e. cannot access from HTML elements) -->
	<script type="module">

		// Initialise a DigitalTwinManager (and store as a global variable)
		manager = new DigitalTwinManager();

		// Load all the metadata
		manager.readMetadata("./data/overall-meta.json", function() {
			// Run start-up function only AFTER metadata is loaded
			startUp();
		});

		// Will run once metadata is loaded asynchronously
		function startUp() {
			// Create the MapBox map
			let map = manager.createMap("map");

			// Provide some default content for the side panel
			manager.setPanelContent(
				"Example Visualisation",
				`
					This section of default text should be used to give a brief introduction on the purpose of the
					visualisation, and to detail how the user can interact with it. For the purpose of this example,
					some junk content to pad it out has been added.
					<br/><br/>
					Lorem ipsum dolor sit amet, consectetur adipiscing elit. Integer mi turpis, posuere a nibh in, tincidunt
					mollis eros. Cras imperdiet ornare enim, et luctus augue iaculis at. Ut tellus ex, auctor non sagittis quis,
					feugiat vitae turpis. Cras efficitur, arcu sed condimentum mattis, quam est tempus lectus, eu faucibus.
				`,
				`	
					<b>Example Legend:</b>
					<br/>
					<div style="display: flex; justify-content: center;">
						<img src="./img/sample-legend.png" width="350px"/>
					</div>
				`,
				"Here is an example footer."
			)

			// Every time a MapBox changes style (i.e. Terrain), it will remove all data sources and layers.
			// This means that we have to listen for this event (below) and re-add sources and re-apply layers.
			var loadedOnce = false;

			map.on('style.load', function() {
				console.log("INFO: A new style has been loaded.");

				// Plot the Fixed Data locations
				manager.plotFixedData();

				// Re-plot any previously plotted Additional Data sources
				manager.restoreAllAdditionalData();

				// On first load only
				if(!loadedOnce) {
					// This callback will be fired when a selection in the layer tree changes
					let treeCallback = null;
					
					// This callback will be fired when a final group is selected in the dropdowns
					let selectCallback = function(selectedGroups) {
						manager.removeAllAdditionalData();
						manager.plotAdditionalData(selectedGroups);
					};

					manager.showControls("./data/layer-tree.json", treeCallback, selectCallback);
					loadedOnce = true;
				}
			});
		}
	</script>

</body>

</html>