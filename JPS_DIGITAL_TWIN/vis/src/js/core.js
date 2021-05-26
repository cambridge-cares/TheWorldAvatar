/*
*	Main script for the Digital Twin visualisation.
*
*	Scripts handling layer-specific functionality are
*	defined in custom modules and loaded in from here.
*/

// Module imports
import * as utility from "./modules/utils.js"

// MapBox API key
const APIKEY = "pk.eyJ1IjoiY21jbGlubm92YXRpb25zIiwiYSI6ImNrbGdqa3RoNDFnanIyem1nZXR3YzVhcmwifQ.hVk983r6YYlmFE8kSMbzhA";
 
// Cached MapBox map object
var map = null;

// Cached MapBox popup
var popup = null;




/**
*	Initialises and configures the MapBox map element.
*/
function initialiseMap() {
	const query = window.location.search;
	const params = new URLSearchParams(query);
	
	// Check if the zoom level was specified in the URL
	var zoom = 10;
	if(params.has("zoom")) {
		try {
			zoom = parseInt(params.get("zoom"));
			console.log("%cFound valid 'zoom' parameter in URL, using that.", "color: green;");
		} catch(error) {
			console.log("%cInvalid 'zoom' parameter in URL, using default.", "color: orange");
		}
	}	
	
	// Check if the map center was specified in the URL
	var lng = "103.80977999427901";
	var lat = "1.3533492751332865";
		
	if(params.has("lng") && params.has("lat")) {
		try {
			lng = parseFloat(params.get("lng"));
			lat = parseFloat(params.get("lat"));
			console.log("%cFound valid 'lng' and 'lat' parameters in URL, using those.", "color: green;");
		} catch(error) {
			console.log("%cInvalid 'lng' and/or 'lat' parameters in URL, using defaults.", "color: orange");
		}
	}	
	
	// Initialise the map
	mapboxgl.accessToken = APIKEY;
	
	map = new mapboxgl.Map({
		container: 'mapContainer',
		style: 'mapbox://styles/mapbox/light-v10',
		zoom: zoom,
		center: [lng, lat]
	});

	console.log("%cInitialised map", "color: green;");
}

// Run logic on page load
window.addEventListener("load", initialiseMap);