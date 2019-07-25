let osmbGlobal;

$(function(){

    //*****************************************************//
    const toggleDisplay = elemId => {
        let x = document.getElementById(elemId);
        if (x.style.display !== 'block') {
            x.style.display = 'block';
        } else {
            x.style.display = 'none';
        }
    };

    $("#readme-button").click(function() {
        toggleDisplay("readme-text");
    });

    document.addEventListener("click", function(evt) {
        var readmeButtonElement = document.getElementById('readme-button'),
            readmeTextElement = document.getElementById('readme-text'),
            targetElement = evt.target;  // clicked element

        if (targetElement == readmeButtonElement || targetElement == readmeTextElement) {
            return; //readme-button or readme-text is clicked. do nothing.
        }

        if(readmeTextElement.style.display === 'block') {
            readmeTextElement.style.display = 'none';
        }
    });
    //*****************************************************//
    //proj4.defs("EPSG:28992","+proj=sterea +lat_0=52.15616055555555 +lon_0=5.38763888888889 +k=0.9999079 +x_0=155000 +y_0=463000 +ellps=bessel +towgs84=565.417,50.3319,465.552,-0.398957,0.343988,-1.8774,4.0725 +units=m +no_defs");
    proj4.defs("EPSG:3857","+proj=merc +a=6378137 +b=6378137 +lat_ts=0.0 +lon_0=0.0 +x_0=0.0 +y_0=0 +k=1.0 +units=m +nadgrids=@null +wktext  +no_defs");
    proj4.defs("EPSG:2326","+proj=tmerc +lat_0=22.31213333333334 +lon_0=114.1785555555556 +k=1 +x_0=836694.05 +y_0=819069.8 +ellps=intl +towgs84=-162.619,-276.959,-161.764,0.067753,-2.24365,-1.15883,-1.09425 +units=m +no_defs ");
    //***************************************************************************
	// default position of map is set at Singapore
	const position = {
		latitude: 1.262008,
        longitude: 103.850973
    };
    //***************************************************************************

	
    //***************************************************************************
    // initial map
	const osmb = new OSMBuildings({
        baseURL: './OSMBuildings',
        zoom: 14.5,
        minZoom: 1,
        maxZoom: 25,
        rotation: -45.6,
        tilt: 20.6,
        position: position,
        state: true, // stores map position/rotation in url
        effects: [], // effects: ['shadows']
        attribution: '� 3D <a href="https://osmbuildings.org/copyright/">OSM Buildings</a>'
    }).appendTo('map');
    //***************************************************************************
	osmbGlobal = osmb;
	
    //***************************************************************************
    // buttons to tilt camera angle, rotate the map, and zoom in/out
    controlButtonsSetter(osmb);
    //***************************************************************************

    
    //***************************************************************************
    osmb.addMapTiles(
        'https://{s}.tiles.mapbox.com/v3/osmbuildings.kbpalbpk/{z}/{x}/{y}.png',
        {
            attribution: '� Data <a href="https://openstreetmap.org/copyright/">OpenStreetMap</a> � � Map <a href="https://mapbox.com/">Mapbox</a>'
        }
    );
    
    // TBC: Building GeoJSON
    // osmb.addGeoJSONTiles('https://{s}.data.osmbuildings.org/0.2/anonymous/tile/{z}/{x}/{y}.json');
    //***************************************************************************

    
    //***************************************************************************
   
    
    //***************************************************************************

    osmb.on('pointerdown', function(e) {

        var id = osmb.getTarget(e.detail.x, e.detail.y, function(id) {

            var coordinates = osmb.unproject(e.detail.x, e.detail.y);
            
            var longitude = coordinates["longitude"];
            var latitude = coordinates["latitude"];

            const coordinatesArray = [];

            if (!isNaN(latitude) && !isNaN(longitude)) {
            	console.log(longitude, latitude);
            	//const convertedCoordinates = proj4('EPSG:2326', [parseFloat(longitude), parseFloat(latitude)]);
            	const convertedCoordinates = proj4('EPSG:3857', [parseFloat(longitude), parseFloat(latitude)]);

                coordinatesArray.push(convertedCoordinates[0]); // x
                coordinatesArray.push(convertedCoordinates[1]); // y

//                document.getElementById("longitude").innerHTML = longitude; // convertedCoordinates[1];
//                document.getElementById("latitude").innerHTML = latitude; // convertedCoordinates[0];

                console.log(coordinatesArray);
            }
        });
    });

    //***************************************************************************

    
    //***************************************************************************
    // converts point from EPSG:28992 to EPSG:4326
    // returns an array
    // longitude = array[0], latitude = array[1]
    const getOSMPoint = (x, y) => {
        return proj4('EPSG:3857', 'EPSG:4326', [x,y])
    };

    const getMidPoint = (coordinatesMin, coordinatesMax) => {
        const latitude = (coordinatesMax[1] + coordinatesMin[1])/2;
        const longitude = (coordinatesMax[0] + coordinatesMin[0])/2;
        return [latitude, longitude];
    };
    

    $('#start').click(function(){
    	//$('#start').attr("disabled", true);
    	
		console.log('button clicked')
		
		$('#inputFields').append('<img id="myProgressBar" style="width:100px;height:100px;" src="https://media.giphy.com/media/3oEjI6SIIHBdRxXI40/giphy.gif"/>'
);
		
        
        let xmax = parseInt($('#xupper').val());
        let xmin = parseInt($('#xlower').val());
        let ymax = parseInt($('#yupper').val());
        let ymin = parseInt($('#ylower').val());
        
        const reactionmechanism = $("#reaction-select option:selected").val();
  
        console.log(reactionmechanism);
        
//        approximate becasue texture only work on power2(has to be 1:1,1:2,1:4...)
//        [xmin, xmax, ymin, ymax] = appro2ratio(xmin, xmax, ymin, ymax);
        [xmin, xmax, ymin, ymax, ratio] = appro2ratio(xmin, xmax, ymin, ymax); // 28 Aug 18
        //   
        const lowerx = xmin;
        const lowery = ymin;
        const upperx = xmax;
        const uppery = ymax;

      	console.log('result')
        var canvas = $('#drawcanvas'); canvas.width(1024*ratio).height(1024); // 28 Aug 18
        var svg = $('contoursvg');svg.width(1024*ratio).height(1024); // 28 Aug 18

        const location = $("#location option:selected").text();
        
        const coordinatesMin = getOSMPoint(lowerx, lowery);
		const coordinatesMax = getOSMPoint(upperx, uppery);
        
		let locationIRI;
//		let plant;
		if (location === "Hong Kong") {
        	locationIRI = "http://dbpedia.org/resource/Hong_Kong";
//        	plant = "http://www.theworldavatar.com/kb/nld/thehague/powerplants/Plant-001.owl#Plant-001";
        } else if (location === "Singapore" || location === "Singapore_2") {
        	locationIRI = "http://dbpedia.org/resource/Singapore";
//        	plant = "http://www.theworldavatar.com/kb/deu/berlin/powerplants/Heizkraftwerk_Mitte.owl#Plant-002";
        }

        const coordinatesMid = getMidPoint(coordinatesMin, coordinatesMax);
        
        let agent = "http://www.theworldavatar.com/kb/agents/Service__ComposedADMS.owl#Service";
        
        var query = {
        	agent, 
			"region": {
				//"srsname": "EPSG:4326",
				"srsname": "EPSG:3857",
				"lowercorner": {
					lowerx,
					lowery
				},
				"uppercorner": {
					upperx,
					uppery
				}
			},
        	location,
        	reactionmechanism
        }
        console.log(query);
		query = JSON.stringify(query);        
		
//		$.ajax({
//			url: '/JPS_SHIP/UpdateShipCoordinates',
//			method: 'GET',
//		}).done(data => {
//			console.log(data);
//		})
		
        const getCoordinationResult = (query) => {
        	
        	var result = "hello";
        		
//        	if (document.getElementById("compose").checked) {
//        		result =  $.getJSON('/JPS_COMPOSITION/execute',
//        				{
//	        				query
//        				});
//        	} else {
        	
        	
        	if (document.getElementById("mock").checked) {
    		result =  $.getJSON('/JPS_SHIP/ADMSCoordinationAgentForShipWithoutCompositionWithMocks',
    				{
        				query
    				});
    	} else {
        		result =  $.getJSON('/JPS_SHIP/ADMSCoordinationAgentForShipWithoutComposition',
        				{
	        				query
        				});
        	}        	
  	
        	return result;
        };
        
        console.log(locationIRI);
        $.when(getCoordinationResult(query)).done(coordResult => {
            		
			var buildingIRIs = coordResult.building;
			var shipIRIs = coordResult.ship;
			var folder = coordResult.folder;
			console.log(coordResult);
			console.log("buildingIRIs = " + buildingIRIs);
			console.log("shipIRIs = " + shipIRIs);
			
			$("#myProgressBar").remove()
        	initadms3dmap(buildingIRIs, [xmin, xmax, ymin, ymax], osmb, location, coordinatesMid, locationIRI, shipIRIs, folder);
			
			
			
        });

    });
    //***************************************************************************

    
    //***************************************************************************
    // Sets position of camera at selected location
    $("#location").on("change", () => {
        const location = $("#location option:selected").text();

        if (location === "Singapore") {
        	document.getElementById("optmsg").innerHTML="";
        	osmb.setPosition({
                latitude: 1.262008,
                longitude: 103.850973
            });
//            $("#xlower").val("11558666.37");
//            $("#xupper").val("11562079.502");
//            $("#ylower").val("139186.423");
//            $("#yupper").val("141908.33");
        	
        	$("#xlower").val("11560879.832");
            $("#xupper").val("11563323.926");
            $("#ylower").val("140107.739");
            $("#yupper").val("143305.896");
            
            osmb.setZoom(14.5);
            osmb.setTilt(20.6);
            osmb.setRotation(-45.6);
        } else if (location === "Singapore_2") {  //singapore 2 is unused
        	osmb.setPosition({
                latitude: 1.262008,
                longitude: 103.850973
            });
            $("#xlower").val("11560879.832");
            $("#xupper").val("11563323.926");
            $("#ylower").val("140107.739");
            $("#yupper").val("143305.896");
            
            osmb.setZoom(14.5);
            osmb.setTilt(20.6);
            osmb.setRotation(-45.6);
        } else if (location === "Hong Kong") {
        	document.getElementById("optmsg").innerHTML="Buildings are projected down directly above the ground although elevation is considered in the calculations.";
            osmb.setPosition({
                longitude: 114.1491155592187,
                latitude: 22.28911086466781
            });
          $("#xlower").val("12693826.33");
          $("#xupper").val("12720831.57");
          $("#ylower").val("2535141.08");
          $("#yupper").val("2562311.02"); 
//          $("#xlower").val("12706630.262");
//          $("#xupper").val("12708200.45");
//          $("#ylower").val("2545539.172");
//          $("#yupper").val("2546850.028");
          
            
            osmb.setZoom(14.5);
            osmb.setTilt(14.5);
            osmb.setRotation(20.9);
        }
        
        
    })
    //***************************************************************************
});

//approximate to ratio 1:1 or 1:2^n // 28 Aug 18
function appro2ratio(xmin, xmax, ymin, ymax){
 
    let copy = [xmax-xmin, ymax-ymin];
    if (copy[0]- copy[1] >Number.EPSILON){
        larger = 0;
        smaller = 1;
    } else {
        larger = 1;
        smaller = 0;
    }
    
    let times = copy[larger]/copy[smaller];
    let lgtime = Math.log2(times);
    let t2 = Math.round(lgtime);
   // console.log(lgtime +' '+t2)
    if(t2 > lgtime) {//we are expanding to larger ratio everything is peachy
        times = Math.pow(2,t2)
        copy[larger] = copy[smaller] * times;
    } else {//we are truncating to smaller ratio, so to not desize, we add to smaller side
        times = Math.pow(2,t2);
        copy[smaller] = Math.round(copy[larger]/times)
    }
    //next is padding
    
    return [xmin, xmin+copy[0], ymin, ymin+copy[1], copy[0]/copy[1]];
}

// approximate to ratio 1:1 or 1:2
//function appro2ratio(xmin, xmax, ymin, ymax){
//    x = xmax - xmin;
//    y = ymax - ymin;
//    ratio = x/y;
//    if(ratio >= 2){
//        ymax = ymin + x/2;
//    } else if( ratio <2 && ratio > 1){
//        ymax = ymin + x;
//    } else if (ratio <=1 && ratio > 1/2){
//        xmax =xmin + y;
//    }else {
//        xmax = xmin + y /2;
//    }
//    
//    return [xmin, xmax, ymin, ymax];
//}

String.prototype.replaceAll = function(search, replacement) {
    var target = this;
    return target.replace(new RegExp(search, 'g'), replacement);
};