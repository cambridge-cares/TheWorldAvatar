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
    //***************************************************************************
	// default position of map is set at The Hague
	const position = {
	    latitude: 52.076146,
        longitude: 4.309961
    };
    //***************************************************************************

	
    //***************************************************************************
    // initial map
	const osmb = new OSMBuildings({
        baseURL: './OSMBuildings',
        zoom: 10,
        minZoom: 10,
        maxZoom: 25,
        rotation: 0.6,
        tilt: 0,
        position: position,
        state: true, // stores map position/rotation in url
        effects: [], // effects: ['shadows']
        attribution: '� 3D <a href="https://osmbuildings.org/copyright/">OSM Buildings</a>'
    }).appendTo('map');
    //***************************************************************************

	
    //***************************************************************************
    // buttons to tilt camera angle, rotate the map, and zoom in/out
    controlButtonsSetter(osmb);
    //***************************************************************************

    
    //***************************************************************************
    osmb.addMapTiles(
           'https://a.tile.openstreetmap.org/{z}/{x}/{y}.png'
        );
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
            	console.log(latitude, longitude);
                //const convertedCoordinates = proj4('EPSG:28992', [parseFloat(longitude), parseFloat(latitude)]);
            	const convertedCoordinates = proj4('EPSG:3857', [parseFloat(longitude), parseFloat(latitude)]);
            	
                coordinatesArray.push(convertedCoordinates[0]); // latitude
                coordinatesArray.push(convertedCoordinates[1]); // longitude

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
        //return proj4('EPSG:28992', 'EPSG:4326', [x,y])
        return proj4('EPSG:3857', 'EPSG:4326', [x,y])
        
    };

    const getMidPoint = (coordinatesMin, coordinatesMax) => {
        const latitude = (coordinatesMax[1] + coordinatesMin[1])/2;
        const longitude = (coordinatesMax[0] + coordinatesMin[0])/2;
        return [latitude, longitude];
    };
    

    $('#start').click(function(){
    	//$('#start').attr("disabled", true);
    	$('#inputFields').append('<img id="myProgressBar" style="width:100px;height:100px;" src="https://media.giphy.com/media/3oEjI6SIIHBdRxXI40/giphy.gif"/>'
);
        
        let xmax = parseInt($('#xupper').val());
        let xmin = parseInt($('#xlower').val());
        let ymax = parseInt($('#yupper').val());
        let ymin = parseInt($('#ylower').val());
        
        const reactionmechanism = $("#reaction-select option:selected").val();
  
        console.log(reactionmechanism);
        
        const lowerx = xmin;
        const lowery = ymin;
        const upperx = xmax;
        const uppery = ymax;

//        approximate becasue texture only work on power2(has to be 1:1,1:2,1:4...)
//        [xmin, xmax, ymin, ymax] = appro2ratio(xmin, xmax, ymin, ymax);
        [xmin, xmax, ymin, ymax, ratio] = appro2ratio(xmin, xmax, ymin, ymax); // 28 Aug 18
       
        var canvas = $('#drawcanvas'); canvas.width(1024*ratio).height(1024); // 28 Aug 18
        var svg = $('contoursvg');svg.width(1024*ratio).height(1024); // 28 Aug 18

        const location = $("#location option:selected").text();
        
        const coordinatesMin = getOSMPoint(lowerx, lowery);
		const coordinatesMax = getOSMPoint(upperx, uppery);
        
		let locationIRI;
		let plant;
		if (location === "The Hague") {
        	locationIRI = "http://dbpedia.org/resource/The_Hague";
        	plant = "http://www.theworldavatar.com/kb/nld/thehague/powerplants/Plant-001.owl#Plant-001";
        } else if (location === "Berlin") {
        	locationIRI = "http://dbpedia.org/resource/Berlin";
        	plant = "http://www.theworldavatar.com/kb/deu/berlin/powerplants/Heizkraftwerk_Mitte.owl#Plant-002";
        }

        const coordinatesMid = getMidPoint(coordinatesMin, coordinatesMax);
        
        //let agent = "http://www.theworldavatar.com/kb/agents/Service__ComposedADMS.owl#Service";
        let agent = "http://www.theworldavatar.com/kb/agents/Service__ADMS.owl#Service"; //temporary change to be consistent
        
        var query = {
        	agent, 
			"region": {
				//"srsname": "EPSG:4326",
				"srsname": "EPSG:3857",
				//"srsname": "EPSG:28992",
				"lowercorner": {
					lowerx/*:xmin*/,
					lowery/*:ymin*/
				},
				"uppercorner": {
					upperx/*:xmax*/,
					uppery/*:ymax*/
				}
			},
        	plant,
        	reactionmechanism
        }
        
		query = JSON.stringify(query);        
     
        const getCoordinationResult = (query) => {
        	
        	var result = "hello";
        		
//        	if (document.getElementById("compose").checked) {
//        		result =  $.getJSON('/JPS_COMPOSITION/execute',
//        				{
//	        				query
//        				});
//        	} else {
        	
        	
        	if (document.getElementById("mock").checked) {
        		result =  $.getJSON('/JPS/ADMSCoordinationAgentWithoutCompositionWithMocks',
    				{
        				query
    				});
    	   	} else {
        		result =  $.getJSON('/JPS/ADMSCoordinationAgentWithoutComposition',
//    	   		result =  $.getJSON('/JPS_DISPERSION/DMSCoordinationAgent',
        			{
	        			query
        			});
        	}        	
  	
        	return result;
        };
        
        console.log(locationIRI);
        $.when(getCoordinationResult(query)).done(coordResult => {
            		
			var buildingIRIs = coordResult.building;
			console.log("buildingIRIs = " + buildingIRIs);
			$("#myProgressBar").remove()
        	initadms3dmap(buildingIRIs, [xmin, xmax, ymin, ymax], osmb, location, coordinatesMid, locationIRI);
        });

    });
    //***************************************************************************

    
    //***************************************************************************
    // Sets position of camera at selected location
    $("#location").on("change", () => {
        const location = $("#location option:selected").text();

        if(location === "Berlin"){
            osmb.setPosition({
                latitude: 52.512997,
                longitude: 13.385423
            });
//            $("#xlower").val("699182");
//            $("#xupper").val("699983");
//            $("#ylower").val("532537");
//            $("#yupper").val("533338");
            $("#xlower").val("1493262.39");
            $("#xupper").val("1494710.67");
            $("#ylower").val("6892594.98");
            $("#yupper").val("6894044.12");
        } else if(location === "The Hague"){
            osmb.setPosition({
                latitude: 52.076146,
                longitude: 4.309961
            });
//            $("#xlower").val("79173");
//            $("#xupper").val("80199");
//            $("#ylower").val("454193");
//            $("#yupper").val("455030");
            $("#xlower").val("476584.89");
            $("#xupper").val("478230.04");
            $("#ylower").val("6812941.68");
            $("#yupper").val("6814587.35");
        }
        osmb.setZoom(10);
        osmb.setTilt(0);
    })
    //***************************************************************************
});

//approximate to ratio 1:1 or 1:2^n // 28 Aug 18
function appro2ratio(xmin, xmax, ymin, ymax){
 
    let copy = [xmax-xmin, ymax-ymin];
    if( copy[0]- copy[1] >Number.EPSILON){
        larger =0;
        smaller = 1;
    } else{
        larger = 1;
        smaller = 0;
    }
    
    let times = copy[larger]/copy[smaller];
    let lgtime = Math.log2(times);
    let t2 = Math.round(lgtime);
   // console.log(lgtime +' '+t2)
    if(t2 > lgtime){//we are expanding to larger ratio everything is peachy
        times = Math.pow(2,t2)
        copy[larger] = copy[smaller] * times;
    } else{//we are truncating to smaller ratio, so to not desize, we add to smaller side
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