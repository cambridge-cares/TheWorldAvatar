$(function(){

    proj4.defs("EPSG:28992","+proj=sterea +lat_0=52.15616055555555 +lon_0=5.38763888888889 +k=0.9999079 +x_0=155000 +y_0=463000 +ellps=bessel +towgs84=565.417,50.3319,465.552,-0.398957,0.343988,-1.8774,4.0725 +units=m +no_defs");
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
        'https://{s}.tiles.mapbox.com/v3/osmbuildings.kbpalbpk/{z}/{x}/{y}.png',
        {
            attribution: '� Data <a href="https://openstreetmap.org/copyright/">OpenStreetMap</a> � � Map <a href="https://mapbox.com/">Mapbox</a>'
        }
    );
    //***************************************************************************

    
    //***************************************************************************

//    const twoDimCheckbox = document.querySelector('input[value="2d"]');
//    let decreaseTiltTimer;
//    let increaseTiltTimer;
//
//    const decreaseTilt = () => {
//        let tilt = osmb.getTilt();
//        tilt--;
//        if (tilt != 0) {
//            osmb.setTilt(tilt);
//        }
//    };
//
//    const increaseTilt = () => {
//        let tilt = osmb.getTilt();
//        tilt++;
//        if (tilt < 45) {
//            osmb.setTilt(tilt);
//        }
//    };
//
//    osmb.on('pointerdown', function(e) {
//        // related to the tilt timers
//        clearInterval(decreaseTiltTimer);
//        clearInterval(increaseTiltTimer);
//
//        const output = document.getElementById("output");
//
//        if (output.style.display !== "none") {
//
//            var id = osmb.getTarget(e.detail.x, e.detail.y, function(id) {
//
//                var coordinates = osmb.unproject(e.detail.x, e.detail.y);
//
//                var longitude = coordinates["longitude"];
//                var latitude = coordinates["latitude"];
//
//                const coordinatesArray = [];
//
//                if (!isNaN(latitude) && !isNaN(longitude)) {
//
//                    const convertedCoordinates = proj4('EPSG:28992', [parseFloat(longitude), parseFloat(latitude)]);
//
//                    coordinatesArray.push(convertedCoordinates[0]); // latitude
//                    coordinatesArray.push(convertedCoordinates[1]); // longitude
//
//                    document.getElementById("longitude").innerHTML = longitude; // convertedCoordinates[1];
//                    document.getElementById("latitude").innerHTML = latitude; // convertedCoordinates[0];
//
//                    // $.getJSON('/JPS/ADMSOutput',
//                    //     {
//                    //         coordinatesLonLat: JSON.stringify(coordinatesArray)
//                    //     },
//                    //     function(data) {
//                    //         var concentrations = data;
//                    //
//                    //         document.getElementById("concentration0").innerHTML = concentrations[2];
//                    //         document.getElementById("concentration10").innerHTML = concentrations[3];
//                    //         document.getElementById("concentration20").innerHTML = concentrations[4];
//                    //         document.getElementById("concentration30").innerHTML = concentrations[5];
//                    //
//                    //     });
//                }
//            });
//        }
//    });

//    twoDimCheckbox.onchange = function(){
//    	if(twoDimCheckbox.checked) {
//            clearInterval(increaseTiltTimer);
//            decreaseTiltTimer = setInterval(decreaseTilt, 15);
//            decreaseTiltTimer;
//
//            const output = document.getElementById("output");
//
//            output.style.display = "inline";
//        } else {
//            clearInterval(decreaseTiltTimer);
//            increaseTiltTimer = setInterval(increaseTilt, 15);
//            increaseTiltTimer;
//
//            const output = document.getElementById("output");
//
//            if (output.style.display !== "none") {
//                output.style.display = "none";
//
//                document.getElementById("longitude").innerHTML = "";
//                document.getElementById("latitude").innerHTML = "";
//                document.getElementById("concentration0").innerHTML = "";
//                document.getElementById("concentration10").innerHTML = "";
//                document.getElementById("concentration20").innerHTML = "";
//                document.getElementById("concentration30").innerHTML = "";
//            }
//        }
//    };
    //***************************************************************************

    
    //***************************************************************************
    $("#reaction-select").on('change',function () {
        //show a map of reactions
        console.log("select changed");
        $.ajax('/JSON/chemsrm.json').done(function (reactionlist) {
            //todo: init building
            console.log(reactionlist.length);
            var parent  = $('#reaction-list');
            console.log(parent);
            for(let reaction of reactionlist){
                parent.append('<li>'+reaction+'</li>')
            }
        }).fail(function () {
            console.log("error")
        })
    })
    //***************************************************************************

    
    //***************************************************************************
    // converts point from EPSG:28992 to EPSG:4326
    // returns an array
    // longitude = array[0], latitude = array[1]
    const getOSMPoint = (x, y) => {
        return proj4('EPSG:28992', 'EPSG:4326', [x,y])
    };

    const getMidPoint = (coordinatesMin, coordinatesMax) => {
        const latitude = (coordinatesMax[1] + coordinatesMin[1])/2;
        const longitude = (coordinatesMax[0] + coordinatesMin[0])/2;
        return [latitude, longitude];
    };

    $('#start').click(function(){
        //$('#start').attr("disabled", true);
        
        let xmax = parseInt($('#xupper').val());
        let xmin = parseInt($('#xlower').val());
        let ymax = parseInt($('#yupper').val());
        let ymin = parseInt($('#ylower').val());

//        approximate becasue texture only work on power2(has to be 1:1,1:2,1:4...)
        [xmin, xmax, ymin, ymax] = appro2ratio(xmin, xmax, ymin, ymax);

        const coordinatesMin = getOSMPoint(xmin, ymin);
        const coordinatesMax = getOSMPoint(xmax-5000,ymax-5000);
        const coordinatesMid = getMidPoint(coordinatesMin, coordinatesMax);

        const location = $("#location option:selected").text();
         
        $.ajax('http://www.theworldavatar.com/JPS/ADMSCoordinationAgent?coordinates='+encodeURIComponent(JSON.stringify({'xmin':xmin,'xmax':xmax, 'ymin':ymin, 'ymax':ymax}).replaceAll('"',"'"))).done(function (bdnlist) {
            //todo: init building
            initadms3dmap(JSON.parse(bdnlist), [xmin, xmax, ymin, ymax], osmb, location, coordinatesMid);
            
        }).fail(function (xhr, testStatus, errorThrown) {
//            console.log("error")
        	console.log(xhr.responseText);
        })
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
            $("#xlower").val("");
            $("#xupper").val("");
            $("#ylower").val("");
            $("#yupper").val("");
        } else if(location === "The Hague"){
            osmb.setPosition({
                latitude: 52.076146,
                longitude: 4.309961
            });
            $("#xlower").val("79480");
            $("#xupper").val("85000");
            $("#ylower").val("454670");
            $("#yupper").val("460000");
        }
        osmb.setZoom(10);
        osmb.setTilt(0);
    })
    //***************************************************************************
});

// approximate to ratio 1:1 or 1:2
function appro2ratio(xmin, xmax, ymin, ymax){
    x = xmax - xmin;
    y = ymax - ymin;
    ratio = x/y;
    if(ratio >= 2){
        ymax = ymin + x/2;
    } else if( ratio <2 && ratio > 1){
        ymax = ymin + x;
    } else if (ratio <=1 && ratio > 1/2){
        xmax =xmin + y;
    }else {
        xmax = xmin + y /2;
    }
    
    return [xmin, xmax, ymin, ymax];
}

String.prototype.replaceAll = function(search, replacement) {
    var target = this;
    return target.replace(new RegExp(search, 'g'), replacement);
};