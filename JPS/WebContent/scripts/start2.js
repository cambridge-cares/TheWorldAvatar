 
    
var globalResult = {};





let testObj = getParameter();
    sendRequest(testObj, headers, function(result){

    // TODO: For testing, we predefine variables
    globalResult = result;
    
    const BERLIN_IRI = "http://dbpedia.org/page/Berlin";
    const THE_HAGUE_IRI = "http://dbpedia.org/page/The_Hague";

    let xmin = globalResult['Region']['xmin'];
    let xmax = globalResult['Region']['xmax'];
    let ymin = globalResult['Region']['ymin'];
    let ymax = globalResult['Region']['ymax'];
    /*
     *      let region = result['Region'];
			let city = result['City'];
			let buildingIRIs = result['BuildingList'];
     */
    const lowerx = xmin;
    const lowery = ymin;
    const upperx = xmax;
    const uppery = ymax;

//    approximate becasue texture only work on power2(has to be 1:1,1:2,1:4...)
    [xmin, xmax, ymin, ymax] = appro2ratio(xmin, xmax, ymin, ymax);


    let locationIRI =  globalResult['City'].replace('resource','page');
     
    
    const coordinatesMin = getOSMPoint(lowerx, lowery);
    let coordinatesMax;

    let location = "Berlin";
    if (locationIRI === THE_HAGUE_IRI) {
        location = 'The Hague';
        coordinatesMax = getOSMPoint(upperx-5000, uppery-5000);
    } else if (locationIRI === BERLIN_IRI) {
    	location = "Berlin";
        coordinatesMax = getOSMPoint(upperx, uppery)
    }
    
    const coordinatesMid = getMidPoint(coordinatesMin, coordinatesMax);
    let buildingIRIs = globalResult['BuildingList'];

   initadms3dmap(buildingIRIs, [xmin, xmax, ymin, ymax], osmb, location, coordinatesMid, locationIRI);

 
   
    const toggleDisplay = elemId => {
        let x = document.getElementById(elemId);
        if (x.style.display !== 'block') {
            x.style.display = 'block';
        } else {
            x.style.display = 'none';
        }
    };

    $("#readme-button").click(function () {
        toggleDisplay("readme-text");
    });

    document.addEventListener("click", function (evt) {
        var readmeButtonElement = document.getElementById('readme-button'),
            readmeTextElement = document.getElementById('readme-text'),
            targetElement = evt.target;  // clicked element

        if (targetElement == readmeButtonElement || targetElement == readmeTextElement) {
            return; //readme-button or readme-text is clicked. do nothing.
        }

        if (readmeTextElement.style.display === 'block') {
            readmeTextElement.style.display = 'none';
        }
    });
    //*****************************************************//

    proj4.defs("EPSG:28992", "+proj=sterea +lat_0=52.15616055555555 +lon_0=5.38763888888889 +k=0.9999079 +x_0=155000 +y_0=463000 +ellps=bessel +towgs84=565.417,50.3319,465.552,-0.398957,0.343988,-1.8774,4.0725 +units=m +no_defs");
    //***************************************************************************
    // default position of map is set at The Hague

    // TODO: Get the position from Parameter Input

    //***************************************************************************


    //***************************************************************************
    // initial map

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
    $("#reaction-select").on('change', function () {
        //show a map of reactions
        console.log("select changed");
        $.ajax('/JSON/chemsrm.json').done(function (reactionlist) {
            //todo: init building
            console.log(reactionlist.length);
            var parent = $('#reaction-list');
            console.log(parent);
            for (let reaction of reactionlist) {
                parent.append('<li>' + reaction + '</li>')
            }
        }).fail(function () {
            console.log("error")
        })
    });
    //***************************************************************************

    osmb.on('pointerdown', function (e) {

        var id = osmb.getTarget(e.detail.x, e.detail.y, function (id) {

            var coordinates = osmb.unproject(e.detail.x, e.detail.y);

            var longitude = coordinates["longitude"];
            var latitude = coordinates["latitude"];

            const coordinatesArray = [];

            if (!isNaN(latitude) && !isNaN(longitude)) {
                console.log(latitude, longitude);
                const convertedCoordinates = proj4('EPSG:28992', [parseFloat(longitude), parseFloat(latitude)]);

                coordinatesArray.push(convertedCoordinates[0]); // latitude
                coordinatesArray.push(convertedCoordinates[1]); // longitude
 
                console.log(coordinatesArray);
            }
        });
    });


});
// approximate to ratio 1:1 or 1:2
function appro2ratio(xmin, xmax, ymin, ymax) {
    x = xmax - xmin;
    y = ymax - ymin;
    ratio = x / y;
    if (ratio >= 2) {
        ymax = ymin + x / 2;
    } else if (ratio < 2 && ratio > 1) {
        ymax = ymin + x;
    } else if (ratio <= 1 && ratio > 1 / 2) {
        xmax = xmin + y;
    } else {
        xmax = xmin + y / 2;
    }

    return [xmin, xmax, ymin, ymax];
}

String.prototype.replaceAll = function (search, replacement) {
    var target = this;
    return target.replace(new RegExp(search, 'g'), replacement);
};


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

    osmb.on('pointerdown', function(e) {

        var id = osmb.getTarget(e.detail.x, e.detail.y, function(id) {

            var coordinates = osmb.unproject(e.detail.x, e.detail.y);

            var longitude = coordinates["longitude"];
            var latitude = coordinates["latitude"];

            const coordinatesArray = [];

            if (!isNaN(latitude) && !isNaN(longitude)) {
                console.log(latitude, longitude);
                const convertedCoordinates = proj4('EPSG:28992', [parseFloat(longitude), parseFloat(latitude)]);

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
            $("#xlower").val("699182");
            $("#xupper").val("699983");
            $("#ylower").val("532537");
            $("#yupper").val("533338");
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


function getParameter() {
    var url_string = window.location.href;
    var url = new URL(url_string.replace(/#/g, '@'));
    var value = url.searchParams.get("value").split("lat=")[0];
    return value.replace(/@/g,'#')
}

function generateSemanticRegion(Region){
    let RegionObj = JSON.parse(Region);
    return regionTemplate.format([RegionObj['lowerY'], RegionObj['lowerX'], RegionObj['upperY'], RegionObj['upperX']]);
}


function sendRequest(model, header, callback) {
    var myUrl = 'http://localhost:8080/JPS_COMPOSITION/LocalQueryEndPoint?rdfModel=' + encodeURIComponent(model) + '&headers=' + encodeURIComponent(JSON.stringify(header));
    var request = $.ajax({
        url: myUrl,
        type: 'GET',
        contentType: 'application/json; charset=utf-8'
    });

    request.done(function (data) {
        let dataInJSON = JSON.parse(data);
        let BuildingList = JSON.parse(dataInJSON['BuildingList']);
        let ADMS = dataInJSON['ADMS'];
        let City = dataInJSON['City'];
        let Region = dataInJSON['Region'];
        convertCoordinates(generateSemanticRegion(Region), function(newRegion) {
            let result = {};
            result['Region'] = newRegion;
            result['City'] = City;
            result['BuildingList'] = BuildingList;
            return callback(result);
        });
    });

    request.fail(function (jqXHR, textStatus) {
        // your failure code here
    });
}


function convertCoordinates(coor, callback){
    var myUrl = 'http://localhost:8080/JPS/CoordianteRefConvertor?value=' + encodeURIComponent(coor);
    var request = $.ajax({
        url: myUrl,
        type: 'GET',
        contentType: 'application/json; charset=utf-8'
    });

    request.done(function (data) {
        callback(JSON.parse(data));
    });

    request.fail(function (error) {

    })
}


String.prototype.format = function (args) {
    var str = this;
    return str.replace(String.prototype.format.regex, function(item) {
        var intVal = parseInt(item.substring(1, item.length - 1));
        var replace;
        if (intVal >= 0) {
            replace = args[intVal];
        } else if (intVal === -1) {
            replace = "{";
        } else if (intVal === -2) {
            replace = "}";
        } else {
            replace = "";
        }
        return replace;
    });
};
String.prototype.format.regex = new RegExp("{-?[0-9]+}", "g");

const getOSMPoint = (x, y) => {
    return proj4('EPSG:28992', 'EPSG:4326', [x,y])
};

const getMidPoint = (coordinatesMin, coordinatesMax) => {
    const latitude = (coordinatesMax[1] + coordinatesMin[1])/2;
    const longitude = (coordinatesMax[0] + coordinatesMin[0])/2;
    return [latitude, longitude];
};

