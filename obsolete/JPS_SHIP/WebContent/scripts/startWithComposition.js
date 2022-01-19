let osmbGlobal;
let originRatio = 1;
metaEndpoint = "http://www.theworldavatar.com/rdf4j-server/repositories/airqualitystation";
//metaEndpoint = "http://localhost:8080/rdf4j-server/repositories/airqualitystation";
let sensorIRIs;

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
    //TODO: add: init attribute table****************************************************
    $( document ).ready(function ()
        {
        let tableTop = $('#map').position().top + $('#map').height() - $('#sensorTable').height();
    console.log($('#map').height);
    $('#sensorTable').css("top", tableTop);
    $('#sensorTable').css("display", "block");
    });
    //****TODO: add render table function*************************************************************
   function renderAttributeTable(attrs){
       let tableDiv = $("#sensorTable").empty();
       let tableStr= "<table class='table'><tr>";
       for (let tab of attrs.names){
           tableStr+="<th>"+tab+"</th>";
       }
       tableStr+="</tr>";
       for(let row of attrs.data){
           tableStr+="<tr>"
           for(let col of row){
               tableStr+="<td>"+col+"</td>"
           }
           tableStr+="</tr>"
       };
       tableStr+="</table>"
       tableDiv.append(tableStr);
   }
   /*****TODO: function: querySensor***/
   function renderSensorStations( sensorLocs) {
       let markers = [];
       //TODO: mock data
       for (let sIRI of sensorLocs){
           console.log("render sensor")
           console.log(sIRI)
           //TODO: after query the position
           let obj = osmbGlobal.addOBJ('/images/tinker.obj', {longitude: sIRI[1],latitude: sIRI[2] },{id: "marker_"+sIRI[0], scale : 1, elevation :70, rotation : 120 , color: 'red'});
           markers.push(obj);
       }
   }

    function querySensor(sensorIRIs, callback){
        let qstr = `
    PREFIX s:<http://www.theworldavatar.com/ontology/ontocape/chemical_process_system/CPS_realization/process_control_equipment/measuring_instrument.owl#>
    PREFIX t:<http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/space_and_time/space_and_time_extended.owl#>
   PREFIX sys:<http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#>
    SELECT Distinct ?graph ?x ?y 
    {graph ?graph {
        ?s t:hasGISCoordinateSystem ?gs.
         ?gs t:hasProjectedCoordinate_y ?cy.
         ?cy sys:hasValue ?yv.
         ?yv sys:numericalValue ?y.
         ?gs t:hasProjectedCoordinate_x ?cx.
         ?cx sys:hasValue ?xv.
         ?xv sys:numericalValue ?x.
    }
    }
    `;

        $.get({
            url:metaEndpoint,
            'Content-Type':"application/json",
            data: { query: qstr,format:'json'}
        })
            .done(function( msg ) {
                console.log( "query result: " );
                let result =queryProcessor(msg).data
                let searched = []
                console.log(result)
                console.log(sensorIRIs)
                for (let item of result){
                    console.log(item[0])
                    if(sensorIRIs.includes(item[0])){
                        searched.push(item)
                        console.log('found location for virtual sensor: '+item[0])
                    }
                }
                callback(null, result)
            });
    }

    function querySensorAttributes(stationIRI, callback) {
       let qstrT = `PREFIX j2:<http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#>
 PREFIX j4:<http://www.theworldavatar.com/ontology/ontosensor/OntoSensor.owl#>
 PREFIX j5:<http://www.theworldavatar.com/ontology/ontocape/chemical_process_system/CPS_realization/process_control_equipment/measuring_instrument.owl#>
 PREFIX j6:<http://www.w3.org/2006/time#>
 PREFIX j7:<http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/space_and_time/space_and_time_extended.owl#>
 SELECT Distinct ?prop ?propval  ?proptimeval ?allpsi ?mean ?max ?min ?individualpsi ?unit
 {graph stationIRI
 {
  ?graph j4:hasOverallPSI ?allpsi .
 ?prop   j2:hasValue ?vprop .
    ?prop j4:hasMeasuredPropertyMean ?mean .
    ?prop j4:hasMeasuredPropertyMax ?max .
    ?prop j4:hasMeasuredPropertyMin ?min .
    ?prop j4:hasPSI ?individualpsi .
?vprop   j4:prescaledNumValue ?propval .
    ?vprop   j2:hasUnitOfMeasure ?unit .
  ?vprop   j6:hasTime ?proptime .
  ?proptime   j6:inXSDDateTime ?proptimeval .
}}
 ORDER BY DESC(?proptimeval) LIMIT10`;
       // stationIRI = "http://www.theworldavatar.com/kb/sgp/singapore/AirQualityStation-001.owl#AirQualityStation-001"
   let qstr = qstrT.replace('stationIRI', '<'+stationIRI+'>');
            console.log(qstr);
        $.get({
            url:metaEndpoint,
            'Content-Type':"application/json",
            data: { query: qstr,format:'json'}
        })
            .done(function( strresult ) {
                console.log( "query sensor station result: " );
                console.log(strresult);
                console.log(typeof  strresult);
                let processed = queryProcessor(strresult);
                callback(null, processed);
            })
            .fail(function(err) {
                console.log( "query sensor attributes failed bc: ");
                console.log(err);
            });
    }
    function queryProcessor(str){
       let lines = str.split('\n');
       let results = [];
        let names = lines[0].split(',');
        for (let i =1; i< lines.length-1;i++){//remove last one which should be empty
            let vs = lines[i].split(',')
            results.push(vs)
        }
        return {data:results, names:names};
    }

    //***************************************************************************
	osmbGlobal = osmb;

//console.log('test query sensor attributes:')
 //TODO:delete local testing
    querySensorAttributes("http://www.theworldavatar.com/kb/sgp/singapore/AirQualityStation-001.owl#AirQualityStation-001",function (err, result) {
console.log(result)});
    //***************************************************************************
    // buttons to tilt camera angle, rotate the map, and zoom in/out
    controlButtonsSetter(osmb);
    //***************************************************************************

    
    //***************************************************************************
    osmb.addMapTiles(
    	'https://a.tile.openstreetmap.org/{z}/{x}/{y}.png'
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
            //TODO: here, add the event listener for clicking on a object
            if(id && id.includes("marker")){//=>sensor marker query event
                //TODO: sensor query is added here
                let stationIRI = id.split('_')[1];
                console.log('clicked on station: '+stationIRI)
                querySensorAttributes(stationIRI, function (err, sensorAttributes) {
                    if (err){console.log(err)}
                    console.log('got sensor attributes to show');
                    console.log(sensorAttributes);
                    sensorAttributes.names= ['pollutant', 'concentration','time','allpsi','mean','max','min','individualpsi']
                    sensorAttributes.data.forEach(item=>{
                        let name = item[0].split('/');
                        name = name[name.length-1]
                        name = name.split('.owl')[0]
                        item[0] = name
                        let unit = item.splice(-1)[0]
                        let unitArr = unit.split('#')
                        unit = unitArr.splice(-1)
                        item[1] = parseFloat(item[1]).toFixed(2)+' '+unit
                        item[4] = parseFloat(item[4]).toFixed(2)+' '+unit
                        item[5] = parseFloat(item[5]).toFixed(2)+' '+unit
                        item[6] = parseFloat(item[6]).toFixed(2)+' '+unit
                        item[7] = parseFloat(item[7]).toFixed(2)

                    })
                    sensorAttributes.data.sort(function(a, b) {
                        var nameA = a[0].toUpperCase(); // ignore upper and lowercase
                        var nameB = b[0].toUpperCase(); // ignore upper and lowercase
                        if (nameA < nameB) {
                            return -1;
                        }
                        if (nameA > nameB) {
                            return 1;
                        }
                    });
                    renderAttributeTable(sensorAttributes);
                })
                   // });
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
    

    function startSimulation(){
    	//$('#start').attr("disabled", true);
    	
		console.log('button clicked')
		
		$('#inputFields').append('<img id="myProgressBar" style="width:100px;height:100px;" src="https://media.giphy.com/media/3oEjI6SIIHBdRxXI40/giphy.gif"/>'
);

		let keyvendor=window.location.pathname.split('/')[2];
		console.log("Keyvendor= "+keyvendor);
        var locationIRI =  mlocation = $('#location').val();
        let folder;
        console.log('locationIRI '+locationIRI);
        let agentScenario =  "/JPS_DISPERSION/" + keyvendor + "/results/latest";

        let agentInformation =  "/JPS_SHIP/GetExtraInfo";//"/info"
        //TODO:determine what sequence to query;
        $.get(agentScenario, {city:locationIRI}).done(function (data) {
            console.log('requested Scenario Agent for folder: '+data);
        folder = data;
        $.get(agentInformation, {filepath:folder}).done(function (info) {
            info=JSON.parse(info);
        	console.log('requested info agent for:');
            console.log(info);
            var buildingIRIs = info.building;
            var shipIRIs = info.ship;
            
            console.log("info="+info.region)
            
            var xmin = parseInt(info.region.lowercorner.lowerx);
            var xmax = parseInt(info.region.uppercorner.upperx);
            var ymin = parseInt(info.region.lowercorner.lowery);
            var ymax = parseInt(info.region.uppercorner.uppery);
            $('#xlower').val(xmin)
            $('#xupper').val(xmax)
            $('#ylower').val(ymin)
            $('#yupper').val(ymax)
            originRatio = (xmax-xmin)/(ymax-ymin);
            let ratio;
            [xmin, xmax, ymin, ymax, ratio] = appro2ratio(xmin, xmax, ymin, ymax); // 28 Aug 18
 
            var canvas = $('#drawcanvas'); canvas.width(1024*ratio).height(1024); // 28 Aug 18
            var svg = $('#contoursvg');svg.attr('width',1024*originRatio).attr('height',1024); // 28 Aug 18
            console.log(xmin+" "+xmax+" "+ymin+" "+ymax)
            const coordinatesMin = getOSMPoint(xmin, ymin);
            const coordinatesMax = getOSMPoint(xmax, ymax);
            const coordinatesMid = getMidPoint(coordinatesMin, coordinatesMax);
            console.log("buildingIRIs = " + buildingIRIs);
            console.log("shipIRIs = " + shipIRIs);
            $("#myProgressBar").remove();
            sensorIRIs = info.airStationIRI;
            //TODO: sensor rendering is added here
            querySensor(sensorIRIs, function (err, senesorData) {
                if(err || !senesorData){
                    console.log(error);
                } else{
                    renderSensorStations(senesorData);
                }
            });
                console.log("sensorIRIs = " + sensorIRIs);

            initadms3dmap(buildingIRIs, [xmin, xmax, ymin, ymax], osmb, mlocation, coordinatesMid, locationIRI, shipIRIs, folder);
        })
        })


    };
    //***************************************************************************

    
    //***************************************************************************
    // Sets position of camera at selected location
    $("#location").on("change", () => {
        const mlocation = $("#location option:selected").text();
        if (mlocation === "Singapore") {
            startSimulation();
            document.getElementById("optmsg").innerHTML = "";
            osmb.setPosition({
                latitude: 1.27993,//1.262008,
                longitude: 103.859//103.850973
            });
//            $("#xlower").val("11558666.37");
//            $("#xupper").val("11562079.502");
//            $("#ylower").val("139186.423");
//            $("#yupper").val("141908.33");

            osmb.setZoom(14.5);
            osmb.setTilt(20.6);
            osmb.setRotation(-45.6);

        }else if (mlocation === "Hong Kong") {
            startSimulation();
        	document.getElementById("optmsg").innerHTML="Buildings are projected down directly above the ground although elevation is considered in the calculations.";
            osmb.setPosition({
                longitude: 114.1491155592187,
                latitude: 22.28911086466781
            });
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