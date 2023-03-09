/**
 * Created by MASTE on 6/20/2017.
 */



// materialMap maps TankIDs and chemicals  
 
var materialMap = {
    //'TankID_1574': 'Chlorine',
	'TankID_1574': 'MethylChloride',
	'TankID_1354': 'Ammonia',
    'TankID_1329': 'Isobutylene',
    'TankID_1328': 'MethylChloride'
};



initMap();


// starts when google map javascript source is loaded 

function initMap() {

    var myLatLng = {lat: 1.2668080555555554, lng: 103.67884555555555};	// initial location at jurong island 
	

    // initiate the map within the element of id 'map'
    map = new google.maps.Map(document.getElementById('map'), {
        center: myLatLng,
        scrollwheel: true,          // allow user to use scrollwheel to zoom in or zoom out
        zoom: 17,                   // set the default zoom level to be 17
        panControl: true,
        panControlOptions: {
            position: google.maps.ControlPosition.TOP_RIGHT
        }
    });


	// create tank instances 
	
    var tank1 = {
        name: 'TankID_1328',
        pos: {lat:1.2673442, lng:103.6682412}
        //103.6682412 1.2673442
    };

    var tank2 = {
        name: 'TankID_1329',
        pos: {lat:1.2672867,lng:103.6681558}
    };

    var tank3 = {
        name: 'TankID_1354',
        pos: {lat:1.2613125,lng:103.6981983}
    };

    var tank4 = {
        name: 'TankID_1574',
        pos: {lat:1.2611653,lng:103.6994828}
    };

    var tanks = [tank1,tank2,tank3];


    for(var k = 0; k < tanks.length; k++)
    {
        var tank = tanks[k];
        var tankIcon = {
            url: 'http://www.theworldavatar.com/ADMSTEST/icon.png', // set icons for the tanks 
            size: new google.maps.Size(400, 500),
            origin: new google.maps.Point(0, 0),
            anchor: new google.maps.Point(0, 0)
        };

        var tankMarker = new google.maps.Marker({
            position: tank.pos,
            icon: tankIcon,
            map: map,
            title: tank.name
        });


        var infowindow = new google.maps.InfoWindow();

		
		/* important: set click event listener for tanks in google map, notice the infowindow parameter is the object to modify in order to 
		 * modify the popup window view
		 */
        google.maps.event.addListener(tankMarker, 'click', (function (tankMarker, k, infowindow) {
            return function () {
				
				// sent a request to Claudius to get attributes in owl files
				
                var muri = "http://www.theworldavatar.com/" + tankMarker.title + '.owl';
                console.log('muri ---',muri);
                $.ajax({
                    url: "http://www.theworldavatar.com:82/getAttrList",
                    method: "POST",
                    data: JSON.stringify({uri: muri}),
                    contentType: "application/json; charset=utf-8",
                    success: function (_attrPairs) {
                        console.log(_attrPairs);
                        var temperatureID    = '';
                        var temperatureValue = '';
                        var pressureID       = '';
                        var pressureValue    = '';
                        var diameterID       = '';
                        var diameterValue    = '';
                        var ambientPressureID= '';
                        var ambientPressureValue = '';

                        for(var i = 0 ; i < _attrPairs.length; i++)
                        {
                            var name = _attrPairs[i]['name'];
                            var value= _attrPairs[i]['value'];
                            if(name.includes('V_Temperature'))
                            {
                                temperatureID = name;
                                temperatureValue = value;
                            }

                            if(name.includes('V_Pressure_'))
                            {
                                pressureID = name;
                                pressureValue = value;
                            }

                            if(name.includes('V_Diameter_'))
                            {
                                diameterID = name;
                                diameterValue = value;
                            }

                            if(name.includes('V_ambientPressure'))
                            {
                                ambientPressureID = name;
                                ambientPressureValue = value;
                            }
                        }

						// construct pop window html 
                        var popContent =
                    '<h2 data data-step="1" data-intro="Id of the tank">' + tankMarker.title+'</h2><br/>' +
                    '<form id="'+  tankMarker.title+'">' +
                    '<table >' +
                    '<tr><td><label>Material: &nbsp;</label></td><td>'+ materialMap[tankMarker.title] +'<label> </label></td> </tr>' +
                    '<tr><td><label id='+ temperatureID +'>Temperature Inside Tank: &nbsp;</label></td><td><input value="'+ temperatureValue +'"></td><td>C</td></tr>' +
                    '<tr><td><label id='+ pressureID +'>Pressure Inside Tank: &nbsp;</label></td><td><input value="'+ pressureValue +'"></td><td>kPa</td></tr>' +
                    '<tr><td><label id='+ diameterID +'>Leakage Hole Diameter: &nbsp;</label></td><td><input value="'+ diameterValue +'"></td><td>cm</td></tr>' +
                    '<tr><td><label id='+ ambientPressureID +'>Ambient Pressure: &nbsp;</label></td><td><input disabled value="' + ambientPressureValue + '"></td><td>Pa</td></tr></table></form>' +
                    '' +
                    '<button onclick="startSimulation(this)" value="'+ tankMarker.title +'">Start Simulation</button>' +
					'<img id="myProgressBar" style="width:100px;height:100px;display:none" src="http://1.bp.blogspot.com/-sbLGKbobBwM/T6PQ2qFAL_I/AAAAAAAAARA/5iQ4SoAY52c/s320/08.gif"/><br/>' ;

                        console.log('Click! Marker=' + tankMarker.title);
                        infowindow.setContent(popContent);
                        infowindow.open(map, tankMarker,function () {
                        });

                    }});



                // the ente
                // construct the content in the popup window , should be executed
                // when ajax has a successful callback


            // execute at last


            };
        })(tankMarker, k, infowindow));

//


    }









    // Create a map object and specify the DOM element for display.
    // Create a marker and set its position.

}

function startSimulation(item) {
    var form = document.getElementById(item.value);
    var table = form.firstElementChild.childNodes[0].childNodes;
    var material = form;
    console.log(form);
    var tankID = form.getAttribute('id');

	
	var myProgressBar = document.getElementById('myProgressBar');
	myProgressBar.style.display = 'block';
	
    JSONArray = [];

    for (var i = 0; i < table.length; i++)
    {
        var name = table[i].getElementsByTagName('label')[0].innerText.split(':')[0];
        var value = table[i].getElementsByTagName('td')[1].firstElementChild.value;
        var JSONName    = '';
        var JSONValue   = value;
        var JSONType    = '';
        switch(name) {
            case 'Temperature Inside Tank':
                JSONName = 'V_Temperature_' + materialMap[tankID] + 'In' + tankID;
                console.log('JSONName',JSONName);
                break;
            case 'Pressure Inside Tank':
                //V_Pressure_AmmoniaInTankID_1354

                JSONName = 'V_Pressure_' + materialMap[tankID] + 'In' + tankID;
                console.log('JSOName',JSONName);
                break;
            case 'Leakage Hole Diameter':
                //V_Diameter_TankID_1354_LeakHole1
                JSONName = 'V_Diameter_' + tankID + '_LeakHole1';
                console.log('JSOName',JSONName);
                break;

            case 'Ambient Pressure':
                JSONName = 'V_ambientPressure';
                console.log('JSONName',JSONName);
                break;

            default:
                console.log('What is it???');
                break;
        }



        if(JSONValue)
        {
            var JSON =
            {

                name : JSONName,
                value : value

            };

            JSONArray[name] = JSON;
    }

    }

    JSONArray['Material'] = materialMap[tankID];
    doCalculation(JSONArray,tankID);



   // console.log(JSONArray)

}

function doCalculation(JSONArray,tankID)
{
     console.log(JSONArray);

     /*
        'TankID_1574': 'Chlorine',
        'TankID_1354': 'Ammonia',
        'TankID_1329': 'Isobutylene',
        'TankID_1328': 'MethylChloride'
      */

     var WeightMap =
     {
         'Chlorine': 71,
         'Ammonia' : 17.031,
         'Isobutylene': 56.106,
         'MethylChloride': 50.49
     };




     var temperature = parseFloat(JSONArray['Temperature Inside Tank']['value']);
     var pressure    = parseFloat(JSONArray['Pressure Inside Tank'].value) * 1000;
     var diameter    = parseFloat(JSONArray['Leakage Hole Diameter'].value) / 100;
     var ambient     = parseFloat(JSONArray['Ambient Pressure'].value);
     var material    = JSONArray['Material'];

     var weight      = WeightMap[material];


     var CapacityMap = {};

     CapacityMap['Chlorine'] = (8.28+0.00056 * (temperature + 273.15))*4184/ weight ;
     CapacityMap['Ammonia'] = (6.7+0.0063 * (temperature + 273.15))*4184/ weight;
     CapacityMap['MethylChloride'] = (34090+72460*Math.pow((1723/(temperature + 273.15))/Math.sinh(1723/(temperature + 273.15)), 2)+44800*Math.pow((780.5/(temperature + 273.15))/Math.cosh(780.5/(temperature + 273.15)), 2))/weight;
     CapacityMap['Isobutylene'] =  (24970+211.8*(temperature + 273.15)-5.7357*10E-5*Math.pow((temperature + 273.15), 3)+0.0014526*10E-10*Math.pow((temperature + 273.15), 4))/weight;


     /*
      Double gammacalcCL = (8.28+0.00056*Float.parseFloat(tempvalue))/(8.28+0.00056*Float.parseFloat(tempvalue)-1.987);
      Double gammacalcNH3 = (6.7+0.0063*Float.parseFloat(tempvalue))/(6.7+0.0063*Float.parseFloat(tempvalue)-1.987);
      Double gammacalcCH3Cl = CpCH3Cl*Float.parseFloat(mvalue)/(CpCH3Cl*Float.parseFloat(mvalue)-8314.4598);
      Double gammacalcIsobut = CpIsobut*Float.parseFloat(mvalue)/(CpIsobut*Float.parseFloat(mvalue)-8314.4598);
      */
     var GammaMap = {};
     GammaMap['Chlorine'] = (8.28+0.00056* (temperature + 273.15) )/(8.28+0.00056*(temperature + 273.15)-1.987);
     GammaMap['Ammonia']  = (6.7+0.0063*(temperature + 273.15))/(6.7+0.0063*(temperature + 273.15)-1.987);
     GammaMap['MethylChloride'] = CapacityMap['MethylChloride'] * weight/(CapacityMap['MethylChloride']*weight-8314.4598);
     GammaMap['Isobutylene'] = CapacityMap['Isobutylene']* weight/(CapacityMap['Isobutylene']* weight - 8314.4598);



    //console.log('Maps',tankID,CapacityMap,GamaMap);

    var capacity = CapacityMap[material];
    var gamma     = GammaMap[material];

    console.log('capacity',capacity);
    console.log('gamma',gamma);
    console.log('filename','http://www.theworldavatar.com/' + tankID + '.owl');

    var filename = 'http://www.theworldavatar.com/' + tankID + '.owl';

    var pchoke= pressure *Math.pow(2/(gamma+1), gamma/(gamma-1));
    var choked= 1*0.25*3.14*(diameter * diameter)* pressure *Math.pow(1* weight /1000* gamma/(8.314* (temperature + 273.15) )*Math.pow(2/(gamma+1),
                ((gamma-1)*(gamma+1))),0.5);

    var nonchoke=1*0.25*3.14*(diameter*diameter)*pressure*Math.pow(2*1*weight/1000*gamma/(8.314* (temperature + 273.15) *(gamma-1))*(Math.pow(ambient/pressure,2/gamma)
            -Math.pow(ambient/pressure,(gamma+1)/gamma)),0.5);


   // console.log('pchoke',pchoke);
   // console.log('choked',choked);
   // console.log('nonchoke',nonchoke);
    var flowRate = 0;
    if (pchoke <= ambient)
    {
        flowRate =nonchoke * 1000;
        console.log('flowRate nonchoke',nonchoke);
    }
    else
    {
        flowRate = choked * 1000;
        console.log('flowRate choked',choked);
    }


    console.log('flowRate ------', flowRate);

 // JSONArray['capacity'] = {name: }

    JSONArray['flowRate'] = {name: 'V_massF_LeakageStream_001', value: flowRate};

// write back to owl file, three calculated values : 1. capacity, 2. gamma, 3. flowRate
    // capacity:



    updateOwlFile(filename,JSONArray);








}


function updateOwlFile(filename,JSONArray) {

    /*<owl:NamedIndividual rdf:about="http://www.theworldavatar.com/TankID_1354.owl#V_Diameter_TankID_1354_LeakHole1">
     <rdf:type rdf:resource="http://www.theworldavatar.com/OntoCAPE/OntoCAPE/upper_level/system.owl#ScalarValue"/>
     <system:hasUnitOfMeasure rdf:resource="http://www.theworldavatar.com/OntoCAPE/OntoCAPE/supporting_concepts/SI_unit/SI_unit.owl#m"/>
     <system:numericalValue rdf:datatype="http://www.w3.org/2001/XMLSchema#float">0.01</system:numericalValue>
     </owl:NamedIndividual>

     DELETE { ?person foaf:givenName 'Bill' }
     INSERT { ?person foaf:givenName 'William' }

    base = http://www.theworldavatar.com/TankID_1354.owl#




     */


    console.log(JSONArray);
    var base = filename + '#';

    var sampleUpdate = [];
    var uri = [];


    var counter = 0;
    for(var item in JSONArray)
    {
        console.log(item);

        var obj = JSONArray[item];

        var targetIRI = obj.name;

        var base = filename + '#';
        var value = obj.value;

        if(targetIRI && counter >= 0 )
        {
            console.log('==== TargetIRI ====',targetIRI);
            var deleteUpdate = 'PREFIX system: <http://www.theworldavatar.com/OntoCAPE/OntoCAPE/upper_level/system.owl#>' +
                'DELETE WHERE{ <' + base + targetIRI + '> system:numericalValue ?o } ';

            var insertUpdate ='PREFIX system: <http://www.theworldavatar.com/OntoCAPE/OntoCAPE/upper_level/system.owl#>' +
                'INSERT DATA { <' + base + targetIRI + '> system:numericalValue ' + value + '} ';

            sampleUpdate.push(deleteUpdate);
            sampleUpdate.push(insertUpdate);
            uri.push(filename);
            uri.push(filename);
        }

        counter++;

    }



   // var sample = 'PREFIX dc: <http://purl.org/dc/elements/1.1/>  INSERT DATA {<http://example/book3> dc:title	"A brand new book" ;   dc:creator "A.N.Other" .}';
    //console.log(insertUpdate);




   // sampleUpdate = [deleteUpdate,insertUpdate];

   // var uri = [filename,filename];

    console.log('updates',sampleUpdate);
    console.log('uri',uri);


    var myUrl = 'http://www.theworldavatar.com/Service_Node_BiodieselPlant3/SPARQLEndPoint?uri='
        + encodeURIComponent(JSON.stringify(uri)) + '&update='
        + encodeURIComponent(JSON.stringify(sampleUpdate)) + '&mode=update';
    var request = $.ajax({
        url: myUrl,
        type: 'GET',
        contentType: 'application/json; charset=utf-8'
    });

    request.done(function(data) {
        console.log('data received', data);
        // start script to write inputs to



                       var path = "C:@TOMCAT@webapps@ROOT@ADMS_TEST_002@automated.bat>" + filename.split('.com/')[1].split('.owl')[0];
                       console.log('path --',path);

     //   var path = "C:@Users@MASTE@Desktop@test@test.bat>yotoxxxt.txt";
//C:@TOMCAT@webapps@ROOT@ADMSINPUTCalculation
//localhost:11614
                            var url = 'http://www.theworldavatar.com/Service_Node_BiodieselPlant3/startScript?path=' + encodeURIComponent(path);
                            var request = $.ajax({
                                url: url,
                                type: 'GET',
                                contentType: 'application/json; charset=utf-8'
                            });

                            request.done(function(data) {

                                window.location.href = 'http://www.theworldavatar.com/ADMS_TEST_002/';
                                console.log('data received from script starter', data);
                              //  window.open("http://www.theworldavatar.com/ADMS_TEST_002/");



                            });

                            request.fail(function(jqXHR, textStatus) {
                                // your failure code here
                            });







    });

    request.fail(function(jqXHR, textStatus) {
        // your failure code here
    });
}


