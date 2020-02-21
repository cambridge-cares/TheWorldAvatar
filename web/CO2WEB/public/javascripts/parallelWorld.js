(function PPMapAlt(){
		
    var ppMap = new PopupMap({useCluster:true});
    // var anotherURL1 = "https://sites.google.com/site/kmlfilescares/kmltest1/testfinalBASE.kml";
    // var anotherURL2 = "https://sites.google.com/site/kmlfilescares/kmltest1/testfinaltestPOWSYSNuclearStartSimulationAndProcessResultAgentCallForTestScenario10.kml";
    var anotherURL1 =  'http://theworldavatar.com/OntoEN/testfinalbase.kml';
    var anotherURL2 = 'http://theworldavatar.com/OntoEN/testfinaltestPOWSYSNuclearStartSimulationAndProcessResultAgentCallForTestScenario10.kml';
    setInterval(function(){
        distotalemission();
    }, 5000);
    
    $(document).on('input', 'input', function () {//when user makes input
        console.log("input changed");
        cleanMsg();
        let el = $(this), value = el.val();
        if (value === "") { 
            return;
        }

        let attrid = el.attr("id");

        if (!validateInput(value)) {
            self.displayMsg(errMsgBox, "Wrong datatype.", "warning");
        }
    });


    //TODO: submit button that sends out simulation
    let runBtn = $("#run-btn");
    let selectedId = 0 ;
   
    // updatePredefined(selectedId)
    $("select#predefined-select").on('change', function () {
         selectedId = parseInt($("select#predefined-select option:checked").val());
         console.log(selectedId)


    })

    runBtn.click(function () {
        runKML(selectedId);
    })
    
    var checkExist = setInterval(function() {
        if ($('#map').length) {
           console.log("Exists!");
           runKML(0);
           clearInterval(checkExist);
        }
     }, 100); // check every 100ms
    //TODO: register for changes if want blinking effect of modification
    function runKML(predefinedId){
        console.log('predefinedID = ', predefinedId);
        infowindow = new google.maps.InfoWindow({
            content: '<h2>Sup!</h2>'
        });
        ppMap.clearAnimatedLines();
        clearMarkers();
        if (predefinedId == '0') {
            
            kmlURL = anotherURL1;
            scenario = "base";
            // appPrefix = prefix1;
        }
        else if (predefinedId == '1') {
            kmlURL = anotherURL2;
            // scenario = "testCoordinateRetroFitNuclearAgentCall20";
            scenario = "testPOWSYSNuclearStartSimulationAndProcessResultAgentCallForTestScenario10";
            // appPrefix = prefix2;
        }
            
        json = { "electricalnetwork":iriofnetwork ,"flag": scenario }
        document.getElementById("loader").style.display = "block";
        ppMap.drawLines(json );
        drawMarkers(json);
        refreshLayer(json, kmlURL);
        displayCO2(json);
        kmlURL = null;
        
    }
    function refreshLayer(iriofnetwork, kmlURL){
        if (kmlLayer){
            kmlLayer.setMap(null);
         }
        drawGenerator(iriofnetwork, kmlURL);
        console.log('Check that it should have refreshed. ')
    }
    //TODO: validate this
    function validateInput() {
        return true;
    }
    /*Msg***/
    let errMsgPanel = $("");

    function msgTemplate (msg, type) {
        return "<p class='alert alert-" + type + "'>" + msg + "</p>";
    }
    function displayMsg(msg, type) {
        //TODO: swithc type
        cleanMsg();
        errMsgPanel.append(msgTemplate(msg, type));

    }
    function distotalemission(){
        $("#co2Value").text(actualCarbon.toFixed(2));
        $("#co2Value2").text(designCarbon.toFixed(2));
        $("#co2ValueYr").text(actualCarbonYr.toFixed(2));
        $("#co2ValueYr2").text(designCarbonYr.toFixed(2));
        $("#wildPercentage").text(wildPercentage.toFixed(2));
        $("#wildPercentage2").text(wildPercentage2.toFixed(2));
        $("#NucGen").text(dict["nuclear"]);
        $("#OilGen").text(dict["oil"]);
        $("#NatGasGen").text(dict["gas"]);
    }
    //TODO: define err msg panel
    function cleanMsg() {
        errMsgPanel.html("");
    }
})();
var  kmlLayer;
function drawGenerator(data, anotherURL){
    var kmljson = {};
    var agenturl = prefix + '/JPS_POWSYS/ENVisualization/createKMLFile'; 
    var kmlurl = createUrlForAgent(scenario, agenturl, data);
    console.log(kmlurl);
    var request = $.ajax({
        url: kmlurl,
        type: 'GET',
        data: kmljson,
        contentType: 'application/json; charset=utf-8',
        success: function(){  
        },
        error: function(ts) {
            alert(ts.responseText);
        }   
    });

    request.done( function(data) {
    console.log ("success create request");
    kmlLayer = new google.maps.KmlLayer({
        // url: 'http://www.theworldavatar.com/OntoEN/testfinal.kml',//In other cases, will eventually be read and overwritten here. NO PROBLEM!
        url: anotherURL+ "?r="+(new Date()).getTime(), //this is completely necessary for cache-busting. 
        suppressInfoWindows: false,
        map: map
    });


        kmlLayer.addListener('click', function(kmlEvent) {
            setKMLMenu(kmlEvent)
        });             
        
    });

    request.fail(function(jqXHR, textStatus) {
    });
}
function drawMarkers(data){
    var agenturl=  prefix + '/JPS_POWSYS/ENVisualization/createMarkers'; 
    var kmlurl = createUrlForAgent(scenario, agenturl, data);
    console.log(kmlurl);
    var request = $.ajax({
        url: kmlurl,
        type: 'GET',
        async: true,
        contentType: 'application/json; charset=utf-8'
    });     
    
    request.done(function(data) {
        var obj0 = JSON.parse(data).result;
        var size=obj0.length; 
        
    //We currently know of a few cases:
    var x;
    // scan some duplicates
    dict = {"nuclear": 0, "gas": 0, "oil": 0};
    var markerdict = new Map();
    var latLng = new Map();
    for (x=0; x< size; x++){
        var obj = JSON.parse(obj0[x]);  
        var fueltype = obj.fueltype;
        var name = obj.name;
        if (fueltype== "NaturalGasGeneration"){
            var icon = {
                url: 'images/naturalgas.png',
                scaledSize : new google.maps.Size(40, 40),
            };
            dict["gas"] += 1;
        }else if (fueltype== "OilGeneration"){
            var icon = {
                url: 'images/oil.png',
                scaledSize : new google.maps.Size(40, 40),
            };
            dict["oil"] += 1;
        }else{
            var icon = {
                url: 'images/radiation.png', 
                scaledSize : new google.maps.Size(40, 40),
            };
            dict["nuclear"] += 1;
        }
        if (markerdict.has(obj.coors.lat)){
            var v = markerdict.get(obj.coors.lat);
            v.push(name);
            markerdict.set(obj.coors.lat, v);
        }else{
            markerdict.set(obj.coors.lat, [name]);
            
            latLng.set(obj.coors.lat, [obj.coors.lng, icon]);
            }
    }

    for (var [key, value] of latLng.entries()) {
        createMarker(key, value, markerdict);
          
    }
	
    });
    }
    function setMarkerMenu(jsonArray)
	{
		var buttonsList = '<p>Please select the Entity you would like to modify</p>';
		console.log(jsonArray);
		for(var index in jsonArray)
		{
			var name = jsonArray[index];
			console.log(name);
			 buttonsList = buttonsList + '<div><label>'  + name.split('#')[1] + '</label>'+
				'<button onclick="selectEBus(event)" style= "cursor: pointer;" id="' + name + '"> > </span></div>'
		}

		buttonsList = '<div id="buttonContainer">'+ buttonsList +'</div><hr/><div id="inputsContainer"></div>';
		// set the content of the popup window.
		infoWindowHtml = '<div>' + buttonsList + '</div>';

		console.log(infoWindowHtml);
		return infoWindowHtml;

	}
function createMarker(key, value, markerdict){
    var marker = new google.maps.Marker({
        position: new google.maps.LatLng(key, value[0]),
        map: map,
        icon: value[1]
      });
    marker.addListener('click', function(){
        _content = setMarkerMenu(markerdict.get(key));
        infowindow.setContent(_content);
        infowindow.open(map, this);
    });
    markers.push(marker);
}
    function clearMarkers() {
        if(!markers){
            return;
        }
        for(marker of markers){
            marker.setMap(null);
            marker=null;
        }
    }
function displayCO2(data){
    //read the value of CO2 and display upon calling
    var agenturl =  prefix + '/JPS_POWSYS/AggregationEmissionAgent/aggregateemission' ;
    var kmlurl = createUrlForAgent(scenario, agenturl, data);
    console.log(kmlurl);
    var request = $.ajax({
        url: kmlurl,
        type: 'GET',
        async: true,
        contentType: 'application/json; charset=utf-8'
    });     
    
    request.done(function(data) {
        var obj0 = JSON.parse(data);
        actualCarbon = obj0.actual;
        actualCarbonYr = actualCarbon*8760/1000000;
        wildPercentage = (actualCarbonYr/emissionValueForSingapore)*100*1000000;
        designCarbon = obj0.design;    
        designCarbonYr = designCarbon*8760/1000000;
        wildPercentage2 = (designCarbonYr/emissionValueForSingapore)*100*1000000;  
        document.getElementById("loader").style.display = "none";
    });
}

function setKMLMenu(kmlEvent){
    var data = kmlEvent.featureData;
    var nameString = data.name.substr(1);
    var names = nameString.split('[');
    var buttonsList = '<p>Please select the Entity you would like to modify</p>';
    for(var index in names)
    {
        var name = names[index];

        buttonsList = buttonsList + '<div><label>' + name.split('#')[1] + '</label>' +
            '<button onclick="selectEBus(event)" style= "cursor: pointer;" id="' + name + '"> > </span></div>'
    }

    buttonsList = '<div id="buttonContainer">'+ buttonsList +'</div><hr/><div id="inputsContainer"></div>';
    // set the content of the popup window.
    kmlEvent.featureData.infoWindowHtml = '<div>' + buttonsList + '</div>';

}
function selectEBus(event) {
    selectedId =  event.srcElement.id;
    openWindow(selectedId);
}

function openWindow(selectedId){
    if (selectedId.includes("Bus")){
        openWindowLineAndBus(selectedId, busInfo);
    }else if (selectedId.includes("ine")){
        //recreate infowindow if present. 
        openWindowLineAndBus(selectedId, branchInfo);
    }
    else{
        openWindowGen(selectedId);
    }
    
}
function openWindowGen(id){
 //since geninfo too large for request header, I'll split it up
    selectedId =  id; //this needs to be saved on a local version, and not towards here. 
    var kmljson = {};
    kmljson["sparqlquery"] = genInfo;
    kmljson["scenarioresource"] = selectedId.split('#')[0];
    var inputsHTML = '';
    var kmlurl = prefix + '/jps/scenario/'+scenario+'/query?query=' + encodeURIComponent(JSON.stringify(kmljson));
    kmljson["sparqlquery"] = genInfo2;
    var kmlurl2 = prefix +'/jps/scenario/'+scenario+'/query?query=' + encodeURIComponent(JSON.stringify(kmljson));
    $.when(
        $.ajax({
        url: kmlurl,
        type: 'GET',
        contentType: 'application/json; charset=utf-8',
        success: function(data){ 
            var obj0 = JSON.parse(data);
            obj1 = obj0['results']['bindings'][0];
        },
        error: function(ts) {
            alert(ts.responseText);
        }   
         }),
    
        $.ajax({
        url: kmlurl2,
        type: 'GET',
        contentType: 'application/json; charset=utf-8',
        success: function(data){   
            var obj0 = JSON.parse(data);
            obj2 = obj0['results']['bindings'][0];
            console.log(obj2);
        },
        error: function(ts) {
            alert(ts.responseText);
        }   
    })).then( function(){
        var obj0 = Object.assign(obj1, obj2);
        console.log(obj0,obj1, obj2)
        var result = Object.keys(obj0).map(function(key) {return [key, obj0[key]];});
        nameSet = [];
        console.log(selectedId);
        var owlName = selectedId.split('#')[1].split('.')[0];
        for(var item in result)
        {
            var pair = result[item];
            if (pair[0] == "entity"){}
            else if(!pair[1]['value'].includes('.owl')) //this is for values only. 
            {
                var inputLine = '<tr><td><label>' + pair[0]+"_" +owlName +'</label></td><td><input class="input_class" data-dataType="' + pair[1]['datatype'] 
                + '" value="' + pair[1]['value'] + '" style="float: right;"></td><td><input class="input_class" value="p.u." style="float: right;" disabled="disabled"></td></tr>';
                inputsHTML = inputsHTML + inputLine;
                nameSet.push(pair[0]);
            }else {
                //for units, just place below the box. 
                //remove the last 
                inputsHTML = inputsHTML.slice(0, -101)
                //add in the units 
                var inputLine = '</td><td><input class="input_class" data-dataType="' + pair[1]['datatype'] + '" value="' + pair[1]['value'].split('#')[1] + '" style="float: right;" disabled="disabled"> </td></tr>';
                inputsHTML = inputsHTML + inputLine;
            }
        }

        console.log(inputsHTML);
        var div = document.getElementById('inputsContainer');
        div.innerHTML = '<table data-type="kml" data-url='+ selectedId +' id="inputsTable">' + inputsHTML + '</table><br/><button onclick="SubmitTable(this)">OPF</button><button onclick="SubmitTable(this)">PF</button>'+
        '<img id="myProgressBar" style="width:100px;height:100px;display:none" src="https://media.giphy.com/media/3oEjI6SIIHBdRxXI40/giphy.gif"/><br/>'


    }, function (jqXHR, textStatus, errorThrown){
        alert(textStatus);
        console.log(errorThrown);
    }
    );
}
function openWindowLineAndBus(id, type, callback){ //gen has its own openWindow cos it's too large. 
    var kmlurl = createUrlForSparqlQuery(scenario, id.split('#')[0], type);
    console.log(kmlurl);
    var inputsHTML = '';
    var request = $.ajax({
        url: kmlurl,
        type: 'GET',
        contentType: 'application/json; charset=utf-8',
        success: function(){  
        },
        error: function(ts) {
            alert(ts.responseText);
        }   
    });
    request.done( function(data) {
        var obj0 = JSON.parse(data);
        obj0 = obj0['results']['bindings'][0];
        console.log(obj0)


        var result = Object.keys(obj0).map(function(key) {return [key, obj0[key]];});
        nameSet = [];
        var owlName = id.split('#')[1];
        for(var item in result)
        {
            var pair = result[item];
            if (pair[0] == "entity"){}
            else if(!pair[1]['value'].includes('.owl')) //this is for values only. 
            {
                var inputLine = '<tr><td><label>' + pair[0]+"_" +owlName +'</label></td><td><input class="input_class" data-dataType="' + pair[1]['datatype'] 
                + '" value="' + pair[1]['value'] + '" style="float: right;"></td><td><input class="input_class" value="p.u." style="float: right;" disabled="disabled"></td></tr>';
                inputsHTML = inputsHTML + inputLine;
                nameSet.push(pair[0]);
            }else {
                //for units, just place below the box. 
                //remove the last 
                inputsHTML = inputsHTML.slice(0, -101)
                //add in the units 
                var inputLine = '</td><td><input class="input_class" data-dataType="' + pair[1]['datatype'] + '" value="' + pair[1]['value'].split('#')[1] + '" style="float: right;" disabled="disabled"> </td></tr>';
                inputsHTML = inputsHTML + inputLine;
            }
        }

        console.log(inputsHTML);
        if (id.includes("Bus")){
            var div = document.getElementById('inputsContainer');
            div.innerHTML = '<table data-type="kml" data-url='+ selectedId +' id="inputsTable">' + inputsHTML + '</table><br/><button onclick="SubmitTable(this)">OPF</button><button onclick="SubmitTable(this)">PF</button>'+
            '<img id="myProgressBar" style="width:100px;height:100px;display:none" src="https://media.giphy.com/media/3oEjI6SIIHBdRxXI40/giphy.gif"/><br/>'
            }
            
            else if (callback == null){
                innerHTML = '<table data-type="line" data-url='+ selectedId +' id="inputsTable">' + inputsHTML + '</table><br/><button onclick="SubmitTable(this)">OPF</button><button onclick="SubmitTable(this)">PF</button>'+
                        '<img id="myProgressBar" style="width:100px;height:100px;display:none" src="https://media.giphy.com/media/3oEjI6SIIHBdRxXI40/giphy.gif"/><br/>';
                infoWindow.setContent(innerHTML);
            }
            
            else{
                const newPromise = new Promise((resolve, reject) => {
                    resolve('Success');
            });
                newPromise.then((successMessage) => {
                    innerHTML = '<table data-type="line" data-url='+ selectedId +' id="inputsTable">' + inputsHTML + '</table><br/><button onclick="SubmitTable(this)">OPF</button><button onclick="SubmitTable(this)">PF</button>'+
                        '<img id="myProgressBar" style="width:100px;height:100px;display:none" src="https://media.giphy.com/media/3oEjI6SIIHBdRxXI40/giphy.gif"/><br/>';
                    console.log(innerHTML);
                    callback(innerHTML);
                });
            }

    });

}
// function SubmitTable(e) {

//     opt = e.innerHTML;
//     var table = document.getElementById('inputsTable');
//     var rows = table.firstElementChild.childNodes;
//     var url = table.getAttribute('data-url');
//     var type = table.getAttribute('data-type');
//     console.log('type',type);

//     var JSONArray  = {};

//     var proceed = true;
//     console.log(rows.length, 'Rows length');
//     for(var i = 0; i < rows.length; i++)
//     {
//         var row = rows[i];
//         var name = row.getElementsByTagName('label')[0].innerText;
//         var value = row.getElementsByTagName('input')[0].value;
        
        
//         if(name.includes('EBus-001')){ // This is a slack bus, the magnitude is always 1 and the angle is always 0
//             //console.log("label forbidden= "+label);
//             if(name.includes('VoltageMagnitude')|| name.includes('Vm_EBus')) {
//                 if (value !== 1){
//                     alert('The value of the voltage magnitude and Vm for a slack bus should always be 1 kV (in p.u format)')
//                     proceed = false;
//                 }
//             }
            
//             if (name.includes('VoltageAngle')|| name.includes('Va_EBus')){
//                 if (value !== 0){
//                     alert('The value of the voltage angle and Va for a slack bus should always be 0 degree')
//                     proceed = false;
//                 }
//             }
//         }
//         else{ // This is a load bus 
//         //console.log("label forbidden= "+label);
//             if(name.includes('VoltageMagnitude')|| name.includes('Vm_EBus')){
//                 if( value > 1.05 || value <= 0.95){
//                     alert('The value of the voltage magnitude and Vm should be between 0.95 and 1.05 kV (in p.u format)')
//                     proceed = false;
//                 }
//             }           
//         }
        
        
        
        
        

        
//         var datatype = row.getElementsByTagName('input')[0].getAttribute('data-dataType');
//         console.log('value',value,'name',name,'url',url);
//         JSONArray[name] = {name: name,value:value, datatype: datatype }
//     }



//     if(proceed){
//         var progress = document.getElementById('myProgressBar');
//         progress.style.display = 'block';
//         updateOwlFile(url,JSONArray,type);
//     }


// }
// function updateOwlFile(filename,JSONArray,_type) {

//     console.log('number',Object.keys(JSONArray).length);
//     console.log('JSONArray',Object.keys(JSONArray));
//     console.log('IRI of resource=',filename);

//     var allItemsArray = [];
//     var indexCounter = 0;
//     var temp = [];
//     for(var item in JSONArray){
//             if(((indexCounter!== 0) && (indexCounter % 10 === 0))||(indexCounter === parseInt(Object.keys(JSONArray).length - 1)))
//             {
//                 if((indexCounter === parseInt(Object.keys(JSONArray).length - 1)))
//                 {
//                     //allItemsArray.push(temp);
//                     //temp = [];
//                     console.log('yes');
//                     allItemsArray.push(temp);
//                     temp = [];
//                     temp.push(item)
//                 }
//                 else
//                 {   console.log('nononon');
//                     allItemsArray.push(temp);
//                     temp = [];
//                     temp.push(item)
//                 }


//             }
//             else
//             {   console.log('yeah');
//                 temp.push(item)
//             }

//             console.log(indexCounter);
//             console.log(item)
//             indexCounter++;
//         }

//     console.log(allItemsArray);


//     var asyncLoop = function(o){
//         var i=-1,
//             length = o.length;
//             console.log(o);
//             console.log(length);
//         var loop = function(){
//             i++;
//             console.log(i);
//             if(i===length){
//                 console.log("CALLBACK called? ");
//                 o.callback(); 
//                 return;
//             }
//             o.functionToLoop(loop, i);
//         };
//         loop();//init
// };


// asyncLoop({
//     length : Math.ceil(Object.keys(JSONArray).length / 10),
//     functionToLoop : function(loop, i){


//         var sampleUpdate = [];
//         var uri = [];

//         var Session = allItemsArray[i];
//         console.log(length);
//         console.log('Session',Session);

//         for(var j = 0; j < Session.length; j++)
//         {
//             var item = Session[j];
//             var obj = JSONArray[item];
//             var targetIRI = obj.name;
//             var dataType = obj.datatype;
//             if(dataType === 'int')
//             {
//                 dataType = 'integer'
//             }
//             console.log('dataType',dataType);
//             var base = filename.split('#')[0] + '#';
//             base = base.replace('/OntoEN','');
//             base=base.replace('theworldavatar','jparksimulator'); //because in electrical it use jparksimulator instead of theworldavatar
//             var value = obj.value;
//             if(targetIRI)
//             {

//                 if (targetIRI.includes("Costn1")){
//                     targetIRI = targetIRI.replace("Costn1","Costn-1" );
                    
//                     console.log(targetIRI);
//                 }else if (targetIRI.includes("Costn2")){
//                     targetIRI = targetIRI.replace("Costn2","Costn-2" );
                    
//                     console.log(typeof targetIRI);
//                 }
//                 var deleteUpdate = "DELETE WHERE {<" + base + targetIRI + "> <http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#numericalValue> " + "?o.}";
//                 var insertUpdate = "INSERT DATA {<" + base + targetIRI + "> <http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#numericalValue> " +value +".}";
                    
                    

//                 console.log('deleteUpdate',deleteUpdate);
//                 console.log('insertUpdate',insertUpdate);

//                 sampleUpdate.push(deleteUpdate);
//                 sampleUpdate.push(insertUpdate);
                
//                 uri.push(filename);
//                 uri.push(filename);
                
//             }
//         }
//         console.log(scenario);
//         console.log(sampleUpdate); 
//         var myUrl = createUrlForSparqlUpdate(scenario,base.split('#')[0], sampleUpdate.join(';'));
//         var request = $.ajax({
//             url: myUrl,
//             type: 'GET',
//             contentType: 'application/json; charset=utf-8'
//         });
//         console.log(myUrl);
//         request.done(function(data) {
//             console.log('data received', data);
//             loop();


//         });

//         request.fail(function(jqXHR, textStatus) {
//             // your failure code here
//         });


//     },
//     callback : function(){

//         //var path = "C:@TOMCAT@webapps@ROOT@OntoEN@startSimulation.bat>" + filename.split('.com/')[1].split('.owl')[0] + '>' + opt;

//         document.getElementById("loader").style.display = "block";
//         var agenturl = prefix + '/JPS_POWSYS/ENAgent/startsimulation'+opt;
//         data = { "electricalnetwork":iriofnetwork}
//         url = createUrlForAgent(scenario, agenturl, data);
//         console.log(url);
//         var delayInMilliseconds = 10000; //1 second

//             setTimeout(function() {
//                 console.log('timeout');
//             }, delayInMilliseconds);
//         var request = $.ajax({
//             url: url,
//             type: 'GET',
//             contentType: 'application/json; charset=utf-8'
//         });

//         request.done(function() {
//             json = { "electricalnetwork":iriofnetwork ,"flag": scenario };
//             displayCO2(json);
//             var delayInMilliseconds = 10000; //1 second
//             setTimeout(function() {
//                 console.log('timeout');
//             }, delayInMilliseconds);
//             console.log('DONE SIMULATION')
//             openWindow(filename);
//             document.getElementById("loader").style.display = "none";
//         });


//     }
// });

// }


function constructLineMenu(id,callback){
    selectedId = id.split('/')[1];
    console.log(selectedId)
    var promise1 = new Promise(function (resolve, reject){
        resolve(openWindowLineAndBus('http://www.jparksimulator.com/kb/sgp/jurongisland/jurongislandpowernetwork/' + selectedId +'#'+selectedId.split('.')[0], branchInfo, callback));
    }); 
    promise1.catch(alert);
}
