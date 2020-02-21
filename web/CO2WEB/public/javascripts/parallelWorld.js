(function PPMapAlt(){
		
    var ppMap = new PopupMap({useCluster:true});
    // just in case the kml file not loading in Claudius, use google sites. 
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


