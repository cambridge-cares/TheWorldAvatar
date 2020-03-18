
var infowindow;
var batteryInfo ="PREFIX j1:<http://www.theworldavatar.com/ontology/ontopowsys/PowSysRealization.owl#> "
		+ "PREFIX j2:<http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#> "
		+ "PREFIX j3:<http://www.theworldavatar.com/ontology/ontopowsys/model/PowerSystemModel.owl#> "
		+ "PREFIX j4:<http://www.theworldavatar.com/ontology/meta_model/topology/topology.owl#> "
		+ "PREFIX j5:<http://www.theworldavatar.com/ontology/ontocape/model/mathematical_model.owl#> "
		+ "PREFIX j6:<http://www.theworldavatar.com/ontology/ontocape/chemical_process_system/CPS_behavior/behavior.owl#> "
		+ "PREFIX j7:<http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/space_and_time/space_and_time_extended.owl#> "
		+ "PREFIX j8:<http://www.theworldavatar.com/ontology/ontocape/material/phase_system/phase_system.owl#> "
		+ "PREFIX j9:<http://www.theworldavatar.com/ontology/ontopowsys/PowSysBehavior.owl#> "
		+ "PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#> "
		
		+ "SELECT ?entity ?V_x ?V_x_unit ?V_y ?V_y_unit ?V_ActivePowerInjection_of_VRB ?V_ActivePowerInjection_of_VRB_unit "
		+ "WHERE {?entity   j9:hasActivePowerInjection ?cap ."
		+"?cap j2:hasValue ?vcap ."
		+"?vcap  j2:numericalValue ?V_ActivePowerInjection_of_VRB ."
		+"?vcap  j2:hasUnitOfMeasure ?V_ActivePowerInjection_of_VRB_unit ."

		+ "?entity   j7:hasGISCoordinateSystem ?coorsys ."

		+ "?coorsys  j7:hasProjectedCoordinate_y  ?y  ."
		+ "?y  j2:hasValue ?vy ." 
		+ "?vy  j2:numericalValue ?V_y ."
		+ "?vy  j2:hasUnitOfMeasure ?V_y_unit ."

		+ "?coorsys  j7:hasProjectedCoordinate_x  ?x  ."
		+ "?x  j2:hasValue ?vx ." 
		+ "?vx  j2:numericalValue ?V_x ."//longitude
		+ "?vx  j2:hasUnitOfMeasure ?V_x_unit ."//longitude

        + "}";
var batterylist = []; 
var batIRI="http://www.theworldavatar.com/kb/batterycatalog/BatteryCatalog.owl#BatteryCatalog";
var pvGenIRI=["http://www.theworldavatar.com/kb/sgp/semakauisland/semakauelectricalnetwork/PV-001.owl#PV-001"];//to be a jsonarray
(function PPMapAlt(){
    var ppMap = new PopupMap({useCluster:true});
    //refresh value of carbon emission every 5 minutes
    setInterval(function(){
        distotalemission();
    }, 10*1000);
    //when user changes input, create notification
    $(document).on('input', 'input', function () {//when user makes input
        console.log("input changed");
        cleanMsg();
        let el = $(this), value = el.val();
        if (value === "") { 
            return;
        }
       
    });
    
    /** once map is instantiated, run base scenario
     * 
     */
    var checkExist = setInterval(function() {
    if ($('#map').length) {
        console.log("Exists!");
        
        ppMap.clearAnimatedLines();
        clearMarkers();
        runKML();
        clearInterval(checkExist);
    }
    }, 100); // check every 100ms
    /** runs rest of functions with newly changed scenario
     * 
     */
    function runKML(){
        infowindow = new google.maps.InfoWindow({
            content: '<h2>Sup!</h2>'
        });
            
        json = { "electricalnetwork":iriofnetwork ,"flag": scenario }
        document.getElementById("loader").style.display = "block";
        ppMap.drawLines(json );
        drawMarkers(json);
        setTimeout(refreshLayer(json), 30000);
        setTimeout(displayCO2(json), 20000);
        
    }
    //link run button to variable
    let runBtn = $("#run-btn"); 
    /** launches Promise when button is clicked. Runs drawBatt
     * @throws rejection if battery is not called. 
     */
    runBtn.click(function () {
        scenario = document.getElementById("scenarioTxt").value;
        if (scenario == ''){
            scenario = "testBatt1"; //auto set scenario to standard to differentiate from base
        }
        // scenario=scenario+uuidv4();
        ppMap.clearAnimatedLines();
        clearMarkers();
        console.log(scenario);
        //initialize promise with function that has resolve and reject statements. 
        drawBattery(function(){
            console.log(batterylist);
            runKML();
        });
    });
    /** creates new scenario through ScenarioModifier.java agent
     * @param scenarioname the name of the scenario, be it base or specific folder 
     * @param agenturl: GET request to Java Backend Servlet
     * @param sparql: JSON packets or what not that the Java backend could request. 
     * @returns modified url for future use. 
     */
    function createNewUrlForAgent(scenarioname, agenturl, agentparams) {

        var url;
        if ((scenarioname == null) || scenarioname == "base") {
            url = agenturl;
        } else {
            agentparams['scenarioagentoperation'] = agenturl;
            var scenariourl = prefix + '/jps/scenariomod/' + scenarioname + '/call';
            url = scenariourl;
        }

        return url + "?query=" + encodeURIComponent(JSON.stringify(agentparams));
    }
    /** queries ESS ESS Coordination Agent
     * This uses createNewUrlForAgent which would create a new scenario. The agent below demands agent to be scenario capable. 
     * @param cb callback
     */
    function drawBattery(cb) 	{
		document.getElementById("run-btn").disabled = true;
		document.getElementById("loader").style.display = "block";
		var batteryjson= {}
		batteryjson["electricalnetwork"] = iriofnetwork;
		batteryjson["BatteryCatalog"] = batIRI;
		batteryjson["RenewableEnergyGenerator"] = pvGenIRI;
		var agenturl = prefix + '/JPS_ESS/startsimulationCoordinationESS';  
        var batteryurl = createNewUrlForAgent(scenario, agenturl, batteryjson); 
        //only calls this special method when creating a new scenario. Otherwise, it calls the basic createURLForAgent
		console.log(batteryurl);
	    var request = $.ajax({
	        url: batteryurl,
	        type: 'GET',
	        data: batteryjson,
            contentType: 'application/json; charset=utf-8',
            timeout:3000*1000,
	        success: function(){  
                console.log('successful execution');
	        },
	        error: function(ts) {
	            alert(ts.responseText);
	        }   
	    });
	    request.done( function(data) {
	        console.log ("success create request");
	        console.log(data);
            //suck the scenariourl to figure out the current scenario
            scenarioUR = JSON.parse(data).jpscontext.scenariourl;
            scenariolst  = scenarioUR.split('/');
            scenario = scenariolst[scenariolst.length-1]
	        batlist = JSON.parse(data).batterylist;
	        console.log(batlist);
	       	batterylist = [];
	        var size=batlist.length;
	        for(x=0;x<size;x++){
	        	//query each iri:
	        		var iri=batlist[x];
	    	        querybatlocation(iri,batteryInfo);
            }
            cb();
            
		});

    }
   
    /** queries the battery location and pushes results to batterylist
     * 
     * @param {String} iri 
     * @param {String} type 
     * @description modifies batterylist
     */
    function querybatlocation(iri,type){//url of battery, query of info. 
        var kmlurl = createUrlForSparqlQuery(scenario, iri.split('#')[0], type);
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
            console.log(data);
            var obj0 = JSON.parse(data);
            obj0 = obj0['results']['bindings'][0];
            console.log(obj0);
            batterylist.push([obj0.entity.value,obj0.V_x.value,obj0.V_y.value]);
            
        
        });
    }
    
    /*Msg***/
    let errMsgPanel = $("");

    function distotalemission(){
        $("#co2Value").text(actualCarbon.toFixed(2));
        $("#co2Value2").text(designCarbon.toFixed(2));
        $("#co2ValueYr").text(actualCarbonYr.toFixed(2));
        $("#co2ValueYr2").text(designCarbonYr.toFixed(2));
        $("#wildPercentage").text(wildPercentage.toFixed(2));
        $("#wildPercentage2").text(wildPercentage2.toFixed(2));
        $("#OilGen").text(dict["oil"]);
        $("#NatGasGen").text(dict["gas"]);
        $("#NoBat").text(batterylist.length);
    }
    //TODO: define err msg panel
    function cleanMsg() {
        errMsgPanel.html("");
    }
})();