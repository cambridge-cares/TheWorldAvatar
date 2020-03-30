scenario = "testFW";
prefix = "http://localhost:8080";
markers = []
var infowindow;
var listOfIRIs = [];
transportIRI = "http://www.theworldavatar.com/kb/sgp/singapore/wastenetwork/TransportSystem-001.owl#TransportSystem-001"; 
wastenetwork = "http://www.theworldavatar.com/kb/sgp/singapore/wastenetwork/SingaporeWasteSystem.owl#SingaporeWasteSystem";
FCQuery = "PREFIX j1:<http://www.theworldavatar.com/ontology/ontowaste/OntoWaste.owl#> "
+ "PREFIX j2:<http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#> "
+ "PREFIX j3:<http://www.theworldavatar.com/ontology/ontopowsys/PowSysPerformance.owl#> "
+ "PREFIX j4:<http://www.theworldavatar.com/ontology/meta_model/topology/topology.owl#> "
+ "PREFIX j5:<http://www.theworldavatar.com/ontology/ontocape/model/mathematical_model.owl#> "
+ "PREFIX j6:<http://www.w3.org/2006/time#> "
+ "PREFIX j7:<http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/space_and_time/space_and_time_extended.owl#> "
+ "PREFIX j8:<http://www.theworldavatar.com/ontology/ontotransport/OntoTransport.owl#> "
+ "SELECT  ?entity  ?name ?V_x ?V_x_unit ?V_y ?V_y_unit ?Waste_Production ?wasteproductionunit "
+ "WHERE {"
+ "?entity  a j1:FoodCourt ."
+ "?entity   j8:hasName ?name ." 
+ "?entity   j7:hasGISCoordinateSystem ?coorsys ."

+ "?coorsys  j7:hasProjectedCoordinate_x  ?x  ."
+ "?x  j2:hasValue ?vx ." 
+ "?vx  j2:numericalValue ?V_x ."//latitude
+ "?vx  j2:hasUnitOfMeasure ?V_x_unit ."//latitude

+ "?coorsys  j7:hasProjectedCoordinate_y  ?y  ." 
+ "?y  j2:hasValue ?vy ." 
+ "?vy  j2:numericalValue ?V_y ."//longitude
+ "?vy  j2:hasUnitOfMeasure ?V_y_unit ."//longitude

+ "?entity   j1:produceWaste ?WP ." 
+ "?WP     j2:hasValue ?vWP ."
+ "?vWP  j2:numericalValue ?Waste_Production ."
+ "?vWP  j2:hasUnitOfMeasure ?wasteproductionunit ."//longitude
 
+ "}";
WTQuery="PREFIX j1:<http://www.theworldavatar.com/ontology/ontowaste/OntoWaste.owl#> "
    + "PREFIX j2:<http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#> "
    + "PREFIX j3:<http://www.theworldavatar.com/ontology/ontopowsys/PowSysPerformance.owl#> "
    + "PREFIX j4:<http://www.theworldavatar.com/ontology/meta_model/topology/topology.owl#> "
    + "PREFIX j5:<http://www.theworldavatar.com/ontology/ontocape/model/mathematical_model.owl#> "
    + "PREFIX j6:<http://www.w3.org/2006/time#> "
    + "PREFIX j7:<http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/space_and_time/space_and_time_extended.owl#> "
    + "PREFIX j8:<http://www.theworldavatar.com/ontology/ontotransport/OntoTransport.owl#> "
    + "SELECT ?entity ?V_x ?V_x_unit ?V_y ?V_y_unit "
    + "WHERE {" 
    + "?entity  a j1:OffsiteWasteTreatmentFacility ."			
    + "?entity   j7:hasGISCoordinateSystem ?coorsys ." 
    + "?coorsys  j7:hasProjectedCoordinate_x  ?x  ."
    + "?x  j2:hasValue ?vx ." 
    + "?vx  j2:numericalValue ?V_x ."//latitude
    + "?vx  j2:hasUnitOfMeasure ?V_x_unit ."//latitude

    + "?coorsys  j7:hasProjectedCoordinate_y  ?y  ." 
    + "?y  j2:hasValue ?vy ." 
    + "?vy  j2:numericalValue ?V_y ."//longitude
    + "?vy  j2:hasUnitOfMeasure ?V_y_unit ."//longitude
    + "}";

//
transportQuery = "PREFIX j1:<http://www.theworldavatar.com/ontology/ontowaste/OntoWaste.owl#> "
    + "PREFIX j2:<http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#> "
    + "PREFIX j3:<http://www.theworldavatar.com/ontology/ontopowsys/PowSysPerformance.owl#> "
    + "PREFIX j4:<http://www.theworldavatar.com/ontology/meta_model/topology/topology.owl#> "
    + "PREFIX j5:<http://www.theworldavatar.com/ontology/ontocape/model/mathematical_model.owl#> "
    + "PREFIX j6:<http://www.w3.org/2006/time#> "
    + "PREFIX j7:<http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/space_and_time/space_and_time_extended.owl#> "
    + "PREFIX j8:<http://www.theworldavatar.com/ontology/ontotransport/OntoTransport.owl#> "
    + "SELECT ?Unit_transport_capacity ?Unit_transport_capacity_unit ?Unit_transport_cost ?Unit_transport_cost_unit "
    + " ?pollutionTransportTax ?pollutionTransportTax_unit ?dieselConsTruck ?dieselConsTruck_unit " 
    + "WHERE {"
    + "?entity  a j8:TransportationRoute ."
    + "?entity   j8:suitFor ?truck ." 
    + "?truck   j1:hasTax ?PTT ." 
    + "?PTT     j2:hasValue ?vPTT ."
    + "?vPTT  j2:numericalValue ?pollutionTransportTax ."
    + "OPTIONAL{?vPTT  j2:hasUnitOfMeasure ?pollutionTransportTax_unit }"

    + "?truck   j8:hasTransportationCapacity ?TC ." 
    + "?TC     j2:hasValue ?vTC ."
    + "?vTC  j2:numericalValue ?Unit_transport_capacity ."
    + "OPTIONAL{?vTC  j2:hasUnitOfMeasure ?Unit_transport_capacity_unit }"

    + "?truck   j8:hasTransportationCost ?TCost ." 
    + "?TCost     j2:hasValue ?vTCost ."
    + "?vTCost  j2:numericalValue ?Unit_transport_cost ." 
    + "OPTIONAL{?vTCost  j2:hasUnitOfMeasure ?Unit_transport_cost_unit }" 

    + "?truck   j8:hasEmission ?Temission ." 
    + "?Temission     j2:hasValue ?vTemission ."
    + "?vTemission  j2:numericalValue ?dieselConsTruck ." 
    + "OPTIONAL{?vTemission  j2:hasUnitOfMeasure ?dieselConsTruck_unit }" 

    + "}";

/** Gets the following output values of the composite waste system. 
	 * the name, revenue, installation cost, operational cost, labor cost, land cost, pollution cost, transport cost
	 * resource cost. 
	 */
var wasteSystemOutputQuery = "PREFIX j1:<http://www.theworldavatar.com/ontology/ontowaste/OntoWaste.owl#> "
    + "PREFIX j2:<http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#> "
    + "PREFIX j3:<http://www.theworldavatar.com/ontology/ontopowsys/PowSysPerformance.owl#> "
    + "PREFIX j4:<http://www.theworldavatar.com/ontology/meta_model/topology/topology.owl#> "
    + "PREFIX j5:<http://www.theworldavatar.com/ontology/ontocape/model/mathematical_model.owl#> "
    + "PREFIX j6:<http://www.w3.org/2006/time#> "
    + "PREFIX j7:<http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/space_and_time/space_and_time_extended.owl#> "
    + "PREFIX j8:<http://www.theworldavatar.com/ontology/ontotransport/OntoTransport.owl#> "
    + "SELECT ?V_ResourceConsumptionCost ?V_TransportCost ?V_PollutionTreatmentTax "
    + " ?V_LandCost ?V_ManPowerCost ?V_OperationalCost ?V_InstallationCost "
    + " ?V_TotalRevenue " 
    + "WHERE {"
    + "?entity  a j2:CompositeSystem ."
    + "?entity   j3:hasUtilityCost ?UC1 ."
    + "?UC1  a j3:UtilityCosts ."
    + "?UC1     j2:hasValue ?vresourcecost ." 
    + "?vresourcecost  j2:numericalValue ?V_ResourceConsumptionCost ." 
    
    + "?entity   j8:hasTransportationCost ?TC1 ."
    + "?TC1     j2:hasValue ?vtransportcost ." 
    + "?vtransportcost     j2:numericalValue ?V_TransportCost ." 
    
    + "?entity   j1:hasTax ?PC1 ."
    + "?PC1     j2:hasValue ?vpollutioncost ."  
    + "?vpollutioncost     j2:numericalValue  ?V_PollutionTreatmentTax ." 

    + "?entity   j3:hasCost ?LC1 ."
    + "?LC1  a j3:CostsForLand ."
    + "?LC1     j2:hasValue ?vlandcost ."
    + "?vlandcost     j2:numericalValue  ?V_LandCost ."

    + "?entity   j3:hasLaborCost ?LabC1 ."
    + "?LabC1     j2:hasValue ?vlaborcost ." 
    + "?vlaborcost    j2:numericalValue ?V_ManPowerCost ." 
    
    + "?entity   j3:hasCost ?OC1 ."
    + "?OC1  a j3:OperationalExpenditureCosts ."
    + "?OC1     j2:hasValue ?voperationalcost ."
    + "?voperationalcost     j2:numericalValue ?V_OperationalCost ."
                
    + "?entity   j3:hasInstallationCost ?IC1 ."
    + "?IC1     j2:hasValue ?vinstallationcost ."  
    + "?vinstallationcost    j2:numericalValue ?V_InstallationCost ."  
    
    + "?entity   j3:hasRevenue ?Rev1 ."
    + "?Rev1     j2:hasValue ?vrevenue ." 
    + "?vrevenue     j2:numericalValue  ?V_TotalRevenue ." 

    + "}";
(function PPMap(){
    var ppMap = new PopupMap({useCluster:true});
})();

/** once map is instantiated, run base scenario
 * 
 */
var checkExist = setInterval(function() {
    if ($('#map').length) {
        position = new google.maps.LatLng(1.367165198,103.801163462);
        map.setCenter(position);
        map.setZoom(12);
        queryForFCMarkers(function(){
            InitialTransportInputs();
            InitialUnitInputs();
        });
        
        clearInterval(checkExist);
    }
}, 100); // check every 100ms

  //when button clicked, run simulation
$(document).ready(function () {
    $("#run-btn").click(function () {
        console.log("Start sim")
        transportUpd = getInputs("table#transportQ tr");
        var offSiteIUpd = getInputs("table#Incineration tr");
        var offSiteCUpd = getInputs("table#CoDigestion tr");
        var offSiteAUpd = getInputs("table#AnaerobicDigestion tr");
        var onSite = getInputs("table#Onsite tr");
        finalArray = [onSite,offSiteIUpd,offSiteCUpd,offSiteAUpd]
        if (JSON.stringify(transportUpd) != JSON.stringify(inittransportUpd)){
            dumpTransport(transportUpd);}
        for (i = 0; i<initArray.length; i++){
            console.log("Initial Array",initArray[i]);
            console.log("Final Array",finalArray[i]);
            if (JSON.stringify(initArray[i])!= JSON.stringify(finalArray[i])){
                updateSite(finalArray[i], i);
            }
        }
        runWTESimulation(function(){
            var dt = Date();
            console.log("Check callback" +dt );
            queryForEconomicComp();
            queryForOnsiteWT();
        });


    });
});
/** run simulation for waste to energy. 
 * @param callback run query for Onsite afterwards
 */
function runWTESimulation(callback){
var para = {"wastenetwork":wastenetwork};

var agenturl = prefix + '/JPS_WTE/startsimulationCoordinationWTE';  
var simulationurl = createUrlForAgent(scenario, agenturl, para); 
var request = $.ajax({
    url: simulationurl ,
    type: 'GET',
    contentType: 'application/json; charset=utf-8',
    success: function(){  
        console.log('successful execution of simulation');
    },
    error: function(ts) {
        alert(ts.responseText);
    }
    
    });  
    //Because simulation takes some time to run, then asynchronous watcher object is triggered next. 
    request.done(function(data) { 
        console.log(data);
        setInterval(function(){
            console.log("Wait for simulation to finish"); //want to fix this. 
        }, 300*1000);
        var dt = Date();
        console.log("Check inferency: "+dt); 
        callback();

    });
}
/** read output of economic costs in cumulative for landcost
 * 
 */
function queryForEconomicComp(){
    var wasteurl = createUrlForSparqlQuery(scenario, wastenetwork,wasteSystemOutputQuery);
    $.ajax({
        url: wasteurl,
        method: "GET",
        success: function (data) {
            var obj0 = JSON.parse(data);
            obj0 = obj0['results']['bindings'][0];
            console.log(obj0);
            dumpEconomic(obj0);
        },
        error: function () {
            console.log("Failed query for compositeQuery")
        }
    });
}
/**  insert economic output data (I think I'm supposed to put it here?)
 * @param data key value pairs of costs
 */
function dumpEconomic(data){
    console.log(data.V_TotalRevenue);
    $("#RCC").text(data.V_ResourceConsumptionCost.value.toFixed(2));
    $("#TC").text(data.V_TransportCost.value.toFixed(2));
    $("#PTT").text(data.V_PollutionTreatmentTax.value.toFixed(2));
    $("#LC").text(data.V_LandCost.value.toFixed(2));
    $("#MC").text(data.V_ManPowerCost.value.toFixed(2));
    $("#OC").text(data.V_OperationalCost.value.toFixed(2));
    $("#IC").text(data.V_InstallationCost.value.toFixed(2));
    $("#TR").text(data.V_TotalRevenue.value.toFixed(2));
}
function queryForOnsiteWT(){

}
const textPanel = $("#text-panel")
const rePanel = $("#result-panel")
const drawPanel = $("#draw-panel")
/** creates Transportation Table to enter values
 * 
 */
function InitialTransportInputs(){
    var iri= transportIRI;
    var transporturl = createUrlForSparqlQuery(scenario, iri, transportQuery);
    $.ajax({
        url: transporturl,
        method: "GET",
        //  contentType: "application/json; charset=utf-8",
        success: function (data) {
            var obj0 = JSON.parse(data);
            obj0 = obj0['results']['bindings'][0];
            console.log(obj0);
            var result = Object.keys(obj0).map(function(key) {return [key, obj0[key]];});
            addInitialInputsDisplay(result, iri);
            inittransportUpd = getInputs("table#transportQ tr");
        },
        error: function () {
            console.log("Can not get location")
        }
    });

}
/** grabs Unit costs for technology, for both onsite and offsite
 * 
 */
function InitialUnitInputs(){
    var agenturl = prefix + '/JPS_WTE/WTEVisualization/readInputs'; 
    var data = {"wastenetwork": wastenetwork};
    var tableUrl = createUrlForAgent(scenario, agenturl, data);
    $.ajax({
        url: tableUrl,
        method: "GET",
        //  contentType: "application/json; charset=utf-8",
        success: function (data) {
            var obj0 = JSON.parse(data).offsite;
            var obj1 = JSON.parse(data).onsite;
            textPanel.add("<h1>Types of Digestion</h1>")
            addUnitInputsDisplay(obj0, "Offsite");
            addUnitInputsDisplay(obj1, "Onsite");
            
            initoffSiteIUpd = getInputs("table#Incineration tr");
            initoffSiteCUpd = getInputs("table#CoDigestion tr");
            initoffSiteAUpd = getInputs("table#AnaerobicDigestion tr");
            initonSite = getInputs("table#Onsite tr");
            initArray = [initonSite,initoffSiteIUpd,initoffSiteCUpd,initoffSiteAUpd];
        },
        error: function () {
            console.log("Can not get location")
        }
    });

}
/** create table for uri pairs. Store uripairs and change when there is???
 * @param {[key, [type, datatype, value]]} uripair
 */
function addInitialInputsDisplay(uripair, iri){
    var inputsHTML = "";
    uripair.forEach((data)=>{
        console.log(data);
        if (data[0].includes("unit")){
            var inputLine = '</td><td><input class="input_class" value="' + data[1]['value'].split('#')[1] 
            + '" style="float: right;" disabled="disabled"> </td></tr>';
        }else{
            var inputLine = '<tr><td><label>' + data[0]
            +'</label></td><td><input class="input_class" data-dataType="' 
            + data[1]['datatype'] + '" value="' + data[1]['value'] 
            + '" style="float: right;">';
        }
        inputsHTML += inputLine;
    });
    var table = '<table data-url='+ iri
    +' id="transportQ">' + inputsHTML 
    + '</table><br>';
    textPanel.append(table);
    return;
}
/** outputUpdate: Only to be used for short values! Otherwise you have to get an asynchronous loop as seeen in pwBaseFile.js
 * 
 * @param {Array} lstOfTargetIRI IRI of variable parameters
 * @param {String} base iri of OWL file
 * @param {Array} UpdateArray List of values to be updated
 */
function outputUpdate(lstOfTargetIRI, base, UpdateArray){

    var sampleUpdate = []
    for (i = 0; i<lstOfTargetIRI.length; i++){
        var deleteUpdate = "DELETE WHERE {<" + base + lstOfTargetIRI[i] + "> <http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#numericalValue> " + "?o.}";
        var insertUpdate = "INSERT DATA {<" + base + lstOfTargetIRI[i]+ "> <http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#numericalValue> " +UpdateArray[i] +".}";
        sampleUpdate.push(deleteUpdate);
        sampleUpdate.push(insertUpdate);
    }
        var myUrl = createUrlForSparqlUpdate(scenario,base.split('#')[0], sampleUpdate.join(';'));
        console.log(sampleUpdate);
        console.log(myUrl);
        var request = $.ajax({
            url: myUrl,
            type: 'GET',
            contentType: 'application/json; charset=utf-8', 
            success: function (data) {//SUCESS updating
                //Update display
                console.log("Success");
            },
            error: function (err) {
                console.log("can not update to server");
            }
        });
        request.done(function(data) {
        });
}
/** dumps transport values into OWL file
 * 
 * @param {Array} transportArray List of values to be transferred. 
 */
function dumpTransport(transportArray){
    base = transportIRI.split('#')[0];
    lstOfTargetIRI = ["#V_TransportationCapacityOfUnitTruckinTransportSystem-001",
     "#V_TransportCostOfUnitTruckinTransportSystem-001",
      "#V_PollutionTransportTaxOfUnitTruckinTransportSystem-001",
    "#V_EmissionOfUnitTruckinTransportSystem-001"]
    outputUpdate(lstOfTargetIRI, base, transportArray);
    
}
/** input parameters are updated
 * 
 * @param {Array} inpParameters input Parameters from respective digester
 * @param {Integer} index Incineration = 1, Co = 2, Ana = 3
 */
function updateSite(inpParameters, index){
    lstOfTargetIRI = ["#V_OperationalCost", "#V_InstallationCost", "#V_PollutionTreatmentTax"];
    listOfIRIs.forEach((iri)=>{ //listOfIRIs are the list of networks in the top node. 
        //strip and add the name of the WasteTreatment plant to each TargetIRI
        newLst = [] 
        var base = iri.split('#')[0];
        for (i = 0; i < 3; i++){
            var var_name = lstOfTargetIRI[i] + index + "Of"+iri.split("#")[1];
            console.log(var_name);
            newLst.push(var_name);
        }
        console.log(base);
        outputUpdate(newLst, base, inpParameters);
    })

}
/** create table for json. 
 * @param {JSON} data
 */
function addUnitInputsDisplay(data, type){
    var inputsHTML = "";
    var ndex = 0;
    var type1 = "";
    var type2 = "";
    var unitsForWT = [""]
    var typesOfDigestion = ["Incineration","CoDigestion", "AnaerobicDigestion"]
    var typesOfDigestion2 = ["Incineration","Co-Digestion", "Anaerobic Digestion"]
    data.forEach((i)=>{
        console.log(i);
        var v = JSON.parse(i);
        console.log(v);
        var result = Object.keys(v).map(function(key) {return [key, v[key]];});
        result.forEach((j)=>{
            console.log(j);
            var inputLine = '<tr><td><label>' + j[0]
            +'</label></td><td><input class="input_class" value="' + j[1]
            + '" style="float: right;"><td><input class="input_class" disabled="disabled" value="' +"USD"
            + '" style="float: right;"></td></tr>';
            
            inputsHTML += inputLine;
        }
        )
        if (type=="Offsite"){
            type1 = type +" "+typesOfDigestion2[ndex]; 
            type2 =  typesOfDigestion[ndex]; 
            ndex += 1;
        }else{
            type2 = type;
            type1 = "Onsite";
        }
        var table = '<h2>'+type1+'</h2><table data-url='+ wastenetwork
        +' id="' + type2 +'">' + inputsHTML 
        + '</table><br>';
        inputsHTML = "";
        textPanel.append(table);
    });
    
    return;
}


/** query backend using greedy method for FC and Offsite. Does Callback to read input from scenario. 
 *  runs during init
 */
function queryForFCMarkers(callback){
    infowindow = new google.maps.InfoWindow({
        content: '<h2>Sup!</h2>'
    });
    var agenturl = prefix + '/JPS_WTE/WTEVisualization/createMarkers'; 
    var data = {"wastenetwork": wastenetwork};
    var markerURL = createNewUrlForAgent(scenario, agenturl, data);
    var request = $.ajax({
        url: markerURL,
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
        var obj0 = JSON.parse(data).result;
        var size=obj0.length; 
        
    //We currently know of a few cases:
    var x;
    // scan some duplicates
    var markerdict = []; 

    for (x=0; x< size; x++){
        
        var obj = JSON.parse(obj0[x]);  
        var name = obj.entity;
        console.log(name);
        if (name.includes("FoodCourt")){
            var icon = {
                url: 'images/naturalgas.png',
                scaledSize : new google.maps.Size(30, 30),
            };
        }else{
            var icon = {
                url: 'images/solar.png', 
                scaledSize : new google.maps.Size(30, 30),
            };
            listOfIRIs.push(name);  
        }
        markerdict.push([name, obj.coors.lat,obj.coors.lng, icon]);
    }

    for (x=0; x< size; x++){
        createMarker(markerdict[x]);
    }
    //run callback function of other initiations. 
    scenarioUR = JSON.parse(data).jpscontext.scenariourl;
    scenariolst  = scenarioUR.split('/');
    scenario = scenariolst[scenariolst.length-1];
    callback();
    });

}
/** creates a single marker and places it on the google map
 * @param {List} lst of generators at that location
 */
function createMarker(lst){
    var marker = new google.maps.Marker({
        position: new google.maps.LatLng(lst[1], lst[2]),
        map: map,
        title: lst[0],
        icon: lst[3]
      });
    marker.addListener('click', function(){
        var content = setMarkerMen(lst[0],
            function (_content) {
            console.log('content',_content);
            infowindow.setContent(_content);
            infowindow.setPosition(new google.maps.LatLng(lst[1], lst[2]));
            infowindow.open(map);
        });
    });
    markers.push(marker);
}
/** constructs and calls upon openWindow for foodcourts and 
 * 
 * @param {String} id iri of line
 * @param {function} callback displays content of infowindow as set in drawLines in PopupMap
 */
function setMarkerMen(id, callback){
    if (id.includes("FoodCourt")){
        typeInfo = FCQuery;
    }else{
        typeInfo = WTQuery;
    }
    var promise1 = new Promise(function (resolve, reject){
        resolve(openWindow(id, typeInfo, callback));
    }); 
    promise1.catch(alert);
}
/** opens infowindow for lines and ebuses. 
 * 
 * @param {String} id iri of object
 * @param {String} typeInfo sparql query of respective object
 * @param {Function} callback to display content as given by innerHTML
 */
function openWindow(id, typeInfo, callback){ //gen has its own openWindow cos it's too large. 
    var kmlurl = createUrlForSparqlQuery(scenario, id.split('#')[0], typeInfo);
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
        if (callback == null){
                innerHTML = '<table data-type="line" data-url='+ id +' id="inputsTable">' + inputsHTML + '</table><br/>'+
                        '<img id="myProgressBar" style="width:100px;height:100px;display:none" src="https://media.giphy.com/media/3oEjI6SIIHBdRxXI40/giphy.gif"/><br/>';
                infowindow.setContent(innerHTML);
            }
            
        else{ //there's a callback that exists
                const newPromise = new Promise((resolve, reject) => {
                    resolve('Success');
            });
                newPromise.then((successMessage) => {
                    innerHTML = '<table data-type="line" data-url='+ id +' id="inputsTable">' + inputsHTML + '</table><br/><button onclick="SubmitTable(this)">OPF</button>'+
                        '<img id="myProgressBar" style="width:100px;height:100px;display:none" src="https://media.giphy.com/media/3oEjI6SIIHBdRxXI40/giphy.gif"/><br/>';
                    console.log(innerHTML);
                    callback(innerHTML); //returning back to the infowindow to change. 
                });
            }

    });

}
function getInputs(nameOfTable) {


    let sourceQuan = [];
    $(nameOfTable).each(function () {
        var row = $(this);
        let cols = row.find("input");
        if(cols.length<1){
            return;
        }
        sourceQuan.push($(cols[0]).val())

    })

    

    return sourceQuan;
}
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
/** accesses parallel scenarios through these helper functions
 * @param scenarioname the name of the scenario, be it base or specific folder 
 * @param iri: iri of resource to be queried. 
 * @param sparql: the sparql update to be fired
 * @returns modified url for query
 */
function createUrlForSparqlUpdate(scenarioname, iri, sparql) {

    var url2 = prefix + '/jps/scenario/' + scenarioname + '/update?query=';
    urljson = {"scenarioresource":iri,"sparqlupdate":sparql};
    url2 += encodeURIComponent(JSON.stringify(urljson)); 
    //url2 += JSON.stringify(urljson); 
    return url2;    
}
/*** accesses parallel scenarios through these helper functions
 * @param scenarioname the name of the scenario, be it base or specific folder 
 * @param iri: iri of resource to be queried. 
 * @param sparql: the sparql query to be fired
 * @returns modified url for update
 */
function createUrlForSparqlQuery(scenarioname, iri, sparql) {

    var url2 = prefix + '/jps/scenario/' + scenarioname + '/query?query=';
    urljson = {"scenarioresource":iri,"sparqlquery":sparql};
    url2 += encodeURIComponent(JSON.stringify(urljson)); 
    //url2 += JSON.stringify(urljson); 
    return url2;    
}
/*** accesses parallel scenarios through these helper functions
 * @param scenarioname the name of the scenario, be it base or specific folder 
 * @param agenturl: GET request to Java Backend Servlet
 * @param sparql: JSON packets or what not that the Java backend could request. 
 * @returns modified url for update
 */
function createUrlForAgent(scenarioname, agenturl, agentparams) {

    var url;
    if ((scenarioname == null) || scenarioname == "base") {
        url = agenturl;
    } else {
        agentparams['scenarioagentoperation'] = agenturl;
        var scenariourl = prefix + '/jps/scenario/' + scenarioname + '/call';
        url = scenariourl;
    }

    return url + "?query=" + encodeURIComponent(JSON.stringify(agentparams));
}

