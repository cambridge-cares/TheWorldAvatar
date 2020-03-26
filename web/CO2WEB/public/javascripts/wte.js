scenario = "base";
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


(function PPMap(){
    var ppMap = new PopupMap({useCluster:true});
})();

  //when button clicked, run simulation
$(document).ready(function () {
    InitialTransportInputs();
    InitialUnitInputs();
     inittransportUpd = getInputs("table#transportQ tr");
    var initoffSiteIUpd = getInputs("table#Incineration tr");
    var initoffSiteCUpd = getInputs("table#CoDigestion tr");
    var initoffSiteAUpd = getInputs("table#AnaerobicDigestion tr");
    var initonSite = getInputs("table#onsite tr");
    initArray = [initoffSiteIUpd,initoffSiteCUpd,initoffSiteAUpd,initonSite]
    $("#run-btn").click(function () {
        console.log("Start sim")
        transportUpd = getInputs("table#transportQ tr");
        var offSiteIUpd = getInputs("table#Incineration tr");
        var offSiteCUpd = getInputs("table#CoDigestion tr");
        var offSiteAUpd = getInputs("table#AnaerobicDigestion tr");
        var onSite = getInputs("table#onsite tr");
        finalArray = [offSiteIUpd,offSiteCUpd,offSiteAUpd,onSite]
        if (JSON.stringify(transportUpd) != JSON.stringify(inittransportUpd)){
            dumpTransport(transportUpd);}
        for (i = 0; i<initArray.length; i++){
            if (JSON.stringify(initArray[i]!= JSON.stringify(finalArray[i]))){
                updateSite(finalArray[i]);
            }
        }

    });
});
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
            addUnitInputsDisplay(obj0, "offsite");
            addUnitInputsDisplay(obj1, "onsite");
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
/**
 * 
 * @param {Array} transportArray 
 */
function dumpTransport(transportArray){
    base = transportIRI.split('#')[0];
    var sampleUpdate = []
    lstOfTargetIRI = ["#V_TransportationCapacityOfUnitTruckinTransportSystem-001",
     "#V_TransportCostOfUnitTruckinTransportSystem-001",
      "#V_PollutionTransportTaxOfUnitTruckinTransportSystem-001",
    "#V_EmissionOfUnitTruckinTransportSystem-001"]
    for (i = 0; i<lstOfTargetIRI.length; i++){
        var deleteUpdate = "DELETE WHERE {<" + base + lstOfTargetIRI[i] + "> <http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#numericalValue> " + "?o.}";
        var insertUpdate = "INSERT DATA {<" + base + lstOfTargetIRI[i]+ "> <http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#numericalValue> " +transportArray[i] +".}";
        sampleUpdate.push(deleteUpdate);
        sampleUpdate.push(insertUpdate);
    }
    console.log(scenario);
        console.log(sampleUpdate); 
        var myUrl = createUrlForSparqlUpdate(scenario,base.split('#')[0], sampleUpdate.join(';'));
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
        console.log(myUrl);
        request.done(function(data) {
            console.log('data received', data);
        });
}
/** input parameters are updated
 * 
 * @param {[List]} inpParameters 
 */
function updateSites(inpParameters){
    listOfIRIs.forEach((i)=>{
        if (i.includes("Waste")){
            console.log(i);

        }
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
            + '" style="float: right;"></td></tr>';
            inputsHTML += inputLine;
        }
        )
        if (type=="offsite"){
            type1 = type +" "+typesOfDigestion2[ndex]; 
            type2 =  typesOfDigestion[ndex]; 
            ndex += 1;
        }else{
            type2 = type;
        }
        var table = '<h2>'+type1+'</h2><table data-url='+ wastenetwork
        +' id="' + type2 +'">' + inputsHTML 
        + '</table><br>';
        inputsHTML = "";
        textPanel.append(table);
    });
    
    return;
}

/** once map is instantiated, run base scenario
 * 
 */
var checkExist = setInterval(function() {
    if ($('#map').length) {
        position = new google.maps.LatLng(1.367165198,103.801163462);
        map.setCenter(position);
        map.setZoom(12);
        queryForFCMarkers();
        clearInterval(checkExist);
    }
}, 100); // check every 100ms
/** query backend using greedy method for FC and Offsite as they are already instantiated. 
 *  runs during init
 */
function queryForFCMarkers(){
    infowindow = new google.maps.InfoWindow({
        content: '<h2>Sup!</h2>'
    });
    var agenturl = prefix + '/JPS_WTE/WTEVisualization/createMarkers'; 
    var data = {"wastenetwork": wastenetwork};
    var markerURL = createUrlForAgent(scenario, agenturl, data);
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
        
        listOfIRIs.push(name);
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
        }
        markerdict.push([name, obj.coors.lat,obj.coors.lng, icon]);
    }

    for (x=0; x< size; x++){
        createMarker(markerdict[x]);
    }
  
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
/*** accesses parallel scenarios through these helper functions
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

