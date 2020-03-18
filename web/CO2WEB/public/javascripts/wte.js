scenario = "base";
prefix = "http://localhost:8080";
markers = []
var infowindow;
wastenetwork = "http://www.theworldavatar.com/kb/sgp/singapore/wastenetwork/SingaporeWasteSystem.owl#SingaporeWasteSystem";
FCQuery = "PREFIX j1:<http://www.theworldavatar.com/ontology/ontowaste/OntoWaste.owl#> "
+ "PREFIX j2:<http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#> "
+ "PREFIX j3:<http://www.theworldavatar.com/ontology/ontopowsys/PowSysPerformance.owl#> "
+ "PREFIX j4:<http://www.theworldavatar.com/ontology/meta_model/topology/topology.owl#> "
+ "PREFIX j5:<http://www.theworldavatar.com/ontology/ontocape/model/mathematical_model.owl#> "
+ "PREFIX j6:<http://www.w3.org/2006/time#> "
+ "PREFIX j7:<http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/space_and_time/space_and_time_extended.owl#> "
+ "PREFIX j8:<http://www.theworldavatar.com/ontology/ontotransport/OntoTransport.owl#> "
+ "SELECT  ?entity  ?name ?xvalue ?yvalue ?wasteproductionvalue"
+ "WHERE {"
+ "?entity  a j1:FoodCourt ."
+ "?entity   j8:hasName ?name ." 
+ "?entity   j7:hasGISCoordinateSystem ?coorsys ."
+ "?coorsys   j7:hasProjectedCoordinate_x ?x ."
+ "?x   j2:hasValue ?xval ."
+ "?xval   j2:numericalValue ?xvalue ."
+ "?coorsys   j7:hasProjectedCoordinate_y ?y ."
+ "?y   j2:hasValue ?yval ."
+ "?yval   j2:numericalValue ?yvalue ."

+ "?entity   j1:produceWaste ?WP ." 
+ "?WP     j2:hasValue ?vWP ."
+ "?vWP  j2:numericalValue ?wasteproductionvalue ."
 
+ "}";
WTFTechQuery = "PREFIX j1:<http://www.theworldavatar.com/ontology/ontowaste/OntoWaste.owl#> "
+ "PREFIX j2:<htt{{p://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#> "
+ "PREFIX j3:<http://www.theworldavatar.com/ontology/ontopowsys/PowSysPerformance.owl#> "
+ "PREFIX j4:<http://www.theworldavatar.com/ontology/meta_model/topology/topology.owl#> "
+ "PREFIX j5:<http://www.theworldavatar.com/ontology/ontocape/model/mathematical_model.owl#> "
+ "PREFIX j6:<http://www.w3.org/2006/time#> "
+ "PREFIX j7:<http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/space_and_time/space_and_time_extended.owl#> "
+ "PREFIX j8:<http://www.theworldavatar.com/ontology/ontotransport/OntoTransport.owl#> "
+ "SELECT DISTINCT ?pollutiontreatmenttaxvalue ?Tech1Capvalue ?installationcostvalue ?operationcostvalue ?transferrateelectricvalue ?energyconsumptionvalue "
+ "WHERE {" + "?entity  a j1:OffsiteWasteTreatmentFacility ." // specified class declared (off or on)

+ "?entity   j1:useTechnology ?Tech1 ." 
//+ "?Tech1 a "+techclass+" ." // specified class declared (tech 1,2,3, or 4,5,6)
+ "?Tech1 j1:hasTechnologyCapacity ?Tech1Cap ." 
+ "?Tech1Cap j2:hasValue ?vTech1Cap ."
+ "?vTech1Cap  j2:numericalValue ?Tech1Capvalue ."

+ "?Tech1   j1:hasTax ?PTT ." + "?PTT     j2:hasValue ?vPTT ."
+ "?vPTT  j2:numericalValue ?pollutiontreatmenttaxvalue ."

+ "?Tech1   j3:hasCost ?OC ." + "?OC     j2:hasValue ?vOC ."
+ "?vOC  j2:numericalValue ?operationcostvalue ."

+ "?Tech1   j3:hasInstallationCost ?IC ." + "?IC     j2:hasValue ?vIC ."
+ "?vIC  j2:numericalValue ?installationcostvalue ."

+ "?Tech1   j1:hasTransferRate ?TR3 ."
+ "?TR3     j1:obtainedFrom <http://www.theworldavatar.com/ontology/ontowaste/OntoWaste.owl#electricity> ."
+ "?TR3     j2:hasValue ?vTR3 ." + "?vTR3  j2:numericalValue ?transferrateelectricvalue ."

+ "?Tech1   j1:requiredConsumption ?RC2 ."
+ "?RC2     j1:inContextOf <http://www.theworldavatar.com/ontology/ontowaste/OntoWaste.owl#energy> ."
+ "?RC2     j2:hasValue ?vRC2 ." + "?vRC2  j2:numericalValue ?energyconsumptionvalue ."

+ "}"
+ "ORDER BY DESC(?Tech1)";
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
    var markerdict = []
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
            infowindow.open(map, this);
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
        typeInfo = WTFTechQuery;
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
                innerHTML = '<table data-type="line" data-url='+ id +' id="inputsTable">' + inputsHTML + '</table><br/><button onclick="SubmitTable(this)">OPF</button>'+
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

