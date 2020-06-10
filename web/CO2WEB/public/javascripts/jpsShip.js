var prefix = "http://localhost:8080";
var markers = []
var infowindow;
var listOfIRIs = [];
var metaEndpoint = prefix + "/rdf4j-server/repositories/airqualitystation";



var NO2json = [{"X(m)":1.1921157116,"Y(m)":103.7832792019,"NO2":1.758644},{"X(m)":1.2102061519,"Y(m)":103.7832711996,"NO2":2.70028},{"X(m)":1.2282965896,"Y(m)":103.7832630768,"NO2":5.278008},{"X(m)":1.2463870249,"Y(m)":103.7832548333,"NO2":0.04759246},{"X(m)":1.2644774576,"Y(m)":103.7832464692,"NO2":15.56921},{"X(m)":1.2825678877,"Y(m)":103.7832379845,"NO2":20.39414},{"X(m)":1.3006583152,"Y(m)":103.7832293792,"NO2":6.240888},{"X(m)":1.3187487401,"Y(m)":103.7832206532,"NO2":0.1043798},{"X(m)":1.3368391622,"Y(m)":103.7832118066,"NO2":0.0},{"X(m)":1.3549295816,"Y(m)":103.7832028394,"NO2":0.0007031696},{"X(m)":1.1921236463,"Y(m)":103.8012525414,"NO2":0.7650797},{"X(m)":1.210214207,"Y(m)":103.8012446573,"NO2":3.997098},{"X(m)":1.2283047652,"Y(m)":103.8012366543,"NO2":17.96103},{"X(m)":1.2463953209,"Y(m)":103.8012285326,"NO2":31.77187},{"X(m)":1.2644858741,"Y(m)":103.801220292,"NO2":20.07102},{"X(m)":1.2825764247,"Y(m)":103.8012119325,"NO2":3.685356},{"X(m)":1.3006669726,"Y(m)":103.8012034542,"NO2":0.09420924},{"X(m)":1.3187575179,"Y(m)":103.8011948571,"NO2":0.01199669},{"X(m)":1.3368480605,"Y(m)":103.8011861411,"NO2":0.01622258},{"X(m)":1.3549386004,"Y(m)":103.8011773062,"NO2":0.00172995},{"X(m)":1.192131463,"Y(m)":103.8192259997,"NO2":0.001312677},{"X(m)":1.2102221423,"Y(m)":103.8192182337,"NO2":1.878788},{"X(m)":1.2283128192,"Y(m)":103.8192103507,"NO2":16.34919},{"X(m)":1.2464034936,"Y(m)":103.8192023506,"NO2":21.67351},{"X(m)":1.2644941654,"Y(m)":103.8191942335,"NO2":16.75356},{"X(m)":1.2825848346,"Y(m)":103.8191859993,"NO2":4.220244},{"X(m)":1.3006755013,"Y(m)":103.8191776481,"NO2":0.04344807},{"X(m)":1.3187661652,"Y(m)":103.8191691798,"NO2":0.0},{"X(m)":1.3368568265,"Y(m)":103.8191605944,"NO2":0.002501672},{"X(m)":1.354947485,"Y(m)":103.8191518919,"NO2":0.0005850521},{"X(m)":1.1921391617,"Y(m)":103.837199575,"NO2":0.0},{"X(m)":1.2102299579,"Y(m)":103.8371919272,"NO2":0.7992001},{"X(m)":1.2283207516,"Y(m)":103.8371841641,"NO2":2.542465},{"X(m)":1.2464115429,"Y(m)":103.8371762858,"NO2":7.517437},{"X(m)":1.2645023315,"Y(m)":103.8371682921,"NO2":24.09183},{"X(m)":1.2825931176,"Y(m)":103.8371601832,"NO2":8.602631},{"X(m)":1.3006839011,"Y(m)":103.837151959,"NO2":0.04613145},{"X(m)":1.3187746819,"Y(m)":103.8371436195,"NO2":0.0},{"X(m)":1.3368654601,"Y(m)":103.8371351648,"NO2":0.0000590878},{"X(m)":1.3549562355,"Y(m)":103.8371265947,"NO2":0.0001573054},{"X(m)":1.1921467423,"Y(m)":103.8551732656,"NO2":0.0},{"X(m)":1.2102376536,"Y(m)":103.855165736,"NO2":0.05783296},{"X(m)":1.2283285624,"Y(m)":103.8551580928,"NO2":0.619101},{"X(m)":1.2464194687,"Y(m)":103.8551503362,"NO2":0.02089391},{"X(m)":1.2645103725,"Y(m)":103.855142466,"NO2":18.2236},{"X(m)":1.2826012736,"Y(m)":103.8551344824,"NO2":21.83832},{"X(m)":1.3006921722,"Y(m)":103.8551263852,"NO2":2.589856},{"X(m)":1.3187830681,"Y(m)":103.8551181746,"NO2":0.0},{"X(m)":1.3368739613,"Y(m)":103.8551098504,"NO2":0.001974617},{"X(m)":1.3549648518,"Y(m)":103.8551014128,"NO2":0.0002526548},{"X(m)":1.1921542049,"Y(m)":103.8731470697,"NO2":1.334192},{"X(m)":1.2102452295,"Y(m)":103.8731396582,"NO2":0.7048039},{"X(m)":1.2283362516,"Y(m)":103.873132135,"NO2":2.006457},{"X(m)":1.2464272712,"Y(m)":103.8731245001,"NO2":0.0},{"X(m)":1.2645182882,"Y(m)":103.8731167534,"NO2":37.7027},{"X(m)":1.2826093027,"Y(m)":103.8731088951,"NO2":48.75678},{"X(m)":1.3007003145,"Y(m)":103.873100925,"NO2":5.256254},{"X(m)":1.3187913237,"Y(m)":103.8730928432,"NO2":0.4017018},{"X(m)":1.3368823302,"Y(m)":103.8730846496,"NO2":0.02389445},{"X(m)":1.354973334,"Y(m)":103.8730763444,"NO2":0.0000057832},{"X(m)":1.1921615495,"Y(m)":103.8911209855,"NO2":0.06663989},{"X(m)":1.2102526856,"Y(m)":103.8911136922,"NO2":0.1407793},{"X(m)":1.2283438192,"Y(m)":103.8911062889,"NO2":0.3614089},{"X(m)":1.2464349502,"Y(m)":103.8910987757,"NO2":2.615068},{"X(m)":1.2645260788,"Y(m)":103.8910911525,"NO2":13.91094},{"X(m)":1.2826172047,"Y(m)":103.8910834195,"NO2":35.17267},{"X(m)":1.3007083281,"Y(m)":103.8910755764,"NO2":10.03782},{"X(m)":1.3187994487,"Y(m)":103.8910676235,"NO2":0.07940782},{"X(m)":1.3368905667,"Y(m)":103.8910595606,"NO2":0.001038789},{"X(m)":1.354981682,"Y(m)":103.8910513877,"NO2":0.0},{"X(m)":1.1921687761,"Y(m)":103.9090950112,"NO2":0.0},{"X(m)":1.2102600218,"Y(m)":103.909087836,"NO2":0.04295366},{"X(m)":1.2283512651,"Y(m)":103.9090805527,"NO2":0.0},{"X(m)":1.2464425059,"Y(m)":103.9090731612,"NO2":2.895215},{"X(m)":1.2645337441,"Y(m)":103.9090656616,"NO2":17.67259},{"X(m)":1.2826249798,"Y(m)":103.9090580538,"NO2":26.59106},{"X(m)":1.3007162128,"Y(m)":103.9090503378,"NO2":16.80966},{"X(m)":1.3188074432,"Y(m)":103.9090425137,"NO2":2.274314},{"X(m)":1.3368986709,"Y(m)":103.9090345814,"NO2":0.008024883},{"X(m)":1.3549898959,"Y(m)":103.909026541,"NO2":0.0},{"X(m)":1.1921758846,"Y(m)":103.9270691451,"NO2":0.0},{"X(m)":1.2102672383,"Y(m)":103.9270620881,"NO2":0.0002838454},{"X(m)":1.2283585894,"Y(m)":103.9270549247,"NO2":0.6349015},{"X(m)":1.2464499381,"Y(m)":103.9270476549,"NO2":0.001335126},{"X(m)":1.2645412842,"Y(m)":103.9270402788,"NO2":14.26682},{"X(m)":1.2826326278,"Y(m)":103.9270327963,"NO2":19.74097},{"X(m)":1.3007239688,"Y(m)":103.9270252074,"NO2":16.98232},{"X(m)":1.3188153071,"Y(m)":103.9270175121,"NO2":2.22069},{"X(m)":1.3369066427,"Y(m)":103.9270097105,"NO2":0.0},{"X(m)":1.3549979756,"Y(m)":103.9270018025,"NO2":0.005256226},{"X(m)":1.1921828751,"Y(m)":103.9450433853,"NO2":0.003944732},{"X(m)":1.2102743348,"Y(m)":103.9450364464,"NO2":0.0003045356},{"X(m)":1.2283657921,"Y(m)":103.945029403,"NO2":1.304565},{"X(m)":1.2464572469,"Y(m)":103.945022255,"NO2":0.2675311},{"X(m)":1.2645486992,"Y(m)":103.9450150024,"NO2":2.949056},{"X(m)":1.2826401489,"Y(m)":103.9450076452,"NO2":14.81825},{"X(m)":1.3007315959,"Y(m)":103.9450001834,"NO2":49.53163},{"X(m)":1.3188230403,"Y(m)":103.9449926169,"NO2":15.88286},{"X(m)":1.3369144821,"Y(m)":103.9449849459,"NO2":0.9250311},{"X(m)":1.3550059211,"Y(m)":103.9449771703,"NO2":0.005183576}];
//first call to initMap. Determine center of map by url

function initMap() {
    //array of pathName
    var arrUrl = window.location.pathname.split('/');
    var location = arrUrl[3];
    var center;
    map = new google.maps.Map(document.getElementById('map'));
    if (location.toLowerCase() == "singapore"){
        center = new google.maps.LatLng(1.367165198,103.801163462);
        map.setZoom(10);
        getRelevantFolder(arrUrl[2], "Singapore");
    } else if (location.toLowerCase() == "hongkong"){
        center = new google.maps.LatLng(22.28911086466781,114.1491155592187);
        map.setZoom(16);
        getRelevantFolder(arrUrl[2], "Hong_Kong");
    }
    map.setCenter(center);
    
  }
function getRelevantFolder(typeOfEmission, city){
    var locationIRI = "http://dbpedia.org/resource/"+city;
    var agentScenario = prefix +  "/JPS_DISPERSION/" + typeOfEmission + "/results/latest";
    document.getElementById("loader").style.display = "block"; 
    //Part 1: get relevant folder
    $.get(agentScenario, {city:locationIRI}).done(function (data) {
        console.log('requested Scenario Agent for folder: '+data);
    }).then(function(data){
        var agentInfo = prefix +  "/JPS_SHIP/GetExtraInfo";
        //Part 2: get the relevant IRIs for ship, as well as for airStationIRIs
        $.get(agentInfo, {path:data}).done(function (data) {
            var info=JSON.parse(data);
            // var shipsIRI = info.ship; 
            // console.log(shipsIRI);
            querySensor(city, function (sensorData) {
                renderSensorStations(sensorData);
            });
            document.getElementById("loader").style.display = "none"; 
        })

    })
}
function renderSensorStations(sensorLocs) {
    //TODO: mock data
    for (let sIRI of sensorLocs){
        createMarker(sIRI);
    }
}

/** creates a single marker and places it on the google map
 * @param {List} lst of generators at that location
 */
function createMarker(lst){
    console.log(lst[2], lst[1]);
    var marker = new google.maps.Marker({
        position: new google.maps.LatLng(lst[2], lst[1]),
        map: map,
        title: lst[0]
      });
    marker.addListener('click', function(){
        querySensorAttributes(lst[0], function (err, sensorAttributes) {
            if (err){console.log(err)}
            document.getElementById("loader").style.display = "block"; 
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
    });
    markers.push(marker);
}
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
    
    document.getElementById("loader").style.display = "none"; 
}
function querySensor(city, callback){
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
            let result =queryProcessor(msg).data
            let search = []
            for (let item of result){
                console.log(item[0])
                if(item[0].includes(city.toLowerCase())){
                    search.push(item);
                }
            }
            callback(search)
        }).fail(function(){
            alert("Search Query failed!" );
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
// select appropriate gas emission sample
function getPoints() {
    console.log("get points");
    var lotsOfMarkers = [];
    var size = NO2json.length;
    for (var i = 0; i < size; i++) {
        var random = {location: new google.maps.LatLng(NO2json[i]["X(m)"],NO2json[i]["Y(m)"]),
        weight: NO2json[i]["NO2"] } ;
            lotsOfMarkers.push(random);
    }

  return lotsOfMarkers;
}
function changeRadius(numeral) {
    heatmap.set('radius', heatmap.get('radius') ? null : numeral);
}
function getLegends(){

    var container = d3.select("#chart");
    var colourScale = d3
        .scaleSequential(d3.interpolateRdYlGn)
        .domain([46,0]);
    var domain = colourScale.domain();
    
    var width = 100;
    var height = 500;
    var  paddedDomain = fc.extentLinear()
    .pad([0.05, 0.05])
    .padUnit("percent")(domain);
  var [min, max] = paddedDomain;
  var expandedDomain = d3.range(min, max, (max - min) / height);
    var xScale = d3
        .scaleBand()
        .domain([0, 1])
        .range([0, width]);
    
    var yScale = d3
        .scaleLinear()
        .domain(paddedDomain)
        .range([height, 0]);
    
    var svgBar = fc
      .autoBandwidth(fc.seriesSvgBar())
      .xScale(xScale)
      .yScale(yScale)
      .crossValue(0)
      .baseValue((_, i) => (i > 0 ? expandedDomain[i - 1] : 0))
      .mainValue(d => d)
      .decorate(selection => {
        selection.selectAll("path").style("fill", d => colourScale(d));
      });
    
    var axisLabel = fc
      .axisRight(yScale)
      .tickValues([...domain, (domain[1] + domain[0]) / 2,
       (domain[1] + domain[0]) / 5, (domain[1] + domain[0]) / 5*2,
       (domain[1] + domain[0]) / 5*3,(domain[1] + domain[0]) / 5*4 ]);
    
    var legendSvg = container.append("svg")
        .attr("height", height)
        .attr("width", width);
    
    var legendBar = legendSvg
        .append("g")
        .datum(expandedDomain)
        .call(svgBar);
    
    var barWidth = Math.abs(legendBar.node().getBoundingClientRect().x);
    legendSvg.append("g")
        .attr("transform", `translate(${barWidth})`)
      .datum(expandedDomain)
      .call(axisLabel)
      .select(".domain")
      .attr("visibility", "hidden");
    
    container.style("margin", "1em");
}

 
/** sleep function for javascript
 * 
 * @param {Integer} ms time in miliseconds 
 */
function sleep(ms) {
    return new Promise(resolve => setTimeout(resolve, ms));
}
/**async function that would provide five minutes to complete simulation and then run callback function
 * @param {Function} callback
 */
async function delayedCallback(callback) 
    {
    var dt = Date();
    console.log("Wait for simulation to finish: "+dt);
    await sleep(300*1000);//five minutes?
    dt = Date();
    console.log("Check callback "+dt);
    callback();
  }

/** clears all markers on the page
 * 
 */
function clearMarkers() {
    if(!markers){
        return;
    }
    for(marker of markers){
        marker.setMap(null);
        marker=null;
    }
}



/** constructs and calls upon openWindow for foodcourts and 
 * 
 * @param {String} id iri of line
 * @param {function} callback displays content of infowindow as set in drawLines in PopupMap
 */
function setMarkerMen(id, callback){
    if (id.includes("FoodCourt")){
        typeInfo = FCQuery;
    }else if (id.includes("OnSite")){
        typeInfo = OnWQuery;   
    }else{
        typeInfo = WTQuery;
    }
    var promise1 = new Promise(function (resolve, reject){
        resolve(openWindow(id, typeInfo, callback));
    }); 
    promise1.catch(alert);
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

