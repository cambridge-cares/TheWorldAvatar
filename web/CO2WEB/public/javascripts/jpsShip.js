// var prefix = "http://localhost:8080";
var prefix = "http://www.jparksimulator.com";
var markers = []
var listOfIRIs = [];
var metaEndpoint = prefix + "/rdf4j-server/repositories/airqualitystation";
var heatmap = null;
var arrXYPollutant;


var slider = document.getElementById("myRange");
var output = document.getElementById("demo");
output.innerHTML = slider.value;

slider.onmouseup = function() { //previously oninput
    output.innerHTML = this.value;
    addheatmap();
}
$("#gasType").on("change", () => {
    console.log("changed in Gas Pollutant");
    addheatmap();
});
$("#location").on("change", () => {
    const mlocation = $("#location option:selected").text();
    var center;
    var arrUrl = window.location.pathname.split('/');
    if (mlocation === "Singapore") {
        center = new google.maps.LatLng(1.272061426648693, 103.86814522217725);
        map.setZoom(10);
         map.set('maxZoom', 11);
        getRelevantFolder(arrUrl[2], "Singapore");

    }else if (mlocation === "Hong Kong") {
        center = new google.maps.LatLng(22.28911086466781,114.1491155592187);
        map.setZoom(10);
         map.set('maxZoom', 11);
        getRelevantFolder(arrUrl[2], "Hong_Kong");
    }
    // var legend = document.createElement("legend");
    // legend.innerHTML = "<h3>Legend</h3>"; 
    // var chart = document.createElement("chart");
    // legend.appendChild(chart);
    map.setCenter(center);
    // map.controls[google.maps.ControlPosition.LEFT_TOP].push(legend);
    
});
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
    var arrUrl = window.location.pathname.split('/');
    var readmeButtonElement = document.getElementById('readme-button'),
        readmeTextElement = document.getElementById('readme-text'),
        targetElement = evt.target;  // clicked element

    if (targetElement == readmeButtonElement || targetElement == readmeTextElement) {
        return; //readme-button or readme-text is clicked. do nothing.
    }

    if(readmeTextElement.style.display === 'block') {
        readmeTextElement.style.display = 'none';
    }
});//first call to initMap. Determine center of map by url

function initMap() {
    //array of pathName
    var arrUrl = window.location.pathname.split('/');
    var center;
    map = new google.maps.Map(document.getElementById('map'));
    center = new google.maps.LatLng(1.272061426648693, 103.86814522217725);
    map.setZoom(10);
    getRelevantFolder(arrUrl[2], "Singapore");
    map.setCenter(center);
    var legend = document.getElementById('legend');

    map.controls[google.maps.ControlPosition.LEFT_TOP].push(legend);

    
  }
function getRelevantFolder(typeOfEmission, city){
    var locationIRI = "http://dbpedia.org/resource/"+city;
    var agentScenario = prefix +  "/JPS_DISPERSION/" + typeOfEmission + "/results/latest";
    document.getElementById("loader").style.display = "block"; 
    //Part 1: get relevant folder
    console.log(agentScenario);
    $.get(agentScenario, {city:locationIRI}).done(function (data) {
        console.log('requested Scenario Agent for folder: '+data);
    }).then(function(data){
        console.log(prefix);
        var agentInfo = prefix +  "/JPS_SHIP/GetExtraInfo";
        // Part 2: get the relevant IRIs for ship, as well as for airStationIRIs
        console.log(agentInfo)
        $.get(agentInfo, {path:data}).done(function (data) {
            var info=JSON.parse(data);
            //Part 3: Handle Ships if they are there
            var shipsIRI = info.ship.collection.items; 
            placeShips(shipsIRI);
            if (city == "Hong_Kong"){
                city = "hongkong";
            }
            querySensor(city, function (sensorData) {
                renderSensorStations(sensorData);
            });
        })
        let agentInformation = prefix + "/JPS/ADMSOutputAllForShips";//"/info"
        console.log(agentInformation);
        $.get(agentInformation, {folder:data}).done(function (info) {
            
            document.getElementById("loader").style.display = "block"; 
            setUpSlider(info.numheight, info.initialheight, info.numinterval);
            console.log(info.listofpol);
            // populateDropDown(info.listofpol);
            populateDropDown();
            var x_coord = [];
            var y_coord = [];
            for (var i = 0; i< info.x_coord.length; i++){
                x_coord.push(parseFloat(info.x_coord[i]));
                y_coord.push(parseFloat(info.y_coord[i]));
            }
            //patch_Job to process info.grid
            var infoGridNew = selectheightGrid(info.grid);
            arrXYPollutant = [x_coord, y_coord, infoGridNew]; //grid = noOfPollutantx(X*Y)
            addheatmap();
            
            if (city == "Hong_Kong"){   
                map.set('maxZoom', 10);
            }else{
                map.set('maxZoom', 11);
            }
            document.getElementById("loader").style.display = "none"; 
        })
    })
}
function setUpSlider(numOfLevel, initHeight, numInterval){
    var slider = document.getElementById("myRange");
    slider.min = initHeight;
    slider.step = numInterval;
    slider.max = initHeight + (numOfLevel-1)* numInterval;
}
function populateDropDown(){
    let dropdown = document.getElementById('gasType');
    dropdown.length = 0;
    var indexes = [ "CO","HCx","NO2","NOx", "O3","SO2","PM2.5", "PM10"];
    dropdown.selectedIndex = 0;
    for (poll in indexes){ 
        option = document.createElement('option');
        option.text = indexes[poll];
        option.value = poll; 
        dropdown.add(option);
    }
}
//realized that its heights x noOfPollutants x 100coordinates
function selectheightGrid(gridArray){
    var newGrid = []
    for(var i = 0; i< 30; i++){
        let newGridElement = selectInfoGrid(gridArray[i]);
        newGrid.push(newGridElement);
    }
    return newGrid;
}
//patch solution passing the noOfPollutants (22) x 100 coordinates
function selectInfoGrid(gridArray){
    //first collate the hydrocarbons C2H6, C2H4,nC4H10,C3H6,Oxylene, and isoprene
    //identify which rows belong to the hydrocarbons
    var hcList = [];
    var noList = [];
    for(var i = 0; i< 100; i++){
        let sumOfNO = parseFloat(gridArray[1][i])+parseFloat(gridArray[2][i]);
        noList.push(sumOfNO);
        let sumOfHC = parseFloat(gridArray[10][i])+parseFloat(gridArray[13][i])+parseFloat(gridArray[15][i])+
        parseFloat(gridArray[17][i])+parseFloat(gridArray[18][i])+parseFloat(gridArray[19][i]);
        hcList.push(sumOfHC);
    }
    return [gridArray[9],hcList,gridArray[2],noList,
    gridArray[0], gridArray[7],  gridArray[20],  gridArray[21] ]
}
function populateDropDownOld(listofpollutants){ //to be used once sensors are re-created
    let dropdown = document.getElementById('gasType');
    dropdown.length = 0;

    // let defaultOption = document.createElement('option');
    // defaultOption.text = 'Choose Pollutant type';

    // dropdown.add(defaultOption);
    dropdown.selectedIndex = 0;
    for (poll in listofpollutants){
        option = document.createElement('option');
        option.text = listofpollutants[poll];
        option.value = poll; //let the index be the value selected
        dropdown.add(option);
    }
}
function addheatmap(){
    
    document.getElementById("loader").style.display = "block"; 
    if (heatmap){
        heatmap.setMap(null);
        heatmap = null;
    }
    heatmap = new google.maps.visualization.HeatmapLayer({
        data: getPollutantAndHeight(), 
        map: map
      });
    var arrUrl = window.location.pathname.split('/');
    if (arrUrl[2].toLowerCase()== "episode" ){
        changeRadius(35);
    }
      document.getElementById("loader").style.display = "none"; 
}
/** supplies data to heatmap in terms of an array of points
 * @returns {[*]} lotsOfMarkers
 */
function getPollutantAndHeight(){
    var arrUrl = window.location.pathname.split('/');
    var firstProjection;
    const mlocation = $("#location option:selected").text();
    if (mlocation.toLowerCase()== "singapore"){
        if (arrUrl[2].toLowerCase()== "adms" ){
            map.setZoom(15);
            firstProjection = "EPSG:3414";
            proj4.defs(firstProjection,
                 "+proj=tmerc +lat_0=1.366666666666667 +lon_0=103.8333333333333 +k=1 +x_0=28001.642 +y_0=38744.572 +ellps=WGS84 +units=m +no_defs ");
        }else{
            firstProjection = "EPSG:32648";
            proj4.defs(firstProjection, "+proj=utm +zone=48 +datum=WGS84 +units=m +no_defs");
        }
    }else{
        if (arrUrl[2].toLowerCase()== "adms" ){
            map.setZoom(15);
            firstProjection = "EPSG:2326";
            proj4.defs(firstProjection,
                "+proj=tmerc +lat_0=22.31213333333334 +lon_0=114.1785555555556 +k=1 +x_0=836694.05 +y_0=819069.8 +ellps=intl +towgs84=-162.619,-276.959,-161.764,0.067753,-2.243649,-1.158827,-1.094246 +units=m +no_defs ");
        }else{
            firstProjection = "EPSG:32650";
            proj4.defs(firstProjection, "+proj=utm +zone=50 +datum=WGS84 +units=m +no_defs");
        }
    }
    var heightLevel = slider.value;
    var heightIndex = (heightLevel -slider.min)/slider.step;
    let dropD= document.getElementById("gasType");
    let pollutantIndex = dropD.options[dropD.selectedIndex].value;
    let sideLength = arrXYPollutant[0].length;
    var typeOfPollutant = Array.from(arrXYPollutant[2][heightIndex][pollutantIndex]);
    const maxNum = Math.max(...typeOfPollutant); //... means [type of Pollutant interator]
    const minNum = Math.min(...typeOfPollutant);
    console.log(maxNum, minNum);
    getLegends([maxNum, minNum]);
    for (var i = 0; i< typeOfPollutant.length; i++){
        var m = parseFloat(typeOfPollutant[i]);
        //normalization from number to 0 - 100 = (m-rmin)/(rmax-rmin)*100
        typeOfPollutant[i] = (m - minNum)/(maxNum-minNum)*100;
    }
    //so length of Gases is a squared integer, pollutant index is an integer
    lotsOfMarkers = [];
    for (var i = 0; i < sideLength; i++) {
        var coordinate = proj4(firstProjection).inverse([arrXYPollutant[0][i],arrXYPollutant[1][i]]);
        var random = {location: new google.maps.LatLng(coordinate[1], coordinate[0]),
        weight: typeOfPollutant[i] } ;
        lotsOfMarkers.push(random);
    }
    if ((maxNum == 0)||(maxNum == NaN)){
        return [];
    }
    return lotsOfMarkers;
}



function placeShips(shipsIRI){
    for (ship of shipsIRI) {
        console.log(ship.lat, ship.lon, ship.name);
        var image = {
            url: 'https://sites.google.com/site/kmlfilescares/jsonsample/ship.png',
            scaledSize : new google.maps.Size(10, 10)
          };
        
        var marker = new google.maps.Marker({
            position: new google.maps.LatLng(ship.lat, ship.lon),
            map: map,
            title: ship.name,
            icon: image, 
            opacity: 0.5
          });
        
          listOfIRIs.push(marker);
      }
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
        title: lst[0], 
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
                name = name[name.length-1];
                name = name.split('.owl')[0];
                item[0] = name;
                let unit = item.splice(-1)[0];
                let unitArr = unit.split('#');
                unit = unitArr.splice(-1);
                item[1] = parseFloat(item[1]).toFixed(2)+' '+unit;
                item[2] = item[2].split('.')[0];
                item[4] = parseFloat(item[4]).toFixed(2)+' '+unit;
                item[5] = parseFloat(item[5]).toFixed(2)+' '+unit;
                item[6] = parseFloat(item[6]).toFixed(2)+' '+unit;
                item[7] = parseFloat(item[7]).toFixed(2);

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
    var qstr;
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
    
    qstr = qstrT.replace('stationIRI', '<'+stationIRI+'>');
    if (stationIRI.includes("002")){
        qstr = qstr.replace('LIMIT10', 'LIMIT9');
    }
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

function changeRadius(numeral) {
    heatmap.setOptions({radius: numeral});
}
/** creates the color range legend using d3 and places it on the panel
 * 
 * @param {[Array]} {maxNum, minNum}
 */
function getLegends(maxMin){
    document.getElementById("chart").innerHTML = "";
    if ((maxMin[0] == 0)||(maxMin[0] == NaN)){
        return;
    }
    var container = d3.select("#chart");
    var colourScale = d3
        .scaleSequential(d3.interpolateRdYlGn) //from the d3 color types
        .domain(maxMin);
    var domain = colourScale.domain();
    
    var width = 80;
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
        .range([height-1, 0]);
    
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
      .tickValues([...domain,
       (domain[1] - domain[0]) / 10+domain[0],(domain[1] - domain[0]) / 5+domain[0],
       (domain[1]- domain[0]) / 10*3+domain[0],(domain[1] - domain[0]) / 5*2+domain[0],(domain[1] - domain[0]) / 2+domain[0],
       (domain[1] - domain[0]) / 5*3+domain[0],(domain[1] - domain[0]) / 10*7+domain[0],
       (domain[1] - domain[0]) / 5*4+domain[0] ,(domain[1]- domain[0]) / 10*9+domain[0]])
       .tickFormat(d3.format(".3g"));
    console.log([...domain,
       (domain[1] - domain[0]) / 10+domain[0],(domain[1] - domain[0]) / 5+domain[0],
       (domain[1]- domain[0]) / 10*3+domain[0],(domain[1] - domain[0]) / 5*2+domain[0],(domain[1] - domain[0]) / 2+domain[0],
       (domain[1] - domain[0]) / 5*3+domain[0],(domain[1] - domain[0]) / 10*7+domain[0],
       (domain[1] - domain[0]) / 5*4+domain[0] ,(domain[1]- domain[0]) / 10*9+domain[0]]);
    
    var legendSvg = container.append("svg")
        .attr("height", height)
        .attr("width", width);
    
    var legendBar = legendSvg
        .append("g")
        .datum(expandedDomain)
        .call(svgBar);
    
    var barWidth = Math.abs(legendBar.node().getBoundingClientRect().x)+17;
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



