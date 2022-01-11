let headers = [];

let BuildingListObj = {'key': 'BuildingList'};
let BuildingListQuery = "PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#> \n" +
    "PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>\n" +
    "PREFIX yago: <http://yago-knowledge.org/resource/>\n" +
    "SELECT ?building\n" +
    "WHERE\n" +
    "{\n" +
    "   ?buildingList rdf:type  <http://test.com/ontology/BuildingList> .\n" +
    "   ?buildingList <http://test.com/property/hasBuilding> ?building .\n" +
    "}";
BuildingListObj['type'] = 'array';
BuildingListObj['names'] = ['building'];
BuildingListObj['query'] = BuildingListQuery;

let ADMSObj = {'key': 'ADMS'};
let ADMSQuery = "PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#> \r\n" +
    "PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>\r\n" +
    "PREFIX yago: <http://yago-knowledge.org/resource/>\r\n" +
    "\r\n" +
    "SELECT ?fileDirectory\r\n" +
    "WHERE\r\n" +
    "{\r\n" +
    "  ?x <http://test.com/Property/hasOutputPosition> ?fileDirectory   .\r\n" +
    "}";

ADMSObj['type'] = 'string';
ADMSObj['names'] = ['fileDirectory'];
ADMSObj['query'] = ADMSQuery;




let CityObj = {'key': 'City'};
let CityQuery =  "PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#> \n" +
    "PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>\n" +
    "PREFIX yago: <http://yago-knowledge.org/resource/>\n" +
    "SELECT ?city\n" +
    "WHERE\n" +
    "{\n" +
    "   ?city rdf:type  <http://www.theworldavatar.com/OntoEIP/supporting_concepts/space_and_time_v1.owl#City> .\n" +
    "}";


CityObj['type'] = 'string';
CityObj['names'] = ['city'];
CityObj['query'] = CityQuery;

let RegionObj = {'key': 'Region'};
let RegionQuery =	"PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>\n" +
    "SELECT ?lowerX ?upperX ?lowerY ?upperY ?ref  WHERE \n"
    + "{"
    + "?region <http://test.com/Property/upperPoint> ?upperPoint . \n"
    + "?upperPoint <http://test.com/Property/hasX> ?upperX .\n"
    + "?upperPoint <http://test.com/Property/hasY> ?upperY .\n"
    + "?region <http://test.com/Property/lowerPoint> ?lowerPoint .\n"
    + "?lowerPoint <http://test.com/Property/hasX> ?lowerX .\n"
    + "?lowerPoint <http://test.com/Property/hasY> ?lowerY .\n"
    + "?region <http://test.com/Property/referenceSystem> ?ref .\n"
    + "}";

RegionObj['type'] = 'map';
RegionObj['names'] = ['lowerX','upperX','lowerY','upperY','ref'];
RegionObj['query'] = RegionQuery;



headers.push(BuildingListObj);
headers.push(ADMSObj);
headers.push(CityObj);
headers.push(RegionObj);

const regionTemplate = '{ \n' +
    '  "http://test.com/lowerPoint" : { \n' +
    '    "http://test.com/Property/hasY" : [ { \n' +
    '      "type" : "literal" ,\n' +
    '      "value" : "{0}" ,\n' +
    '      "datatype" : "http://www.w3.org/2001/XMLSchema#string"\n' +
    '    }\n' +
    '     ] ,\n' +
    '    "http://www.w3.org/1999/02/22-rdf-syntax-ns#type" : [ { \n' +
    '      "type" : "literal" ,\n' +
    '      "value" : "http://test.com/ontology/Point"\n' +
    '    }\n' +
    '     ] ,\n' +
    '    "http://test.com/Property/hasX" : [ { \n' +
    '      "type" : "literal" ,\n' +
    '      "value" : "{1}" ,\n' +
    '      "datatype" : "http://www.w3.org/2001/XMLSchema#string"\n' +
    '    }\n' +
    '     ]\n' +
    '  }\n' +
    '   ,\n' +
    '  "http://test.com/upperPoint" : { \n' +
    '    "http://test.com/Property/hasY" : [ { \n' +
    '      "type" : "literal" ,\n' +
    '      "value" : "{2}" ,\n' +
    '      "datatype" : "http://www.w3.org/2001/XMLSchema#string"\n' +
    '    }\n' +
    '     ] ,\n' +
    '    "http://www.w3.org/1999/02/22-rdf-syntax-ns#type" : [ { \n' +
    '      "type" : "literal" ,\n' +
    '      "value" : "http://test.com/ontology/Point"\n' +
    '    }\n' +
    '     ] ,\n' +
    '    "http://test.com/Property/hasX" : [ { \n' +
    '      "type" : "literal" ,\n' +
    '      "value" : "{3}" ,\n' +
    '      "datatype" : "http://www.w3.org/2001/XMLSchema#string"\n' +
    '    }\n' +
    '     ]\n' +
    '  }\n' +
    '   ,\n' +
    '  "http://test.com/aRegionInstance" : { \n' +
    '    "http://test.com/Property/upperPoint" : [ { \n' +
    '      "type" : "uri" ,\n' +
    '      "value" : "http://test.com/upperPoint"\n' +
    '    }\n' +
    '     ] ,\n' +
    '    "http://test.com/Property/lowerPoint" : [ { \n' +
    '      "type" : "uri" ,\n' +
    '      "value" : "http://test.com/lowerPoint"\n' +
    '    }\n' +
    '     ] ,\n' +
    '    "http://www.w3.org/1999/02/22-rdf-syntax-ns#type" : [ { \n' +
    '      "type" : "literal" ,\n' +
    '      "value" : "http://test.com/ontology/Region"\n' +
    '    }\n' +
    '     ] ,\n' +
    '    "http://test.com/Property/referenceSystem" : [ { \n' +
    '      "type" : "literal" ,\n' +
    '      "value" : "EPSG:4326"\n' +
    '    }\n' +
    '     ]\n' +
    '  }\n' +
    '}';


function getParameter() {
    var url_string = window.location.href;
    var url = new URL(url_string.replace(/#/g, '@'));
    var value = url.searchParams.get("value");
    return value.replace(/@/g,'#')
}

function generateSemanticRegion(Region){
    //y_min, x_min, y_max, x_max
    console.log('Region 2 : ',Region);
    console.log('Region Type : ',typeof(Region));

    let RegionObj = JSON.parse(Region);

    return regionTemplate.format([RegionObj['lowerY'], RegionObj['lowerX'], RegionObj['upperY'], RegionObj['upperX']]);
}


function sendRequest(model, header) {
    var myUrl = 'http://localhost:8080/JPS_COMPOSITION/LocalQueryEndPoint?rdfModel=' + encodeURIComponent(model) + '&headers=' + encodeURIComponent(JSON.stringify(header));
    var request = $.ajax({
        url: myUrl,
        type: 'GET',
        contentType: 'application/json; charset=utf-8'
    });

    request.done(function (data) {
//        console.log(data);
        let dataInJSON = JSON.parse(data);
        let BuildingList = JSON.parse(dataInJSON['BuildingList']);
        console.log(BuildingList);
        let ADMS = dataInJSON['ADMS'];
        console.log(ADMS);
        let City = dataInJSON['City'];
        console.log(City);
        let Region = dataInJSON['Region'];
        console.log('Region: \n', Region);
        convertCoordinates(generateSemanticRegion(Region));
        //document.getElementById("text").innerText = JSON.stringify(BuildingList) + "\n" +  JSON.stringify(ADMS) + "\n" + JSON.stringify(City) + "\n" + JSON.stringify(Region);


        document.getElementById("text").innerText = BuildingList;



    });

    request.fail(function (jqXHR, textStatus) {
        // your failure code here
    });
}


function convertCoordinates(coor){
    var myUrl = 'http://localhost:8080/JPS/CoordianteRefConvertor?value=' + encodeURIComponent(coor);
    var request = $.ajax({
        url: myUrl,
        type: 'GET',
        contentType: 'application/json; charset=utf-8'
    });
    
    request.done(function (data) {
        console.log(JSON.parse(data));
        return JSON.parse(data);
    });
    
    request.fail(function (error) {
        
    })
}


String.prototype.format = function (args) {
    var str = this;
    return str.replace(String.prototype.format.regex, function(item) {
        var intVal = parseInt(item.substring(1, item.length - 1));
        var replace;
        if (intVal >= 0) {
            replace = args[intVal];
        } else if (intVal === -1) {
            replace = "{";
        } else if (intVal === -2) {
            replace = "}";
        } else {
            replace = "";
        }
        return replace;
    });
};
String.prototype.format.regex = new RegExp("{-?[0-9]+}", "g");


let testObj = getParameter();
sendRequest(testObj, headers);



