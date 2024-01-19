// This Agent enables selecting an area on the google map
// Then generates region in the form of rdf model



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

const newRegion = '{"region":{"lowercorner":{"lowerx":"{1}","lowery":"{0}"},"uppercorner":{"upperx":"{3}","uppery":"{2}"},"srsname":"EPSG:4326"}}';


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

function generateSemanticRegion(y_min,x_min,y_max,x_max){
    return newRegion.format([y_min, x_min, y_max, x_max]);
}


