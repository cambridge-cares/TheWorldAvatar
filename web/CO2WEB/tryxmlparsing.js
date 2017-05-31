/**
 * Created by Shaocong on 5/31/2017.
 */

//Try libxmljs lib

var libxmljs = require("libxmljs");
var fs = require("fs");


var file = fs.readFileSync("./testFile/JurongIsland.owl");



var xmlDoc = libxmljs.parseXml(file);
var root = xmlDoc.root();

var namespaceOb = {};

namespaceOb["rdf"] = "http://www.w3.org/1999/02/22-rdf-syntax-ns#";
namespaceOb['owl'] = "http://www.w3.org/2002/07/owl#";
var uris = root.find("//rdf:type[contains(@rdf:resource,'WaterNetwork')]", namespaceOb);

console.log(uris.length);
for (let curi of uris) {
    console.log(curi.name());
}

