/**
 * Created by Shaocong on 7/19/2017.
 */
let $rdf = require('rdflib');
let parser = require('../util/rdfParser')({fileUrl:"../testFile/JParkLandLots.owl"});

let util = require('util')

let SPA = `
PREFIX JParkLandLots: <http://www.jparksimulator.com/JParkLandLots#>
    PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
    PREFIX system: <http://www.theworldavatar.com/OntoCAPE/OntoCAPE/upper_level/system.owl#>

    select distinct ?LandLot_CarbonEmissions ?ValueOf_CarbonEmissions
    where {
    ?LandLot_CarbonEmissions rdf:type JParkLandLots:CarbonEmissions;
system:hasValue ?x.
    ?x system:numericalValue ?ValueOf_CarbonEmissions.}

order by DESC(?ValueOf_CarbonEmissions)
`;
//console.log(JSON.stringify( parser.graph))

util.inspect(parser.graph)
console.log(JSON.stringify($rdf.SPARQLToQuery(SPA, false, parser.graph)))

parser.graph.query($rdf.SPARQLToQuery(SPA, false, parser.graph), function () {

    for(let i = 0 ; i < arguments.length;i++){
        console.log("!!!!!!!!!!!!!!!!!!!!")
        console.log(arguments[i])
    }
    console.log("done")
});