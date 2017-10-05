/**
 * Created by Shaocong on 9/5/2017.
 */
var parser =require('../agents/rdfParser'),
fs = require('fs'),
    resolve = require('path').resolve;
var uri = resolve("../testFiles/E-301.owl");




var qsTypeCountry = `
    PREFIX system_realization: <http://www.theworldavatar.com/OntoEIP/system_aspects/system_realization.owl#>
        PREFIX system_v1: <http://www.theworldavatar.com/OntoEIP/upper_level/system_v1.owl#>
     PREFIX j.0: <http://www.theworldavatar.com/OntoEIP/OntoCAPE/OntoCAPE/upper_level/technical_system.owl#>
     PREFIX p14: <http://www.theworldavatar.com/OntoEIP/OntoEN/power_plant.owl#>
          PREFIX p9: <http://www.theworldavatar.com/OntoEIP/supporting_concepts/space_and_time_v1.owl#>

        select distinct ?Power_Plant_Name ?Fuel_Type ?Country
        where {
        ?Power_Plant_Name a p14:PowerGenerator;
    j.0:realizes ?Fuel_Type.

        ?Power_Plant_Name a p14:PowerGenerator;
    p9:hasAddress ?x.
        ?x system_v1:hasName ?Country.
}`;
var qsCapacity = `    PREFIX system_realization: <http://www.theworldavatar.com/OntoEIP/system_aspects/system_realization.owl#>         
         PREFIX j.2: <http://www.theworldavatar.com/OntoEIP/OntoCAPE/OntoCAPE/upper_level/system.owl#>

     select distinct ?Power_Plant ?Capacity
       where {                         
         ?Power_Plant a system_realization:DesignCapacity;
         j.2:hasValue ?x.
         ?x j.2:numericalValue ?Capacity.
             }`;



var qsChildren= `
PREFIX industrialPark: <http://www.theworldavatar.com/OntoEIP/Eco-industrialPark.owl#>
    PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
    PREFIX owl: <http://www.w3.org/2002/07/owl#>
    PREFIX system: <http://www.theworldavatar.com/OntoCAPE/OntoCAPE/upper_level/system.owl#>
    PREFIX Service: <http://www.theworldavatar.com/Service.owl#>
   select distinct ?X ?myUri
    where {
    ?X industrialPark:hasIRI ?myUri.    
    }
`;
//    ?valueE rdf:type owl:NamedIndividual.


var qsV = `
    PREFIX system: <http://www.theworldavatar.com/OntoCAPE/OntoCAPE/upper_level/system.owl#>
       PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
    PREFIX owl: <http://www.w3.org/2002/07/owl#>
   select distinct  ?valueE ?value
    where {
    ?valueE a owl:NamedIndividual.
    ?valueE  system:numericalValue ?value.
    }
`;

var qsUri = `
    PREFIX owl: <http://www.w3.org/2002/07/owl#>
   select distinct  ?valueE 
    where {
    ?valueE a owl:Ontology.
    }
`;

const qsImport = `
    PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
    PREFIX owl: <http://www.w3.org/2002/07/owl#>
   select distinct  ?import
    where {
         ?x a owl:Ontology.
        ?x owl:imports ?import.    
        
    }
    `;
try{
    var file = fs.readFileSync(uri);
    var mparser = new parser.RdfParser({uri: uri, file : file})
    mparser.mquery(qsImport, function (err, data) {
        if(err){
            throw err;
        }
        console.log(  data)


    });
}catch (err){
    throw err;
}




/**
parserO.query(qsTypeCountry, function (err, data) {
    if(err){
        throw err;
    }
    console.log(    JSON.stringify(data)
    )
});
*/