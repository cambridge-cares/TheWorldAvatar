/**
 * Created by Shaocong on 9/5/2017.
 */
var uri = "C:/Users/Shaocong/WORK/webJPSGit/irp3-WebJPS-git/CO2WEB/testFiles/powerplants/Collinsville_Coal_Power_Plant_Australia.owl"
var parser =require('../agents/rdfParser'),
fs = require('fs')
var file = fs.readFileSync(uri);
var mparser = new parser.RdfParser({uri: uri, file : file})
var parserO = require('../agents/rdfParserObsolete')({fileUrl:uri})

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



mparser.mquery(qsTypeCountry, function (err, data) {
    if(err){
        throw err;
    }
console.log(    JSON.stringify(data)
)
    //type/ name / capacity
    console.log("extract!!!!!!!!!!!!!!!")
   // console.log(data[0]['?Capacity']['value'])
    console.log(data[0]['?Country']['value'])
    console.log(data[0]['?Fuel_Type']['value'])

});

mparser.geoCoordsQuery(function (err, data) {

    console.log(data)
})

/**
parserO.query(qsTypeCountry, function (err, data) {
    if(err){
        throw err;
    }
    console.log(    JSON.stringify(data)
    )
});
*/