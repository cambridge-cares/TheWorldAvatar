const epQueryer = require("./epQueryWrapper");
const config = require('../config');
const aQueryer = Object.create(epQueryer);

const queryStr = `PREFIX powerplant: <http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#>
PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
PREFIX coordinate: <http://www.theworldavatar.com/ontology/ontocape/upper_level/coordinate_system.owl#>
PREFIX system: <http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#>
PREFIX space_and_time_extended: <http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/space_and_time/space_and_time_extended.owl#>
PREFIX technical_system: <http://www.theworldavatar.com/ontology/ontocape/upper_level/technical_system.owl#>
PREFIX system_performance: <http://www.theworldavatar.com/ontology/ontoeip/system_aspects/system_performance.owl#>
PREFIX system_v1: <http://www.theworldavatar.com/ontology/ontoeip/upper_level/system_v1.owl#>
PREFIX system_realization: <http://www.theworldavatar.com/ontology/ontoeip/system_aspects/system_realization.owl#>

SELECT DISTINCT ?powerPlantIRI ?numericalValue_x ?numericalValue_y 
WHERE
{

?powerPlantIRI space_and_time_extended:hasGISCoordinateSystem ?CoordinateSystem .
?CoordinateSystem  space_and_time_extended:hasProjectedCoordinate_x ?x_coordinate .
?CoordinateSystem  space_and_time_extended:hasProjectedCoordinate_y ?y_coordinate .
?x_coordinate  system:hasValue ?value_x_coordinate .
?y_coordinate  system:hasValue ?value_y_coordinate . 
?value_x_coordinate  system:numericalValue ?numericalValue_x .
?value_y_coordinate  system:numericalValue ?numericalValue_y . 

}`;



function getEnglandPPcoordinates(callback) {
    aQueryer.queryPromise(queryStr, config.comoRDF4j) // the config.localRDF4j is the address of the endpoint
        .then((result)=>{
            let coordList = [];
            for(line of result){
                let uri = line['powerPlantIRI'];//here in the[] is your name in query represents uri
                let Latitute = line['numericalValue_y'];//here in the[] is your name in query represents Latitute
                let Longitude = line['numericalValue_x'];//here in the[] is your name in query represents Longitude

                coordList.push({
                    uri: uri, location: { Latitute: parseFloat(Latitute), Longitude: parseFloat(Longitude) }
                });
            }

            callback(null, coordList);
        })
        .catch((err)=>{callback(err)})


}
module.exports= getEnglandPPcoordinates;