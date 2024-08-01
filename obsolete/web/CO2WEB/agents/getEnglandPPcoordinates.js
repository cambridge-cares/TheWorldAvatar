const epQueryer = require("./epQueryWrapper");
const config = require('../config');
const aQueryer = Object.create(epQueryer);

const queryStr = `PREFIX powerplant: <http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#>
PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
PREFIX coordinate: <http://www.theworldavatar.com/ontology/ontocape/upper_level/coordinate_system.owl#>
PREFIX system: <http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#>
PREFIX space_and_time_extended:<http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/space_and_time/space_and_time_extended.owl#>
SELECT ?powerPlantIRI ?numericalValue_x ?numericalValue_y
WHERE
{
?powerPlantIRI rdf:type powerplant:PowerPlant .
?powerPlantIRI space_and_time_extended:hasGISCoordinateSystem ?CoordinateSystem .
?CoordinateSystem  space_and_time_extended:hasProjectedCoordinate_x ?x_coordinate .
?CoordinateSystem  space_and_time_extended:hasProjectedCoordinate_y ?y_coordinate .
?x_coordinate  system:hasValue ?value_x_coordinate .
?y_coordinate  system:hasValue ?value_y_coordinate . 
?value_x_coordinate  system:numericalValue ?numericalValue_x .
?value_y_coordinate  system:numericalValue ?numericalValue_y . 
}`;


/**
const queryStr = `PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
SELECT * where
{?uri rdf:type <http://dummy>.
?uri <http://x> ?lat.
  ?uri <http://y> ?lng.
}`;
**/

function getEnglandPPcoordinates(callback) {
    aQueryer.queryPromise(queryStr, config.localRDF4j) // the config.localRDF4j is the address of the endpoint
        .then((result)=>{
            let coordList = [];
            for(line of result){
                let uri = line['powerPlantIRI'];//here in the[] is your name in query represents uri
                let lat = line['numericalValue_y'];//here in the[] is your name in query represents latitute
                let lng = line['numericalValue_x'];//here in the[] is your name in query represents latitute
                coordList.push({uri: uri, location :{lat: parseFloat(lat), lng:parseFloat(lng)}});
            }

            callback(null, coordList);
        })
        .catch((err)=>{callback(err)})


}
module.exports= getEnglandPPcoordinates;

/**
//return {uri: uri, location :{lat: parseFloat(item[uri].y), lng:parseFloat(item[uri].x)}}
getEnglandPPcoordinates((err, result)=>{
    if(err) console.log(err);
    console.log(result);

})
**/