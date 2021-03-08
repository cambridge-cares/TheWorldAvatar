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

SELECT DISTINCT ?powerPlantIRI ?numericalValue_x ?numericalValue_y ?Built_year ?owned_company ?Country ?value_of_Designed_Capacity ?Generation_Type 
WHERE
{
?powerPlantIRI rdf:type powerplant:PowerPlant .

?powerPlantIRI technical_system:realizes ?Generation_Type .

?powerPlantIRI powerplant:hasYearOfBuilt ?hasYear .
?hasYear system:hasValue ?value_year .
?value_year system:numericalValue ?Built_year .
?powerPlantIRI space_and_time_extended:hasGISCoordinateSystem ?CoordinateSystem .
?CoordinateSystem  space_and_time_extended:hasProjectedCoordinate_x ?x_coordinate .
?CoordinateSystem  space_and_time_extended:hasProjectedCoordinate_y ?y_coordinate .
?x_coordinate  system:hasValue ?value_x_coordinate .
?y_coordinate  system:hasValue ?value_y_coordinate . 
?value_x_coordinate  system:numericalValue ?numericalValue_x .
?value_y_coordinate  system:numericalValue ?numericalValue_y . 

?powerPlantIRI system_v1:isOwnedBy ?owner .
?owner system_v1:hasName ?owned_company .

?powerPlantIRI system:hasAddress ?Country .

?powerPlantIRI system_realization:designCapacity ?Capacity .
?Capacity system:hasValue ?Capacity_value .
?Capacity_value system:numericalValue ?value_of_Designed_Capacity .

}`;



function getEnglandPPcoordinates(callback) {
    aQueryer.queryPromise(queryStr, config.RDF4j) // the config.localRDF4j is the address of the endpoint
        .then((result) => {
            let coordList = [];
            for (line of result) {
                let uri = line['powerPlantIRI'];//here in the[] is your name in query represents uri
                let Latitude = line['numericalValue_y'];//here in the[] is your name in query represents Latitude
                let Longitude = line['numericalValue_x'];//here in the[] is your name in query represents Longitude
                let built_year = line['Built_year'];
                let Owner = line['owned_company'];
                let Country = line['Country'];
                let Designed_Capacity = line['value_of_Designed_Capacity'];
                let Generation_Technology = line['Generation_Type'];


                coordList.push({
                    uri: uri, location: { Latitude: parseFloat(Latitude), Longitude: parseFloat(Longitude) }, built_year: built_year, Owner: Owner, Country: Country,
                    Designed_Capacity: parseFloat(Designed_Capacity), Generation_Technology: Generation_Technology
                    
                });
            }

            callback(null, coordList);
        })
        .catch((err) => { callback(err) })


}
module.exports = getEnglandPPcoordinates;