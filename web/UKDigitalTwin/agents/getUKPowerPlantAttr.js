const epQueryer = require("./epQueryWrapper");
const config = require('../config');
const aQueryer = Object.create(epQueryer);



// Construct the query string
function queryStrUKPowerPlant(powerPlantIRI, callback) {
    let queryStrPowerPlantAttr = `PREFIX powerplant: <http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#>
PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
PREFIX coordinate: <http://www.theworldavatar.com/ontology/ontocape/upper_level/coordinate_system.owl#>
PREFIX system: <http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#>
PREFIX space_and_time_extended:<http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/space_and_time/space_and_time_extended.owl#>
PREFIX technical_system: <http://www.theworldavatar.com/ontology/ontocape/upper_level/technical_system.owl#>
PREFIX system_performance: <http://www.theworldavatar.com/ontology/ontoeip/system_aspects/system_performance.owl#>
PREFIX system_v1: <http://www.theworldavatar.com/ontology/ontoeip/upper_level/system_v1.owl#>
PREFIX system_realization: <http://www.theworldavatar.com/ontology/ontoeip/system_aspects/system_realization.owl#>
SELECT DISTINCT ?GPS_x ?GPS_y ?gty ?pft ?gt ?vag ?uag ?vco2 ?uco2 ?owned_company ?Built_year ?Country ?value_of_Designed_Capacity ?unit_of_Designed_Capacity
WHERE
{` +

        powerPlantIRI + ` space_and_time_extended: hasGISCoordinateSystem ?CoordinateSystem .
?CoordinateSystem  space_and_time_extended: hasProjectedCoordinate_x ? x_coordinate.
? CoordinateSystem  space_and_time_extended: hasProjectedCoordinate_y ? y_coordinate.
? x_coordinate  system: hasValue ? value_x_coordinate.
? y_coordinate  system: hasValue ? value_y_coordinate. 
? value_x_coordinate  system: numericalValue ?GPS_x .
? value_y_coordinate  system: numericalValue ?GPS_y .` +

        powerPlantIRI + ` rdf:type powerplant:PowerPlant .` +
        powerPlantIRI + ` technical_system:realizes ?gty .

?gty powerplant:hasAnnualGeneration ?AnnualGeneration .
?AnnualGeneration system:hasValue ?AnnualGeneration_value .
?AnnualGeneration_value system:numericalValue ?vag .
?AnnualGeneration_value system:hasUnitOfMeasure ?uag .

?gty powerplant:consumesPrimaryFuel ?pft .
?gty powerplant:usesGenerationTechnology  ?gt .

?gty system_performance:hasEmission ?CO2Emission .
?CO2Emission system:hasValue ?CO2Emission_value .
?CO2Emission_value system:numericalValue ?vco2 .
?CO2Emission_value system:hasUnitOfMeasure ?uco2 .` +

        powerPlantIRI + ` system_v1:isOwnedBy ?owner .
?owner system_v1:hasName ?owned_company .` +

        powerPlantIRI + ` powerplant:hasYearOfBuilt ?hasYear .
?hasYear system:hasValue ?value_year . 
?value_year system:numericalValue ?Built_year .` +

        powerPlantIRI + ` system:hasAddress ?Country .` +

        powerPlantIRI + ` system_realization:designCapacity ?Capacity .
?Capacity system:hasValue ?Capacity_value .
?Capacity_value system:numericalValue ?value_of_Designed_Capacity .
?Capacity_value system:hasUnitOfMeasure ?unit_of_Designed_Capacity .
}`;

    callback(queryStrPowerPlantAttr);
    // return queryStrPowerPlantAttr;
}


// Ruturn the query results
function queryAttrUKPowerPlant(queryStr) {
    let powerPlantAttrList = [];
    aQueryer.queryPromise(queryStr, config.localRDF4j) // the config.localRDF4j is the address of the endpoint
        .then((result) => {
            for (line of result) {
                let latitute = line['GPS_y'];
                let longitude = line['GPS_x'];
                let generation_type = line['gty'];
                let Primary_Fuel_type = line['pft'];
                let Generation_Technology = line['gt'];
                let Annual_Generation = line['vag'];
                let CO2_Emission = line['vco2'];
                let Owner = line['owned_company'];
                let Built_year = line['Built_year'];
                let Country = line['Country'];
                let Designed_Capacity = line['value_of_Designed_Capacity'];

                powerPlantAttrList.push({
                    latitute: latitute, longitude: longitude, generation_type: generation_type, Primary_Fuel_type: Primary_Fuel_type, Generation_Technology: Generation_Technology,
                    Annual_Generation: Annual_Generation, CO2_Emission: CO2_Emission, Owner: Owner, Built_year: Built_year, Country: Country, Designed_Capacity: Designed_Capacity
                });
            }


        });

    return powerPlantAttrList;
}

module.exports = queryStrUKPowerPlant(powerPlantIRI, queryAttrUKPowerPlant);
/**
var powerPlantIRIList = getpowerPlantIRI();
var powerPlantAttrQueryStr = ``;
var powerPlantAttr = [];

for (i = 0; i < powerPlantIRIList.length; i++) {
    powerPlantAttrQueryStr = getPowerPlantAttrQueryStr(powerPlantIRIList[i]);
    powerPlantAttr = getPowerPlantAttr(powerPlantAttrQueryStr);
}



// Retrieve the powerPlantIRI from RDF4j triple store
function getpowerPlantIRI() {
    let powerPlantIRIList = [];
    let queryStrPowerPlantIRI = `PREFIX powerplant:<http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#>
    PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
    SELECT DISTINCT ?powerPlantIRI
    WHERE
    {
    ?powerPlantIRI rdf:type powerplant:PowerPlant .
    }`;
    aQueryer.queryPromise(queryStrPowerPlantIRI, config.localRDF4j) // the config.localRDF4j is the address of the endpoint
        .then((result) => {
            for (line of result) {
                powerPlantIRIList.push(line);
            }
        });
    return powerPlantIRIList;
}
*/
