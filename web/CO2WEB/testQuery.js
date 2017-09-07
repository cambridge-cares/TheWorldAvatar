/**
 * Created by Shaocong on 8/31/2017.
 */
const parser = require('./agents/rdfParser');

var uri = "http://www.theworldavatar.com/Swanbank_B_Coal_Power_Plant_Australia.owl"
uri = uri.replace("http://www.theworldavatar.com", __dirname + "/TestFile");
var mparser = new parser.RdfParser({fileUrl: uri});
var qsTypeCountry = `
    PREFIX system_realization: <http://www.theworldavatar.com/OntoEIP/system_aspects/system_realization.owl#>
        PREFIX system_v1: <http://www.theworldavatar.com/OntoEIP/upper_level/system_v1.owl#>

        select distinct ?Power_Plant_Name ?Fuel_Type ?Country
        where {
        ?Power_Plant_Name a p14:PowerGenerator;
    j.0:realizes ?Fuel_Type.

        ?Power_Plant_Name a p14:PowerGenerator;
    p9:hasAddress ?x.
        ?x system_v1:hasName ?Country.
}`;
mparser.mquery(qsTypeCountry, function (err, result) {

    console.log(JSON.stringify(result));

})