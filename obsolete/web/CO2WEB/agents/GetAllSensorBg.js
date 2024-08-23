/**
 */
const axios = require('axios').default;

/*SPRAQL Query******************/
const bgep = 'http://10.25.188.4:9999/blazegraph/namespace/bmstest/sparql';

const qstrSensors = `
select ?sensor {
?sensor <https://github.com/cambridge-cares/TheWorldAvatar/blob/develop/JPS_Ontology/ontology/ontotimeseries/OntoTimeSeries.owl#hasTimeSeries> ?o.}
`;
/***********************************/
//    ?DataPoint rdf:type owl:NamedIndividual;

function getAllSensors(callback) {
    //readPPChildren
    //request on each to get geo info
        const searchParams = new URLSearchParams();
        searchParams.set('query', qstrSensors);
        
        axios.post(bgep, searchParams.toString())
            .then(function (response) {
                resultJ = response.data;
                if (('results' in resultJ && 'bindings' in resultJ['results'])===false){
                    callback(new Error('No result returned from blazegraph'));
                }
                urlResultList = resultJ['results']['bindings'];
                let urls = urlResultList.map((resultitem)=>{return resultitem['sensor']['value']});
                callback(null, urls);
            })
            .catch(function (error) {
                callback(error);
            });
    
    
}

module.exports = getAllSensors;
