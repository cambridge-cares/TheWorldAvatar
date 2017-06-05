/**
 * Created by MASTE on 5/2/2017.
 */

var fs = require('fs'),
    $rdf = require('rdflib');
var rdfData=fs.readFileSync(__dirname+'/E-301.owl').toString();

var store=$rdf.graph();
var contentType='application/rdf+xml';
var baseUrl="http://www.jparksimulator.com";
var name = "ValueOfHeatDutyOfE-301";
var IRI = $rdf.sym(baseUrl + "/E-301.owl#" + name);
var pre = $rdf.sym('file:/C:/OntoCAPE/OntoCAPE/upper_level/system.owl#hasUnitOfMeasure');

try{
    $rdf.parse(rdfData,store,baseUrl,contentType);

    var stms = store.statementsMatching(IRI, pre , undefined);
    for (var i=0; i<stms.length;i++) {
        var stm = stms[i];
        if(stm.object)
        {
            console.log(stm) // the WebID of a friend
        }
        console.log("------------------------------------------");
    }
} catch(err){
    console.log(err);
}