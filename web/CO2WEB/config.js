
/**

 * Configurations for the project, used both in app && test
 *  @ root folder for owl files
 *  @ root node file name
 *  @ port
 */


var path = require('path')
var config = {};

config.baseUri = "http://www.theworldavatar.com"

config.crebase = "http://www.theworldavatar.com/damecoolquestion/ontochem"
config.ontokinbase = "http://www.theworldavatar.com/damecoolquestion/ontokin2"

config.localRDF4j = "https://como.ceb.cam.ac.uk/rdf4j-server/repositories/UKPowerPlant"

// configDevelop();

configDeploy();
configClaudius();
config.agentShowcaseNode = path.join(config.root , "kb/subsetWorld.owl");
config.worldNode = path.join(config.root , "kb/TheWorld.owl");
config.ppNode = path.join(config.root , "kb/powerplants/WorldPowerPlants.owl");
config.jurongNode = path.join(config.root ,"kb/sgp/jurongisland/JurongIsland.owl");
config.b3Node = path.join(config.root , "kb/sgp/jurongisland/biodieselplant3/BiodieselPlant3.owl");
config.ontoENNode = path.join(config.root, "kb/sgp/jurongisland/jurongislandpowernetwork/JurongIslandPowerNetwork.owl");



//config.b2Node = path.join(config.root , "BiodieselPlant2.owl");
config.bmsFolder = path.join(config.root , "BMS");
config.bmsNode = path.join(config.bmsFolder , "CARES_Lab.owl");
config.bmsplotnode = path.join(config.bmsFolder, "BCA_RT_sensor1.owl");
config.semakauNode = path.join(config.root , "kb/sgp/semakauisland/SemakauIsland.owl");
config.landLotNode=path.join(config.root , "kb/sgp/jurongisland/JParkLandLots.owl");
config.ontochemNode= config.crebase + '/query';
config.ontokinNode= config.ontokinbase + '/query';
config.ontokinRemoteNode = "http://localhost/rdf4j-server/repositories/ontokin"
config.ontocompchemRemoteNode = "http://localhost/rdf4j-server/repositories/ontocompchem"
config.ontospeciesRemoteNode = "http://localhost/rdf4j-server/repositories/ontospecies"
//TODO: this later should be wrapped in owl file
config.heatWasteScript = path.join(__dirname, "agents/WHR_network_optimization_trim.py")
config.heatWasteNode = path.join(config.root, "wasteheatnetwork.owl")





function configDevelop() {
    // config.root = path.join(__dirname ,  "testFiles") ; // own folder for testing
    config.port =3000
//    config.root = path.normalize("C:/Users/LONG01/TOMCAT/webapps/ROOT");
    config.root = path.normalize("C:/Users/KADIT01/TOMCAT/webapps/ROOT");
    config.registerUrl = "http://localhost:";
    config.ppFolder = path.join(config.root, "kb/powerplants")
    //"http://www.thewordavatar.com:82/change";
}

function configDeploy() {
     config.root = path.normalize("C:/TOMCAT/webapps/ROOT");
    //config.root = path.normalize("C:/Users/LONG01/TOMCAT/webapps/ROOT");
	//config.root2 = path.normalize("C:/TOMCAT/webapps/ROOT/kb/sgp/semakauisland");
    config.port = 82;//port for deploy
    config.registerUrl = "http://10.25.188.104";
    config.changeUrl = "http://www.theworldavatar.com:82";
    config.ppFolder = path.join(config.root , "kb/powerplants");
	//config.ppFolder = path.normalize("C:/TOMCAT/webapps/ROOT/kb/powerplants");

}
function configClaudius(){
    config.onCares = true;
}
function configCOMO(){
    config.onCares = false;
}

module.exports = config;