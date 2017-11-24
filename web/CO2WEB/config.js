/**

 * Configurations for the project, used both in app && test
 *  @ root folder for owl files
 *  @ root node file name
 *  @ port
 */


var path = require('path')
var config = {};



configDevelop();
//configDeploy();
config.worldNode = path.join(config.root , "TheWorld.owl");
config.jurongNode = path.join(config.root ,"JurongIsland.owl");
config.b3Node = path.join(config.root , "BiodieselPlant3.owl");

config.b2Node = path.join(config.root , "BiodieselPlant2.owl");
config.bmsFolder = path.join(config.root , "BMS");
config.bmsNode = path.join(config.bmsFolder , "CARES_Lab.owl");
config.bmsplotnode = path.join(config.bmsFolder, "BCA_RT_sensor1.owl");
config.semakauNode = path.join(config.root , "SemakauIsland.owl");
config.landLotNode=path.join(config.root , "JParkLandLots.owl");
//TODO: this later should be wrapped in owl file
config.heatWasteScript = path.join(__dirname, "agents/WHR_network_optimization_trim.py")
config.heatWasteNode = path.join(config.root, "wasteheatnetwork.owl")


config.registerPath = "dataObserve";
config.changePath = "change";
config.viewRoot = path.join(__dirname , "views");

config.bmsUrlPath =  config.registerUrl+"/" +config.registerPath;          //testing
config.myUrlPath = config.changeUrl+"/" +config.changePath;



function configDevelop() {
    config.root = path.join(__dirname ,  "testFiles") ; // own folder for testing
    config.port = 3000;//port for deploy
    config.registerUrl = "http://localhost:2000";
    config.changeUrl = "http://localhost:3000";
    config.ppFolder = path.join(config.root, "powerplants")
    //"http://www.thewordavatar.com:82/change";
}

function configDeploy() {
    config.root = path.normalize("C:/TOMCAT/webapps/ROOT");
    config.port = 82;//port for deploy
    config.registerUrl = "http://10.25.188.104";
    config.changeUrl = "http://www.theworldavatar.com:82";
    config.ppFolder = config.root

}













module.exports = config;