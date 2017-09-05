/**

 * Configurations for the project, used both in app && test
 *  @ root folder for owl files
 *  @ root node file name
 *  @ port
 */


var path = require('path')
var config = {};



//configDevelop();
configDevelop();
config.worldNode = config.root + "/TheWorld.owl";
config.jurongNode = config.root + "/JurongIsland.owl";
config.bmsNode = config.root + "/CARES_Lab.owl";
config.semakauNode = config.root + "/SemakauIsland.owl";
config.landLotNode=config.root + "/JParkLandLots.owl";
config.registerPath = "dataObserve";
config.changePath = "change";
config.viewRoot = __dirname + "/views";

config.bmsUrlPath =  config.registerUrl+"/" +config.registerPath;          //testing
config.myUrlPath = config.changeUrl+"/" +config.changePath;



function configDevelop() {
    config.root = __dirname + "/testFiles" ; // own folder for testing
    config.port = 3000;//port for deploy
    config.registerUrl = "http://localhost:2000";
    config.changeUrl = "http://localhost:3000";

    //"http://www.theworldavatar.com:82/change";
}

function configDeploy() {
    config.root = "C:/TOMCAT/webapps/ROOT";
    config.port = 82;//port for deploy
    config.registerUrl = "http://10.25.188.104";
    config.changeUrl = "http://www.theworldavatar.com:82";

}













module.exports = config;