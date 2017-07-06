/**

 * Configurations for the project, used both in app && test
 *  @ root folder for owl files
 *  @ root node file name
 *  @ port
 */


var path = require('path')
var config = {};

config.root = "C:/TOMCAT/webapps/ROOT";


//__dirname + "/TestVisualization" ; // own folder for testing

config.worldNode = config.root + "/TheWorld.owl";
config.jurongNode = config.root + "/JurongIsland.owl";
config.bmsNode = config.root + "/CARES_Lab.owl";
config.semakauNode = config.root + "/SemakauIsland.owl";

config.port = 82;//port for deploy
config.registerPath = "dataObserve";
config.registerUrl = "http://10.25.188.104";
//"http://localhost:2000";
config.bmsUrlPath =  config.registerUrl+"/" +config.registerPath;          //testing

   // "http://10.25.188.104/dataObserve";
config.viewRoot = __dirname + "/views";
config.changePath = "change";
config.changeUrl = //"http://localhost:3000";
"http://www.theworldavatar.com:82";
config.myUrlPath = config.changeUrl+"/" +config.changePath;

    //"http://www.theworldavatar.com:82/change";

module.exports = config;