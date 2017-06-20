/**

 * Configurations for the project, used both in app && test
 *  @ root folder for owl files
 *  @ root node file name
 *  @ port
 */

var config = {};

config.root = //"C:/TOMCAT/webapps/ROOT";


__dirname + "/TestVisualization" ; // own folder for testing

config.rootNode = "TheWorld.owl";
config.port = 3000;//port for deploy
config.bmsUrl =  "http://localhost:2000/dataObserve";           //testing

    //"http://10.25.188.104/dataObserve";
config.viewRoot = __dirname + "/views";
config.myUrl = "http://localhost:3000/change";

    //"http://www.theworldavatar.com:82/change";

module.exports = config;