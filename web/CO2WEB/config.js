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
config.port = 2000;//port for deploy
config.bmsUrl = "http://10.25.188.104/dataObserve";
config.viewRoot = __dirname + "/views";
config.myUrl = "http://www.theworldavatar.com:81/change";

module.exports = config;