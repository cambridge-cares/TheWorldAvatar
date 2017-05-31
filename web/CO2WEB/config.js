/**
 * Created by Shaocong on 5/25/2017.
 * Configurations for the project, used both in app && test
 *  @ root folder for owl files
 *  @ root node file name
 *  @ port
 */

var config = {};

config.root = 
//"C:/TOMCAT/webapps/ROOT";

__dirname + "/TestVisualization" ; // own folder for testing

config.rootNode = "TheWorld.owl";
config.port = 82; //port for testing

module.exports = config;