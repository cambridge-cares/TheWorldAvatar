/**
 * Created by Shaocong on 6/7/2017.
 */

var path = require('path');

exports.filePath = 
//"D:/apache-tomcat-9.0.0.M21/webapps/ROOT/JPS_KB_CARES_Lab_Node";
 __dirname + "/ROOT"; //folder that contains the data file

exports.fileName = "FH-01.owl";
exports.fileLocation  = exports.filePath + "/"+exports.fileName;// location of the data file ,filename Included


exports.port = 2000;//port of testing
//=80; //port for deploying