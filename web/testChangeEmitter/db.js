/**
 * Created by Shaocong on 6/7/2017.
 * A mock db for this template
 */
var fs = require('fs');
var util = require('util');

var config = require('./config');

var xml2js = require("xml2js");
var xmlparser = new xml2js.Parser();



function db(){
    var file= config.fileLocation;
 var self = this;


 getData = function(callback){

     fs.readFile(file, function(err, data) {

         if(err){
             callback(err);
             return;
         }
         xmlparser.parseString(data, function (err, result) {
             if(err){
                 callback(err);
                 return;
             }
             //retreive all value data
             let values = result['rdf:RDF']["system:ScalarValue"];


             //now sort according to id contains in name
              values.sort(function (a, b) {
                  //retreive id

                  return getIDfromName(a['$']['rdf:ID']) - getIDfromName(b['$']['rdf:ID']);


                  function getIDfromName(nameStr){
                      var nameArr =nameStr.split("_");

                      return parseInt(  nameArr[nameArr.length - 1]);

                  }
              });

              console.log(util.inspect(values));
              let scaValues = values.map((item) => {
                  console.log(JSON.stringify(item));
                  let dateStr = item['system:isObservedAgainstBackdrop'][0]['coordinate_system:CoordinateValue'][0]['system:numericalValue'][0]['_'];
                 return {value : item['system:numericalValue'][0]['_'], time :ddmmyy2utc(dateStr)};
              });

              callback(null, scaValues);
         });
     });



    };

 /*utility function to transfer date from ddmmyy format to utc format*/
    function ddmmyy2utc(datestring){
// 6- 8-2017 21:58:38

        var parts = datestring.match(/(\d{1,2})- (\d{1,2})-(\d{4}) (\d{2}):(\d{2}):(\d{2})/);

        console.log(JSON.stringify(parts));

        return Date.UTC(parts[3], parts[1] - 1, parts[2], parseInt(parts[4]) - 8, parts[5], parts[6]);

    }


 return {getData };

}

//db().getData(function () {});
module.exports = db;




