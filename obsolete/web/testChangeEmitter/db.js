/**
 * Created by Shaocong on 6/7/2017.
 * A mock db for this template
 */
var fs = require('fs');
var util = require('util');

var config = require('./config');

var xml2js = require("xml2js");
var xpath = require("xml2js-xpath");
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
             //let values = result['rdf:RDF']["system:ScalarValue"];
             console.log(file);
               let values = xpath.find(result, "//owl:NamedIndividual//system:ScalarValue");
            // let values2 = xpath.find(result, "//http://www.w3.org/2002/07/owl#NamedIndividual[@http://www.w3.org/1999/02/22-rdf-syntax-ns#about='#VAV-E/7-7_sensor3']" );

                 //"//http://www.theworldavatar.com/OntoCAPE/OntoCAPE/upper_level/system.owl#ScalarValue");
                  console.log("@@@@@@@@@@@@@@")
                for (let value of values){

                    console.log(getIDfromName(value['$']['rdf:ID']));
                }



             //now sort according to id contains in name
              values.sort(function (a, b) {
                  //retreive id

                  return getIDfromName(a['$']['rdf:ID']) - getIDfromName(b['$']['rdf:ID']);

              });
             function getIDfromName(nameStr){
                 var nameArr =nameStr.split("_");

                 return parseInt(  nameArr[nameArr.length - 1]);

             }
             // console.log(util.inspect(values));
              let scaValues = values.map((item) => {
                //  console.log(JSON.stringify(item));
                  let dateStr = item['system:isObservedAgainstBackdrop'][0]['coordinate_system:CoordinateValue'][0]['system:numericalValue'][0]['_'];
                //  console.log(item['system:numericalValue'][0]['_']);
                 return {value : item['system:numericalValue'][0]['_'], time :ddmmyy2utc(dateStr)};
              });

              callback(null, scaValues);
         });
     });



    };

 /*utility function to transfer date from ddmmyy format to utc format*/
    function ddmmyy2utc(datestring){
// 6- 8-2017 21:58:38

        var parts = datestring.match(/(\d{1,2})-\s*(\d{1,2})-(\d{4})\s+(\d{1,2}):(\d{2}):(\d{2})/);

      //  console.log(JSON.stringify(parts));

        if(!parts){
            console.log("!!!!!!!!!!!!!!!")
            console.log(datestring);
        }
        return Date.UTC(+parts[3], parts[1]-1, +parts[2], parseInt(parts[4])-8, +parts[5], parts[6]);

    }


 return {getData };

}

//db().getData(function () {});
module.exports = db;




