/***
 * This is only for testing purpose/
 * This program makes sure that the dummy file will add a new data point every 500ms like the real bms file does.
 *
 */
var xml2js = require('xml2js');
var xmlparser = new xml2js.Parser();

var util = require('util');
var fs = require('fs');

var config = require('../config');
var file = config.fileLocation;


//add a new value to fake bms owl/dummy every 10s
setInterval(add2Dummy, 10000);//TODO: should actually be 60s, for ease of testing use 10s

 function add2Dummy() {


    fs.readFile(file, function(err, data) {
        xmlparser.parseString(data, function (err, result) {
            console.log(config.fileLocation);

           // console.log(util.inspect(result));
            //console.log(util.inspect(result['rdf:RDF']["system:ScalarValue"]));
            var id = result['rdf:RDF']["system:ScalarValue"].length +1; //keep record of the data id;
            var time = "6- 8-2017 21:"+(id+17)+":00"; // make a mock time string

             var value = 1177.67 + Math.random() *500; //make a mock value
             value = value.toFixed(2);
            console.log(id);
            /*template data point in json*/
            var temp = { '$': { 'rdf:ID': 'v_AirFlow_'+id },
                'system:numericalValue': [ { '_': value ,'$' :{"rdf:whatever":"no" }} ],
                'system:hasUnitOfMeasure': [ { '_': 'CMH','$' : {"rdf:whatever":"no"} } ],
                "system:isObservedAgainstBackdrop":[
                    {
                        "coordinate_system:CoordinateValue":[
                            {
                                "$":{
                                    "rdf:ID":"t_1"
                                },
                                "system:numericalValue":[
                                    {
                                        "_":time,
                                        "$":{
                                            "rdf:datatype":"http://www.w3.org/2001/XMLSchema#string"
                                        }
                                    }
                                ]
                            }
                        ]
                    }
                ],

            };

            result['rdf:RDF']["system:ScalarValue"].push(temp);
            //build xml from json
            var builder = new xml2js.Builder();
            var xml = builder.buildObject(result);
            //now write back to file
            fs.writeFile(file, xml, function (err) {
                if(err){
                    throw err;
                }
            })

        });
    });

};

