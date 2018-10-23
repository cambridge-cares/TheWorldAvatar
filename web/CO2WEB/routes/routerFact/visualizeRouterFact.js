/****
 * Factory to create a router with file-connection-reader module.
 * Parameter : topNode file
 * @type {*}
 */
var express = require('express');
var owlProcesser = require("../../agents/fileConnection2Way.js");

var connectionsReader = Object.create(owlProcesser)
/* GET users listing. */

//TODO: buffer logic, so no need recalculate for each request, but still robust,
//TODO: could be time, or responding to change
/****
 * Factory to create a router with file-connection-reader module.
 * @param topNodeAddress  address of top node file
 */
var visualizationRouterFactory = function (topNodeAddress) {
    var router = express.Router();

    router.get('/', function(req, res, next) {

                            
                res.render('visual'); //render the view with this value, { result: JSON.stringify(results)

    
    });
    
    router.get('/links', function(req, res, next) {
    
    
        connectionsReader.process({topnode : topNodeAddress}).then((results)=>{

            
            console.log("read connections");
            
            //res.setHeader('Content-Type', 'application/json');
            //res.json(results);//for testing
            console.log(results)
            conns = results;
            results.topnode = topNodeAddress;
    
    
            res.json(results); //render the view with this value
            
        });
        
    });
    /***
    router.get('/includeImport', function(req, res, next) {


        connectionsReader.getChildrenRecur({ showImport : true, topnode : topNodeAddress}, function (err, results) {

            if(err){
                console.log(err);
                res.status(500).send(err);
            }

            console.log("read connections");

            //res.setHeader('Content-Type', 'application/json');
            // res.json(results);
            res.json(results); //render the view with this value


        });
    });


    router.get('/showServiceOnly', function(req, res, next) {

        connectionsReader.getChildrenRecur({ showServiceOnly : true, showServiceUrl: true, topnode : topNodeAddress}, function (err, results) {

            if(err){
                res.status(500).send(err);
            }

            console.log("read connections");

            //res.setHeader('Content-Type', 'application/json');
            // res.json(results);
            res.json(results); //render the view with this value


        });


    });
***/
    return router;
};


module.exports = visualizationRouterFactory;
