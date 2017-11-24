/****
 * Factory to create a router with file-connection-reader module.
 * Parameter : topNode file
 * @type {*}
 */
var express = require('express');
var connectionsReader = require("../../agents/fileConnection.js");

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


            connectionsReader.getChildrenRecur({topnode : topNodeAddress}, function (err, results) {

                if(err){
                    res.status(500).send(err);
                    console.log(err);
                    return;
                }

                console.log("read connections");

                //res.setHeader('Content-Type', 'application/json');
                //res.json(results);//for testing
                conns = results;
                res.render('visual', { result: JSON.stringify(results) }); //render the view with this value


            });

    });


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

    return router;
};


module.exports = visualizationRouterFactory;
