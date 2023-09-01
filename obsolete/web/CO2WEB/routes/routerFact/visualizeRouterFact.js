/****
 * Factory to create a router with file-connection-reader module.
 * Parameter : topNode file
 * @type {*}
 */
var express = require('express');
var owlProcesser = require("../../agents/fileConnection2Way.js");

var connectionsReader = Object.create(owlProcesser)
/* GET users listing. */


/****
 * Factory to create a router with file-connection-reader module.
 * @param topNodeAddress  address of top node file
 */
var visualizationRouterFactory = function (opts) {
    var router = express.Router();
    var viewName = opts.viewName?opts.viewName:'visual';
    router.get('/', function(req, res, next) {
                res.render(viewName); //render the view with this value, { result: JSON.stringify(results)
    });
    
    router.get('/links', function(req, res, next) {
    
            opts['showImport'] = false;

        connectionsReader.process(opts).then((results)=>{

            
            console.log("read connections");
            
            //res.setHeader('Content-Type', 'application/json');
            //res.json(results);//for testing
            console.log(results)
            conns = results;
            results.topnode = opts.topnode;
    
    
            res.json(results); //render the view with this value
            
        });
        
    });
    
    router.get('/includeImport', function(req, res, next) {

        opts['showImport'] = true;
    
        connectionsReader.process(opts).then((results)=>{

            
            console.log("read connections");
            
            //res.setHeader('Content-Type', 'application/json');
            //res.json(results);//for testing
            //console.log(results)
            conns = results;
            results.topnode = opts.topnode;

            //res.setHeader('Content-Type', 'application/json');
            // res.json(results);
            res.json(results); //render the view with this value


        });
    });

/**
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
