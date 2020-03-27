/****
 * Factory to create a router with file-connection-reader module.
 * Parameter : topNode file
 * @type {*}
 */
var express = require('express');
var owlProcesser = require("../../agents/fileConnection2Way.js");
var config  = require("../../config.js")
var connectionsReader = Object.create(owlProcesser)
/* GET users listing. */

//TODO: buffer logic, so no need recalculate for each request, but still robust,
//TODO: could be time, or responding to change
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
    
        opts['level'] = 1;
        connectionsReader.processSingle(opts).then((results)=>{

            
            console.log("read connections");
            
            //res.setHeader('Content-Type', 'application/json');
            //res.json(results);//for testin
            //add a second level query
            console.log(results)
            conns = results;
            results.topnode = opts.topnode;
            res.json(results); //render the view with this value
            
        });
        
    });
    router.post('/linksingle', function(req, res, next) {
        console.log('body')
       let body = JSON.parse(req.body)
        let supQuery = 'query' in body?body['query']:null;
        let topnode = body['iri'];
        //console.log(topnode)
        //console.log(supQuery)
		let useSharp = opts.useSharp
    
        connectionsReader.processSingle({supQuery, topnode, useSharp}).then((results)=>{
            
            console.log("read connections for single node");
            
            //res.setHeader('Content-Type', 'application/json');
            //res.json(results);//for testing
            //console.log(results)
            let secondQuery = {useSharp:true, topnode:config.ontocompchemNode,supQuery:opts.supQuery};
            console.log(opts.supQuery)
            connectionsReader.processSingle(secondQuery).then((results2)=> {
                console.log('secondary result')
                console.log(results2)
                if(results2.length>0) {
                    results = results.concat(results2);
                    console.log(results)
                }
                res.json(results); //render the view with this value
            })
    

            });
        
        
            
        });
        
    
    router.get('/includeImport', function(req, res, next) {

        let importO = {};
        importO['showImport'] = true;
        importO['topnode'] = opts['topnode']
        importO['supQuery'] = `
PREFIX owl: <http://www.w3.org/2002/07/owl#>
    select   ?uri
    where {
        ?a owl:imports ?uri;}`
        connectionsReader.processSingle(importO).then((results)=>{

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
