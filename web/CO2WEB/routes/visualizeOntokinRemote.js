var express = require('express');

var owlProcesser = require("../agents/fileConnection2Way.js");
var router = express.Router();

var connectionsReader = Object.create(owlProcesser);
var topNode2 = require("../config").ontospeciesRemoteNode;
var topNode=  require("../config").ontocompchemRemoteNode;
console.log("****************")
console.log(topNode)
let opts = {useSharp:true, topnode:topNode,viewName:'visualizeExUpdate',supQuery:

    `
 PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
PREFIX ontocompchem: <http://www.theworldavatar.com/ontology/ontocompchem/ontocompchem.owl#> 


SELECT distinct ?uri
    WHERE {{
    ?uri rdf:type ontocompchem:G09 .
    }}

`};

let opts2 = {useSharp:true, topnode:topNode,viewName:'visualizeExUpdate',supQuery:
        `
 PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
PREFIX ontokin: <http://www.theworldavatar.com/kb/ontokin/ontokin.owl#>
PREFIX ontocompchem: <http://www.theworldavatar.com/ontology/ontocompchem/ontocompchem.owl#> 

SELECT distinct  ?uri
    WHERE {{
?s ontocompchem:hasUniqueSpecies  ?uri
    }}

`};
router.get('/', function(req, res, next) {
    res.render(opts.viewName); //render the view with this value, { result: JSON.stringify(results)
});

router.get('/links', function(req, res, next) {
    opts['level'] = 1;
    opts2['level'] = 1;
    connectionsReader.processSingle(opts).then((results)=>{
        connectionsReader.processSingle(opts2).then((results2)=>{

        console.log("read connections");
        //for(let re of results){
        //    console.log(re);
        //}
        //res.setHeader('Content-Type', 'application/json');
        //res.json(results);//for testing
            results2.forEach((item)=>{item['source'] = topNode2})
            results = results.concat(results2)
        conns = results;
        results.topnode = opts.topnode;
        res.json(results); //render the view with this value
    });
    });

});
router.post('/linksingle', function(req, res, next) {
    console.log('body')
    console.log(req.body)
    let body = JSON.parse(req.body)
    let supQuery = 'query' in body?body['query']:null;
    let topnode = topNode;
    let level = 1;
    console.log(topnode)
    console.log(supQuery)
    let useSharp = opts.useSharp

    connectionsReader.processSingle({supQuery, topnode, useSharp, level}).then((results)=>{


        console.log("read connections");

        //res.setHeader('Content-Type', 'application/json');
        //res.json(results);//for testing
        console.log(results)
        conns = results;


        res.json(results); //render the view with this value

    });

});

module.exports = router;




let q = `
PREFIX ontocompchem: <http://www.theworldavatar.com/ontology/ontocompchem/ontocompchem.owl#> 

SELECT distinct  ?uri
WHERE {
?uri ontocompchem:hasUniqueSpecies  <http://www.theworldavatar.com/kb/ontospecies/00b537ef-8b6f-3246-9a7e-edd0259c6e09.owl#00b537ef-8b6f-3246-9a7e-edd0259c6e09>
}`
let optst = {useSharp:true, topnode:topNode,viewName:'visualizeExUpdate', supQuery:q}
connectionsReader.processSingle(optst).then((resultst)=> {
console.log(resultst)
})