var express = require('express');
var async = require('async');
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


SELECT distinct ?uri ?label
    WHERE {
    ?uri rdf:type ontocompchem:G09 .
    ?uri ontocompchem:hasUniqueSpecies  ?label.
    } 
`};

let opts2 = {useSharp:true, topnode:topNode,viewName:'visualizeExUpdate',supQuery:
        `
 PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
PREFIX ontokin: <http://www.theworldavatar.com/kb/ontokin/ontokin.owl#>
PREFIX ontocompchem: <http://www.theworldavatar.com/ontology/ontocompchem/ontocompchem.owl#> 

SELECT distinct  ?uri 
    WHERE {
?s ontocompchem:hasUniqueSpecies  ?uri.
    }
`};

let opts3 = {useSharp:true, level: 1,topnode:topNode2,viewName:'visualizeExUpdate',supQuery:
`
SELECT distinct  ?uri ?label
    WHERE {
?uri <http://www.w3.org/2000/01/rdf-schema#label> ?label
    }`};



router.get('/', function(req, res, next) {
    res.render(opts.viewName); //render the view with this value, { result: JSON.stringify(results)
});

router.get('/links', function(req, res, next) {
    opts['level'] = 1;
    opts2['level'] = 1;
    connectionsReader.processSingle(opts).then((results)=>{
        connectionsReader.processSingle(opts2).then((results2)=>{
        console.log("read connections");
            results2.forEach((item)=>{item['source'] = topNode2})
            connectionsReader.processSingle(opts3).then((labelResult)=>{
                let filterResult = [], co2result = [];
                console.log('looking for labels')
                    for (let uriItem of results2) {
                        for(let labelItem of labelResult) {
                            if (uriItem["target"] === labelItem["target"]) {
                            uriItem["label"] = labelItem["label"];
                            console.log('found label')
                            console.log(uriItem["label"])
                            }
                        }
                        for(let mechaItem of results){
                            if (mechaItem["label"] === uriItem["target"]){//mechaItem links to co2
                            if (uriItem["label"] === "CO2"){
                                mechaItem["label"] ="";
                                console.log('find co2 entity')
                                co2result.push(mechaItem);
                                co2result.push(uriItem);
                                } else{
                                mechaItem["label"] ="";
                                filterResult.push(mechaItem);
                                filterResult.push(uriItem);
                            }

                            }
                        }

                }

               filterResult.splice(60);


                let finalResults = filterResult.concat(results2)
                res.json(filterResult.concat(co2result)); //render the view with this value
            });
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
        conns = results;


        res.json(results); //render the view with this value

    });

});

module.exports = router;




let q = `
PREFIX ontocompchem: <http://www.theworldavatar.com/ontology/ontocompchem/ontocompchem.owl#> 

SELECT distinct  ?uri
    WHERE {{
?s ontocompchem:hasUniqueSpecies  ?uri
    }}`

let optst = {useSharp:true, topnode:topNode,viewName:'visualizeExUpdate', supQuery:q}
connectionsReader.processSingle(opts).then((resultst)=> {
console.log(resultst)
})