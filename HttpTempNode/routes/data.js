/**
 * Created by Shaocong on 4/17/2017.
 * Handling /data request.
 * When post, emit change to clients from socket io
 */
let express = require('express');
let async = require('async');
let util = require('util');
let router = express.Router();

let changeListener = (require("../util/changeListener"))();
let loopChild = require("../util/loopChild");
let db = require("../util/mockDB");
//TODO: get data
router.get('/', function (req, res, next) {

    //parse json for request
    try {
        console.log("receive json: "+req.query.uris);
        let data = JSON.parse(req.query.uris);

        if(!data.hasOwnProperty("uris")){
            console.log("uris not defined in json object uris parsed by query string");
            throw new Error("uris not defined in json object uris parsed by query string");
        }
        let uris = data.uris;
        console.log("names2 search: "+ uris.toString());

        //results array format [ {uriA : DataA}, {uriB : DataB}...  ]
        //search it among own owl/db first
        db.searchDB(uris, function (err, dBresults, noFoundUris) {//return error if any, results of found, arr of not found uris(the rest)
                if(err){
                    console.log(err);
                    res.status(500).send({error: err.message});

                }

            if(!noFoundUris || noFoundUris.length < 1){//all requested data is found in own db, return immediately
                console.log(util.inspect(dBresults));

                let temp = {results:dBresults};
                res.json(temp);
                return;
            }
            //else, request children for data
            console.log("search path:"+req.originalUrl+"  baseUrl:"+req.baseUrl+"  path:"+req.path);


            async.concat(noFoundUris,

                function (queryParam, callback) {
                    loopChild(req.baseUrl,queryParam, function (err, result) {
                        if (err) {
                            callback(err);
                            return;
                        }
                        //extract null from result arr or object
                        result = result.filter(function(n){ return n !== null && n!==undefined });
                        if(result.length < 1){
                            callback(new Error("Data can not be found"));
                            return;
                        }
                        callback(null, result[0]);

                    })
                }
                , function (err, childResults) {
                    if (err) {
                        console.log(err);
                        res.status(500).send({error: err.message});
                        return;
                    }
                    console.log("own db results: " +util.inspect(dBresults));
                    console.log("children db results: " +util.inspect(childResults));
                    let temp = {results:dBresults.concat(childResults)};
                    console.log(temp);

                    res.json(temp);

                });

        });


    } catch (errParseQuery) {
        if (errParseQuery) {
            res.status(500).send({error: errParseQuery.message});
        }
    }
});


/* WHEN POST NEW DATA, emit change event */
router.post('/', function (req, res, next) {
//PARSE data
    if (req.body === undefined) {
        //console.log("ERR: Req body undefined");
        res.status(400).send({error: "Request body undefined"});
        return;
    }
    let reqBody = req.body;

//  TODO:update data(if data not in current node, post to destination)


    changeListener.emitChange(reqBody);

    //TODO:return updated data
    res.send("success");
});

module.exports = router;