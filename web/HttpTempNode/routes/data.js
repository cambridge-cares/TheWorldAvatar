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
//TODO: check request

router.get('/', function (req, res, next) {
    //query : name, property
    console.log("request data");
    //console.log(req.path);
   // console.log(req.originalUrl);
    try {
        //TODO: pack everything into nodeIRI and property IRI
        console.log("search name: "+req.query.name);
        let propertyIRI = req.query.property || undefined;
        if(!req.query.name){
            res.status(500).send({error : "Search node IRI not supplied"});
        }
        let nodeIRI = req.query.name;


        console.log("search property: "+req.query.property);


        //results array format [ {uriA : DataA}, {uriB : DataB}...  ]
        //search it among own owl/db first
        db.searchDB(nodeIRI, propertyIRI, function (err, result) {//return error if any, results of found, arr of not found uris(the rest)
                if(err){
                    console.log(err);
                    res.status(500).send({error: err.message});

                }


                res.json(result);
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