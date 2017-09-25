/**
 * Created by Shaocong on 9/15/2017.
 */
/**
 * Created by Shaocong on 6/15/2017.
 */
var log4js = require('log4js');
var logger = log4js.getLogger();
logger.level = 'debug';

var express = require('express');
var router = express.Router();

var config = require("../config");

var LiteralData = require("../agents/GetLiteralData");



router.post('/', function (req, res, next) {
        if(!req.body) {
            next(new Error("Can not find req body"));
        }
        console.log(req.body);
        let parsedBody = JSON.parse(req.body);
    console.log(parsedBody);

    let uri = parsedBody.uri;

        if(!uri){
            console.log("Can not find uri")
            next(new Error("Can not find uri in req body"))
         return;
         }
        LiteralData(uri, function (err, result) {
        if(err){
            next(err);
            return;
        }
            res.json(result); //render the view with this value

        })
});

module.exports = router;