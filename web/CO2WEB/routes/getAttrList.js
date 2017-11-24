/***
 * Router
 *corresponds to agent: GetLiteralData
 * @type {log4js}
 */
var log4js = require('log4js');
var logger = log4js.getLogger();
logger.level = 'debug';

var express = require('express');
var router = express.Router();

var config = require("../config");

var LiteralData = require("../agents/GetLiteralData");

router.use(function(req, res, next) {
  res.header("Access-Control-Allow-Origin", "*");
  res.header("Access-Control-Allow-Headers", "Origin, X-Requested-With, Content-Type, Accept");
  next();
});

router.post('/', function (req, res, next) {
        if(!req.body) {
            next(new Error("Can not find req body"));
        }
        let parsedBody = JSON.parse(req.body);

    let uri = parsedBody.uri;

        if(!uri){
            console.log("Can not find uri")
            next(new Error("Can not find uri in req body"))
         return;
         }
        LiteralData( function (err, result) {
        if(err){
            next(err);
            return;
        }
            res.json(result); //render the view with this value

        },uri)
});

module.exports = router;