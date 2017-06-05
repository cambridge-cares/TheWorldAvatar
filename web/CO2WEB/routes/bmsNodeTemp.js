/**
 * Created by Shaocong on 6/5/2017.
 */
var express = require('express');
var router = express.Router();
var  request = require("request");

let url = "http://www.bms.com/JPS_KB_CARES_Lab_Node/FH-01.owl";
/* GET home page. */
router.get('/', function(req, res, next) {

    request(url, function (err, resp, body) {

        if(err){
            next(err);
            return;
        }

        console.log(body);
        res.header('Content-Type','text/xml').send(body)

    })

    //send a file
});

module.exports = router;
