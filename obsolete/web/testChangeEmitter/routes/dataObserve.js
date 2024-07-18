/**
 * Created by Shaocong on 6/7/2017.
 * other nodes can call this route to:
 * start observing change on a file in this node
 * Also get the file content at the moment of calling
 * After call this route, Each time the file changes, this node will post the changed file to each registered url
 * json data : {url, getInitData}
 * @url: string,   url to be registered(including path) / url to post change to
 * @getInitData: bool, if require for initial data, initial data will be sent int response if this is set to true
 */

var express = require('express');
var router =express.Router();
var bodyParser = require("body-parser");
var watcherConfig = require("../util/changeWatcher");
var config = require("../config");


var changeWatcher = watcherConfig({location:config.fileLocation, contentType:"application/json"});
var db = require('../dbtest')();

changeWatcher.setWatch(function (err, results) {

    if(err){
        console.log(err);//TODO err handling
    }

    console.log(results);
});//TODO: err handling: what if one request is not sent?

router.use(bodyParser.json());

/*req to be an observer of data change*/
router.post("/dataObserve", function (req, res) {
    console.log("get post /dataObserve");
    console.log(req.body);
console.log("get initial data: "+ req.body.getInitData);

if(req.body.url){//TODO: check if valid url

    if(req.body.register) {
        console.log("register " + req.body.url);
        changeWatcher.register(req.body.url);
    } else if(req.body.deregister){
        if(!changeWatcher.deregister(req.body.url)){
            res.status(400).send("this url is not registered");
        }
    }
    }

    if(req.body.getInitData){//also req to get initial data from this node db?
        //return with init data
        console.log("now get initial data ");

      //  res.sendFile(config.fileLocation);
      //initial data
       db.getData((err, results)=>{
           if(err){
               res.status(500).send({error: err});
               return;
           }
           res.status(200).json({data:results, name:config.fileName});


       });




    } else {
        //return a simple success
        res.status(200).send("success");
    }


});


module.exports = router;