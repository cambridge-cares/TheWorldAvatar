var express = require('express');
var router = express.Router();
var connectionsReader = require("../fileConnection.js");

/* GET users listing. */



router.get('/', function(req, res, next) {


  connectionsReader({depth : 0}, function (err, results) {

    if(err){
      res.status(500).send(err);
    }

    console.log("read connections");
    for(var con of results){
      console.log("\nS:"+con.source+"\nT:"+con.target);

    }
    //res.setHeader('Content-Type', 'application/json');
   // res.json(results);
    res.render('visual', { result: JSON.stringify(results) }); //render the view with this value


  });
});


router.get('/includeImport', function(req, res, next) {


  connectionsReader({depth :0, showImport : true}, function (err, results) {

    if(err){
      res.status(500).send(err);
    }

    console.log("read connections");
    for(var con of results){
      console.log("\nS:"+con.source+"\nT:"+con.target);

    }
    //res.setHeader('Content-Type', 'application/json');
    // res.json(results);
    res.json(results); //render the view with this value


  });
});

module.exports = router;
