var express = require('express');
var router = express.Router();
var connectionsReader = require("../agents/fileConnection.js");

/* GET users listing. */

var conns;

router.get('/', function(req, res, next) {


if(!conns){
  connectionsReader({depth : 0}, function (err, results) {

    if(err){
      res.status(500).send(err);
	  console.log(err);
      return;
    }

    console.log("read connections");

    //res.setHeader('Content-Type', 'application/json');
    //res.json(results);//for testing
	conns = results;
    res.render('visual', { result: JSON.stringify(results) }); //render the view with this value


  });
} else {
	    res.render('visual', { result: JSON.stringify(conns) }); //render the view with this value
}
});


router.get('/includeImport', function(req, res, next) {


  connectionsReader({ showImport : true}, function (err, results) {

    if(err){
		console.log(err);
      res.status(500).send(err);
    }

    console.log("read connections");

    //res.setHeader('Content-Type', 'application/json');
    // res.json(results);
    res.json(results); //render the view with this value


  });
});


router.get('/showServiceOnly', function(req, res, next) {

    connectionsReader({ showServiceOnly : true}, function (err, results) {

        if(err){
            res.status(500).send(err);
        }

        console.log("read connections");
        //res.setHeader('Content-Type', 'application/json');
        // res.json(results);
        res.json(results); //render the view with this value


    });


});

module.exports = router;
