
var express = require('express');
var router = express.Router();

router.get('/', function (req, res, next) {
        res.sendfile('index.html'); //render the view with this value
});


module.exports = router