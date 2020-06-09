var express = require('express');
var router = express.Router();

router.get('/:model', function(req, res, next) {
    // here we have episode or fish in req.params.model
    res.render('jpsShip'); //render the view with this value
    next();
});
router.get('/', function(req, res, next) {
    // here we have episode or fish in req.params.model
    res.send('jpsShip'); //render the view with this value
    next();
});
module.exports = router;