var express = require('express');
var router = express.Router();

router.get('/:model', function(req, res, next) {
    res.render('jpsShip'); //render the view with this value
});
module.exports = router;