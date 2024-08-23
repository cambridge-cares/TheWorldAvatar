/**
 * Created by Shaocong on 6/13/2017.
 */
var db = require('../db')();


db.getData(function (err, results) {

    console.log(results);
})