/**
 * Created by Shaocong on 9/4/2017.
 */
var saxStream = require('sax-stream');

var request =require('request')

request('http://blog.npmjs.org/rss')
    .pipe(saxStream({
        strict: true,
        tag: 'item'
    }))
        .on('data', function(item) {
            console.log(item);
            console.log("__________________________")
        });