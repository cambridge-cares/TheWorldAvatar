/**
 * Created by Shaocong on 6/12/2017.
 */
var fs = require('fs');

var watcherConfig = require("../util/changeWatcher");
var testFile = __dirname + "/test.txt";
var changeWatcher = watcherConfig({location:testFile, contentType:"text/plain"});

