/**
 * Created by Shaocong on 6/7/2017.
 * Try changewatcher module
 */
var watcherConfig =  require("./util/changeWatcher");


var watcher  =watcherConfig({location : __dirname + "/test.xml"});

watcher.register("http://localhost:3000/change");