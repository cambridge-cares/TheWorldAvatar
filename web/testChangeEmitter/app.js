/**
 * Created by Shaocong on 6/7/2017.
 * A prototype set up for a node server with ability to inform other nodes of self data change
 * other node server can subscribe for this node's data change by
 * POST to /dataObserve route with own url+path
 * Each time a change happened to the node's data file
 * Node server will post to all registered url+path with the modified file's full content
 */
var express = require('express');
var app = express();
var config=  require("./config");
var port = config.port;


/*route to register for data change broadcasting**/
var dataObserve = require("./routes/dataObserve");
app.use("/", dataObserve);

function setHeader(res, mpath){
  console.log("path"+ mpath);
  res.setHeader("Content-Type","text/xml");
  res.setHeader("Content-Disposition","inline");
    console.log("SEtting headers");


}
/*serve static data file*/
app.use(express.static(config.filePath, {'setHeaders': setHeader}));



/*start the server**/
app.listen(port, function () {
    console.log('Server listening on port '+ port);
});
