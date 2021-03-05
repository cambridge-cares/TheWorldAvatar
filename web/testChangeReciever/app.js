/**
A Prototype setup

 */
var express = require('express');
var app = express();
var bodyParser = require('body-parser');
var request = require('request');

var util = require('util');
var path = require('path');



/*view engine setup*/
app.set('views', path.join(__dirname, 'views'));
app.set('view engine', 'pug');
app.use(bodyParser.text({ type: 'text/xml' }));
app.use(express.static(path.join(__dirname, 'public')));



/*posting to dataObserve to get orginal data & register for future data change*/

var dataCopy = null;

function register(callback) {
    request.post({url: "http://localhost:2000/dataObserve" ,body: JSON.stringify({url:"http://localhost:3000/change", getInitData:true}),headers: {
        'content-type': 'application/json'
    }}, function (err,res ,body) {

        console.log("posting to dataObserve localhost 2000");

        if(err){
            console.log(err);
            callback(err);
            return;
        }
        console.log("intial body");
        console.log(body);
        dataCopy = body;
        callback(null, body);
    });

}





app.get("/", function (req, res, next) {

    //TODO: render initial dat
    res.sendFile(__dirname + '/views/bmsPlot.html');






});


/*future data change will be post to this route*/
app.post("/change", function (req, res) {
    //retreive changed data
    //do whatever you need to do with this data
    //now we only record it down

    console.log("update body");
console.log(util.inspect(req.body));

//console.log(req.header('content-type'));
//TODO: socket, send this info to all clients
    dataCopy = req.body;
    io.emit("update", req.body);
    res.status(200).send("success");
});

// catch 404 and forward to error handler
app.use(function(req, res, next) {
    var err = new Error('Not Found');
    err.status = 404;
    next(err);
});
// error handler
app.use(function(err, req, res, next) {
    // set locals, only providing error in development
    res.locals.message = err.message;
    res.locals.error = req.app.get('env') === 'development' ? err : {};

    // render the error page
    res.status(err.status || 500);
    res.render('error.pug');
});

var http = require('http').Server(app);
var io = require('socket.io')(http);

/*socket io***/
io.on('connection', function(socket){


    if(dataCopy === null){

        register(function (err, initialData) {
            if(err){
                console.log(err);
                return;//TODO: err handling
            }

            socket.emit("initial",initialData);
            dataCopy = initialData;
            console.log('a user connected');

        })


    } else {
        socket.emit("initial", dataCopy);
        console.log('a user connected');
    }
});

var port = 3000;
http.listen(port, function () {
    console.log('Server listening on port '+ port);
});
