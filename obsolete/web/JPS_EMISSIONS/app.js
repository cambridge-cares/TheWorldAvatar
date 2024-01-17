// Libraries
const express = require('express');
const path = require('path');
const favicon = require('serve-favicon');
const logger = require('morgan');
const cookieParser = require('cookie-parser');
const bodyParser = require('body-parser');

// Routers
const emissionAgentRouter = require('./routes/emission-agent');

//
const app = express();

// View Engine
app.set('views', path.join(__dirname, 'views'));
app.set('view engine', 'pug');

// uncomment after placing your favicon in /public
//app.use(favicon(path.join(__dirname, 'public', 'favicon.ico')));
app.use(logger('dev'));

// Body Parser Middleware
app.use(bodyParser.json());
app.use(bodyParser.urlencoded({extended: false}));

app.use(cookieParser());

// Set Static Path
app.use(express.static(path.join(__dirname, 'public')));

app.get('/JPS_EMISSIONS', (req, res) => {
    res.sendFile(path.join(__dirname, 'public/index.html'));
});

app.use('/JPS_EMISSIONS', emissionAgentRouter);

// Catch 404 and forward to error handler
app.use((req, res, next) => {
    let err = new Error('Not Found');
    err.status = 404;
    next(err);
});

// Error handler
// Must be called after all other app.use() and routes calls
// The last middleware in the request handling process
app.use((err, req, res, next) => {
    // set locals, only providing error in development
    res.locals.message = err.message;
    res.locals.error = req.app.get('env') === 'development' ? err : {};

    // render the error page
    res.status(err.status || 500);
    // res.render('error');
    // console.log(err.stack);
    // res.status(500).send('An error occurred.');
});

app.listen(81, () => {
   console.log('Example app listening on port 81!');
});

module.exports = app;