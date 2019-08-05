/***
wrapper module for  rabbitmq listener
***/
var RabbitMQ = require('rabbitmq-node');

var rabbitmq = new RabbitMQ({'url':'amqp://guest:guest@www.theworldavatar.com:5672'});

var msgWatcher = {};

/***
init function
define listeners and start subscribing
***/
msgWatcher.init = function (io) {
    
    let self = this

    rabbitmq.on('message', function(message) {
        console.log(message);
        //when get message, send message to browser thru io to all subscribers
        var a = self.packMsg(message);
        io.emit("agentevent", a);
    
    
    });
    
    rabbitmq.on('error', function(err) {
        console.log('err:')
        console.error(err);
    });
    
    rabbitmq.on('logs', function(print_log) {
        console.info(print_log);
    });
    
    rabbitmq.subscribe('jps.agents');
     
     }
     
/***
pack message into format
***/
msgWatcher.packMsg = function (msg) {
  //  console.log(msg['data']['agent iri'])
    return {agentId :msg['data']['agent iri'], plantId:msg['data']['plant'],scenarioId:msg['data']['scenario id']}
//return msg;
}


module.exports = msgWatcher;