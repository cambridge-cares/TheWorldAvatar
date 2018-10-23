var RabbitMQ = require('rabbitmq-node');

var rabbitmq = new RabbitMQ({'url':'amqp://guest:guest@www.theworldavatar.com:83'});

var msgWatcher = {};

//msgWatcher.init = function (io) {
    
    rabbitmq.on('message', function(message) {
        console.log(message);
        //when get message, send message to browser thru io to all subscribers
        
    });
    
    rabbitmq.on('error', function(err) {
        console.log('err:')
        console.error(err);
    });
    
    rabbitmq.on('logs', function(print_log) {
        console.info(print_log);
    });
    
    rabbitmq.subscribe('jps.agents');
//}







