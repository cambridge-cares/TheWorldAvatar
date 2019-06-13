var RabbitMQ = require('rabbitmq-node');

var rabbitmq = new RabbitMQ({'url':'amqp://guest:guest@www.theworldavatar.com:5672'});

var msgWatcher = {};


//todo: delete later

const pl = [
    
    'http://www.theworldavatar.com/kb/powerplants/PowerPlant_Germany_6493bc40-69a8-483a-a39b-4bc2c19abd41.owl',
    'http://www.theworldavatar.com/kb/powerplants/PowerPlant_Austria_c4d88b6a-9cf7-43d7-b600-92bc74ef46d3.owl',
    'http://www.theworldavatar.com/kb/powerplants/Pearsall_Gas_Plant_TX_USA.owl',
    'http://www.theworldavatar.com/kb/powerplants/JSW_Ratnagiri_Coal_Power_Plant_India_01.owl',
    'http://www.theworldavatar.com/kb/powerplants/PowerPlant_France_03e37fb2-11a9-42f1-8a3e-02b085cc1be5.owl',
    'http://www.theworldavatar.com/kb/powerplants/Anhui_Pingwei_Coal_Power_Plant_China.owl',
    'http://www.theworldavatar.com/kb/powerplants/Vallur_Coal_Power_Plant_India_02.owl',
    'http://www.theworldavatar.com/kb/powerplants/Herdecke_CCGT_Power_Plant_Germany.owl',
    'http://www.theworldavatar.com/kb/powerplants/Akita_Thermal_Power_Plant_Japan_04.owl'
    
];
msgWatcher.init = function (io) {
    
    let self = this
    //todo: mock, delete later
/**
    let plidx = 0, sidx = 0;

        "use strict";
    setInterval(()=>{
        
        
        for(let i = 0; i < 4 ; i++) {
            let stud = {agentId: 'fakeagent' + sidx, scenarioId: sidx, plantId: pl[plidx]};
            console.log(stud);
            setTimeout(()=>{
            io.emit("agentevent", stud);
            },3000*(i+1));
            if (plidx++ === pl.length) {
                plidx = 0;
                sidx++;
            }
        }
    },15000);
    
  ***/
    rabbitmq.on('message', function(message) {
        console.log(message);
        //when get message, send message to browser thru io to all subscribers
        //todo: send io
        var a = self.packMsg(message);
//todo: uncom
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
     

msgWatcher.packMsg = function (msg) {
  //  console.log(msg['data']['agent iri'])
    return {agentId :msg['data']['agent iri'], plantId:msg['data']['plant'],scenarioId:msg['data']['scenario id']}
//return msg;
}


module.exports = msgWatcher;