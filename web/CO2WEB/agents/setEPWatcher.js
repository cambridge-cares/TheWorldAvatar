const EventEmitter = require('events');
const endpointChangeEmitter = require('./endpointChangeEmitter')
class Ev extends EventEmitter {}



function setEpWatcher(){
    "use strict";
    let watchEvent = new Ev()
    
    function informSocket(data, observers, callback) {
        //console.log("inform")
       // console.log(data.length)
        try{//all nodes are local, we use event emitter to inform file change
            watchEvent.emit('new', data);
            
        }catch(err){
            console.log('informSocket error')
            callback(err)
            return;
        }
        callback(null,"success");
    }
    
    const epChangeEmitter = new endpointChangeEmitter(informSocket)
    epChangeEmitter.listen(1200000)
    
    return {watchEvent, epChangeEmitter}
}

//
module.exports = setEpWatcher

//logic: connect front end
