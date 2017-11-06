/**
 * Created by Shaocong on 4/17/2017.
 * Listener for data change, inherits from nodejs eventemitter
 * This class should be used whenever a change to data happens and need to notify front=end
 * Any function listens to change event should listen to this event
 */

var EventEmitter = require('events').EventEmitter;
var util = require('util');
function EventCtrl() {  EventEmitter.call(this)};

util.inherits(EventCtrl, EventEmitter);

var eventCtrl ;

function  getChangeListener() {
    eventCtrl = eventCtrl || new EventCtrl();//lazy init


    /**
     * emit a change event
     * @param data   changed data
     */
    function emitChange(data){
        eventCtrl.emit("change", data);
    }

    return{eventCtrl, emitChange} //return change emitter itself and a function to emit change;
}





module.exports = getChangeListener;