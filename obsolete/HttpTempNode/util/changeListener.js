/**
 * Created by Shaocong on 4/17/2017.
 * Listener for data change, inherits from nodejs eventemitter
 */

var EventEmitter = require('events').EventEmitter;
var util = require('util');
function EventCtrl() {  EventEmitter.call(this)};

util.inherits(EventCtrl, EventEmitter);

var eventCtrl ;

function  getChangeListener() {
    eventCtrl = eventCtrl || new EventCtrl();


    function emitChange(data){
        eventCtrl.emit("change", data);
    }

    return{eventCtrl, emitChange};
}





module.exports = getChangeListener;