/**
 * Created by Shaocong on 9/8/2017.
 * Set parameters for watcher for bms, including a temporary
 * inform funciton using eventEmitter since they are not set separated.
 */

const changeWatcher = require('./groupChangeWatcher')
const EventEmitter = require('events');
const config = require('../config')
const bmsFolder = config.bmsFolder;
const fs = require('fs')
const path = require('path')
class Ev extends EventEmitter {}

function setBMSWatcher() {
    //define a inform function
    var watchEvent = new Ev(); //an eventemitter as informer

    function informIndi(data, observer, callback) {
        console.log("inform")
        try{//all nodes are local, we use event emitter to inform file change
            watchEvent.emit('change', data);

        }catch(err){
            callback(err)
            return;
        }
        callback(null,"success");
    }

    var bmsWatcher =  changeWatcher(bmsFolder, informIndi);//construct a new watcher for us

    bmsWatcher.registerAll("worldnode", false);
    //register a special data for plotting
    //bmsWatcher.register("","worldnode", true);


    return watchEvent;
}

module.exports = setBMSWatcher;