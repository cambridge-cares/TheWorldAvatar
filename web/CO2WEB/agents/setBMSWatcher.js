/**
 * This module utlizes the common chageWatcher module that sets some specific params in our use case.
 * Set parameters for watcher for bms, including a temporary
 * inform funciton using eventEmitter since they are not set separated.
 */
var log4js = require('log4js');
var logger = log4js.getLogger();
logger.level = 'debug';

const changeWatcher = require('./groupChangeWatcher')
const EventEmitter = require('events');
const config = require('../config')
const bmsFolder = config.bmsFolder;
const fs = require('fs')
const path = require('path')
class Ev extends EventEmitter {}

/**
main func
**/
function setBMSWatcher() {
    //define a inform function
    var watchEvent = new Ev(); //an eventemitter as informer

    function informIndi(data, observer, callback) {
        logger.debug("inform")
        try{//all nodes are local, we use event emitter to inform file change
            watchEvent.emit('change', data);

        }catch(err){
            callback(err)
            return;
        }
        callback(null,"success");
    }

    var bmsWatcher =  changeWatcher(config.root, informIndi);//construct a new watcher for us

    bmsWatcher.registerAll("worldnode", false);
    //register a special data for plotting
    //bmsWatcher.register(config.bmsplotnode,"worldnode", true);


    return {watchEvent, bmsWatcher};
}

module.exports = setBMSWatcher;