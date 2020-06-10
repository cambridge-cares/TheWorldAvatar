/**
 * This module utlizes the common chageWatcher module that sets some specific params in our use case.
 * Set parameters for watcher for bms, including a temporary
 * inform funciton using eventEmitter since they are not set separated.
 */
var log4js = require('log4js');
var logger = log4js.getLogger();
logger.level = 'debug';

const EventEmitter = require('events');
const config = require('../config')
const bmsFolder = config.bmsFolder;
const fs = require('fs')
const path = require('path')
var chokidar = require('chokidar');
var fsre = require('fs-readdir-recursive')
const changeInformer = require('./groupChangeWatcher')
var LiteralData = require('../agents/GetLiteralData');

class Ev extends EventEmitter {}

/**
main func
**/
const watchDir = config.root


function setBMSWatcher() {
    //define a inform function
    var watchEvent = new Ev(); //an eventemitter as informer

    function informIndi(data, observer, callback) {

		if(data!==null && ('data' in data)){
		}
        try{//all nodes are local, we use event emitter to inform file change
            watchEvent.emit('update', data);

        }catch(err){
            callback(err)
            return;
        }
        callback(null,"success");
    }
    
    var FileResource = function (uri, filename) {
        this.uri = uri
        this.filename = filename
    }
    FileResource.prototype = {
        sendData: function (dataSwitch) {
            let withChangeData = {};
            let changedFilenames  ={uri:this.uri, filename : this.filename};
    let self = this;
            function getDataP() {
                return new Promise(function (resolve, reject) {
            
                    LiteralData(function (err, data) {
                        if(err){
                            reject(err);
                        }
						console.log('literal data')
                        console.log(data)
                        resolve({data});
                    }, self.uri);
            
            
                });
            }
           return new Promise(function (resolve, reject) {
            if(dataSwitch){//has data request,
                let  dataPromise = getDataP();
                dataPromise.then(function (promisedData) {
                    Object.assign(withChangeData, changedFilenames, promisedData);
                    resolve([changedFilenames,withChangeData ]);
                })
            } else{//just filenames
                resolve([changedFilenames,withChangeData ]);
            }
            })
        }
    }
    
    
    
    var fileWillChange = chokidar.watch(watchDir, { //watch over specific dir
        ignored: /(\.(?!owl))+/,  //ignore everything does not has .owl in their name
        // ignored :/.*\.txt$/,
        persistent: true,
        awaitWriteFinish: true,
        ignoreInitial : true,
        alwaysStat:true,
        followSymlinks: false,
        usePolling: true
    });
    
    var bmsWatcher =  changeInformer(fileWillChange, informIndi);//construct a new watcher for us
    
    
    //set changes for each file in the dir
    var owlfiles = fsre(watchDir);
    owlfiles = owlfiles.filter((name)=>{return name.match(/^.*\.owl$/)})
    logger.debug(owlfiles);
    
    owlfiles.forEach(function (name) {
        let uri = path.join(watchDir, name);
        bmsWatcher.setWatch(uri, new FileResource(uri, name));
    })
    
    function registerAll(observerUri, receiveData) {
        logger.debug("register all")
        owlfiles.forEach(function (file) {
            let targetPath = path.join(watchDir, file);
            bmsWatcher.register(targetPath, observerUri, receiveData)
        })
    }
    
    registerAll("worldnode", false);
    
    
    //emits change whenever end point is changed

    //register a special data for plotting
    //bmsWatcher.register(config.bmsplotnode,"worldnode", true);


    return {watchEvent, bmsWatcher};
}

module.exports = setBMSWatcher;