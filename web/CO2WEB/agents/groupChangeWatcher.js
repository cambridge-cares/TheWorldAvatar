/**
Module that watches over an directory/group of file, and will inform anyone who has registered for any of the file with the modified(the modification can come from any sources, independent of this module) data.

 This module abstracts the change-inform  logic from any other services, with this module, this procedure needs not to be hard-coded in every module that induces a change in the owl file, but rather, the change will be detected with the file by this module, and sending out the message to any other module that should be aware.

 *Note: By principle one node only own one data file and its own service, but
 * currently we host nodes together, so a group watcher is required to watch an entire folder to
 * see what is changed
 *
 * Note: currently the data each time informed are uri, filename, and all the literal data, the latter is retrieve by the GetLiteral module, this in the future should probably be changed. One possible way is to delegate it to user, so user can specify which module they want to use for retrieving data.
 *
 * Note: deregister logic is not yet implemented.
 */
var log4js = require('log4js');
var logger = log4js.getLogger();
logger.level = 'debug';

var async = require('async')
var path = require('path')



/***
 * Module that watches over an directory/group of files
 * @param dir
 * @param informIndi a callback function with arguments : (data, observer ,callback)   data is the data informed by watcher, observer is the observer name registered, callback is a callback(err[, result]) to inform watcher if it is correct
 * @returns {{register: register}}
 */
function changeInformer(thingWillChange, informIndi){
    logger.debug("changeInformer")
    var watchDogs = new Map(); //map of watchdogs
    
    function WatchDog(resources){
        this.resources = resources;
        this.observers = new Map(); //observer list
		this.timer = null
    }

    WatchDog.prototype = {
        register: function (newObserver, receiveData) {//register this observer
           // logger.debug("dog for " + this.uri+" registers " + newObserver)
            if (!(newObserver in this.observers.keys())){
            this.observers.set(newObserver, {name: newObserver, receiveData: receiveData});}
           // logger.debug("register for "+this.uri);
        },
		
		setTimer: function(time){
			let self  = this;
			this.timer = setTimeout(function(){self.timer  = null;},time)
		},
		checkTimerCanRun: function(){
		if(this.timer === null){
				return true;
			}
			return false;
		},

        informAll: function (informIndi) {// inform all observer
            const self = this;
            dataPromise = this.resources.sendData(this.hasDataRequestObserver())
            dataPromise.then(function (promisedData) {
				console.log('observers')
              console.log(self.observers)
                //console.log('informer get promised data, '+promisedData[1].length)
                loopInform(promisedData);
            }).catch(e=>{
                "use strict";
                console.log(e)
            })
                function loopInform(promiseData) {
                changedFilenames = promiseData[0]
                    withChangeData = promiseData[1]
                    /*define single inform with data**/
                    function informWithData(observer, callback){
                        // logger.debug("!!!!")
                        // logger.debug(observer)
                        var mobserver = observer[1].name;
						if(mobserver.includes('data'))
        console.log('informing: '+mobserver)
                        var data = changedFilenames;
                        if(observer[1].receiveData){
                            data = withChangeData;
                        }
                       // console.log('data in informAll:'+data.length);
                        informIndi(data, mobserver, callback);
                    }
                    //console.log(self.observers)
                    /*call inform on all observer*****/
                    if(self.observers.size > 0){
                      //  logger.debug("Call concat")
                        async.concat(self.observers, informWithData,//send modified file to each observer
                            function (err, results) {
                                if (err) {
                                    logger.debug("Can not read changed data:" + err);
                                    return;
                                }
                                //console.log('finish informing')
                                logger.debug("finish informing")
                                if(results){
                                    //logger.debug(JSON.stringify(results));
                                }
                            });

                    }
                }
        },
        hasDataRequestObserver : function () {
            let hasDRO = false;

            this.observers.forEach(function (observer) {
              // console.log("!!!!!!!!!!!!!!")
               // console.log(observer)
                if(observer.receiveData){
                   hasDRO = true;
                }
            })
            return hasDRO;
        }

    }
    
    
    thingWillChange
        .on('change', (register_name, stats)=>{//when dir changed
            //console.log('File '+ register_name+' has been changed');
            //logger.debug(stats)
            if(watchDogs.has(register_name)){ //check if this file is required to be watched
                //console.log("Ask watchdog to inform")
                let watchDog = watchDogs.get(register_name);
				if(watchDog.checkTimerCanRun()){
                watchDog.setTimer(5000);
				watchDog.informAll(informIndi);
				} else{
					console.log('too freuquent update')
				}
            } else{

            }
        })
        .on('raw', (event, file, details)=>{
          logger.debug(event)
          logger.debug(file)

        })
 
    

    function setWatch(treasureid,treasure) {
        var newdog = new WatchDog(treasure);
        watchDogs.set(treasureid, newdog);
        return newdog;
    }


    
    function register(registerName, observerUri, receiveData) {
        if(watchDogs.has(registerName)){
           var dog = watchDogs.get(registerName);
          dog.register(observerUri, receiveData);
        }
        else{
        }
    }




  return{setWatch, register};


    
}

module.exports = changeInformer