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

var chokidar = require('chokidar');
var async = require('async')
var fsre = require('fs-readdir-recursive')
var path = require('path')

var BMSData =require('./GetBmsDataObsolete');

var LiteralData =require('./GetLiteralData');

/***
 * Module that watches over an directory/group of files
 * @param dir
 * @param informIndi a callback function with arguments : (data, observer ,callback)   data is the data informed by watcher, observer is the observer name registered, callback is a callback(err[, result]) to inform watcher if it is correct
 * @returns {{register: register}}
 */
function groupwatcher(dir, informIndi){
    logger.debug("groupwatcher")
    var watchDogs = new Map(); //map of watchdogs

    var watcher = chokidar.watch(dir, { //watch over specific dir
        ignored: /(\.(?!owl))+/,  //ignore everything does not has .owl in their name
       // ignored :/.*\.txt$/,
        persistent: true,
        awaitWriteFinish: true,
        ignoreInitial : true,
        alwaysStat:true,
        followSymlinks: false,
        usePolling: true
    });

    /**
     * Watch agent constructor, bears basic functionality
     * @param uri  id of watchdog. in online request is also an url, in local version, a disk path
     * @constructor
     */
    function WatchDog(uri, filename){
        this.uri = uri;
        this.filename = filename;
        this.observers = new Map(); //observer list
    }

    WatchDog.prototype = {
        register: function (newObserver, receiveData) {//register this observer
           // logger.debug("dog for " + this.uri+" registers " + newObserver)
            this.observers.set(newObserver, {name: newObserver, receiveData: receiveData});
            logger.debug("register for "+this.uri);

            this.observers.forEach(function (item) {
               // logger.debug(item)
            })
        },
        deregister:function (oldObserver) {
            this.observers.delete(oldObserver);
        },
        informAll: function (informIndi) {// execute a individual inform function on all observer


            /*define data to be sent*****/
            let changedFilenames, withChangeData = {};
            //TODO: method to get data:bmsDataReader
           let self = this;
            changedFilenames  ={uri:this.uri, filename : this.filename};

           function getDataP() {//TODO: this should be delegate to the user instead of defined here
              return new Promise(function (resolve, reject) {

                      LiteralData(function (err, data) {
                          if(err){
                              reject(err);
                          }
                          logger.debug(data)
                          resolve({data});
                      }, self.uri);


               });
           }

    // logger.debug(this.observers);

            if(this.hasDataRequestObserver()){
               let  dataPromise = getDataP();
                dataPromise.then(function (promisedData) {
                    //TODO: merge instead of explicit declare
                    Object.assign(withChangeData, changedFilenames, promisedData);
                    console.log("groupChangeWatcher: sent update data: ")
                    console.log(withChangeData.data);
                    loopInform();
                })
            } else{
               loopInform();
            }



                function loopInform() {
                    /*call inform on all observer*****/
                    if(self.observers.size > 0){
                      //  logger.debug("Call concat")
                        async.concat(self.observers, informWithData,//send modified file to each observer
                            function (err, results) {
                                if (err) {
                                    logger.debug("Can not read changed data:" + err);
                                    return;
                                }
                                logger.debug("finish informing")
                                if(results){
                                    //logger.debug(JSON.stringify(results));
                                }
                            });

                    }
                }

                /*define single inform with data**/
                function informWithData(observer, callback){
                   // logger.debug("!!!!")
                   // logger.debug(observer)
                    var mobserver = observer[1].name;

                    var data = changedFilenames;
                    if(observer[1].receiveData){
                       data = withChangeData;
                    }
                    informIndi(data, mobserver, callback);
                }



        },
        hasDataRequestObserver : function () {
            let hasDRO = false;

            this.observers.forEach(function (observer) {
               console.log("!!!!!!!!!!!!!!")
                console.log(observer)
                if(observer.receiveData){
                   hasDRO = true;
                }
            })
            return hasDRO;
        }

    }
    //The main argument here is when to create a watchdog for file?
    //1.when user specifically requires it 2.watch over every file then
    //TOOK  2

    //create watchdog for every owl file in dir
    //, (file)=>{return file.match(/^.*\.owl$/)}
    var owlfiles = fsre(dir);
    logger.debug("!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!");

    owlfiles = owlfiles.filter((name)=>{return name.match(/^.*\.owl$/)})
    logger.debug(owlfiles);
    owlfiles.forEach(function (owlfile) {
        let targetPath = path.join(dir, owlfile);
        setWatch(owlfile);
    })
   // var owlfiles = files.filter((file)=>{return file.match(/^.*\.owl$/)});
  /**
   var files = fs.readdirSync(dir);
    logger.debug(files);
    var owlfiles = files.filter((file)=>{return file.match(/^.*\.owl$/)});
    owlfiles.forEach(function (owlfile) {
        let targetPath = path.join(dir, owlfile);
        setWatch(owlfile);
    })
***/

    watcher
        .on('change', (filepath, stats)=>{//when dir changed
            logger.debug('File '+ filepath+'has been changed');
            //logger.debug(stats)
            if(watchDogs.has(filepath)){ //check if this file is required to be watched
                logger.debug("Ask watchdog to inform")
                let watchDog = watchDogs.get(filepath);
                watchDog.informAll(informIndi);
            } else{

            }
        })
        .on('raw', (event, file, details)=>{
          logger.debug(event)
          logger.debug(file)

        })
 
    

    function setWatch(filename) {
        let filepath = path.join(dir, filename)
        //TODO:get filename
        var newdog = new WatchDog(filepath, filename);
        watchDogs.set(filepath, newdog);
        return newdog;
    }

    function registerAll(observerUri, receiveData) {
        logger.debug("register all")
        owlfiles.forEach(function (file) {
            let targetPath = path.join(dir, file);
            register(targetPath, observerUri, receiveData)
        })
    }
    
    function register(targetPath, observerUri, receiveData) {
        if(watchDogs.has(targetPath)){
           var dog = watchDogs.get(targetPath);
          dog.register(observerUri, receiveData);
        }
        else{
            //TODO: err no such target
           // var newdog = new WatchDog(targetPath);
           // watchDogs.set(targetPath, newdog);
        }
    }




  return{register, registerAll};


    
}


module.exports = groupwatcher