/**
 * Created by Shaocong on 9/8/2017.
 * By principle one node only own one data file and its own service, but
 * currently we host nodes together, so a group watcher is required to watch an entire folder to
 * see waht is changed
 */

var chokidar = require('chokidar');
var log = console.log.bind(console);
var async = require('async')
var fs = require('fs')
var path = require('path')



/***
 * Module that watches over an directory/group of files
 * @param dir
 * @param informIndi a callback function with arguments : (data, observer ,callback)   data is the data informed by watcher, observer is the observer name registered, callback is a callback(err[, result]) to inform watcher if it is correct
 * @returns {{register: register}}
 */
function groupwatcher(dir, informIndi){
    console.log("groupwatcher")
    var watchDogs = new Map(); //map of watchdogs

    var watcher = chokidar.watch(dir, { //watch over specific dir
        //ignored: /^((?!\.owl).)*$/,  //ignore everything does not has .owl in their name
        ignored :/.*\.txt$/,
        persistent: true
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
            console.log("dog for " + this.uri+" registers " + newObserver)
            this.observers.set(newObserver, {name: newObserver, receiveData: receiveData});
            this.observers.forEach(function (item) {
                console.log(item)
            })
        },
        deregister:function (oldObserver) {
            this.observers.delete(oldObserver);
        },
        informAll: function (informIndi) {// execute a individual inform function on all observer


            /*define data to be sent*****/
            let changedFilenames = {uri:this.uri, filename : this.filename};
            //TODO: method to get data:bmsDataReader
            let withData = {}
            /*define single inform with data**/
            function informWithData(observer, callback){
                console.log("!!!!")
                console.log(observer)
                var mobserver = observer[1].name;

                var data = changedFilenames;
                if(observer.receiveData){
                    data["data"] = withData;
                }
                informIndi(data, mobserver, callback);
            }

            /*call inform on all observer*****/
            if(this.observers.size > 0){
                console.log("Call concat")
                async.concat(this.observers, informWithData,//send modified file to each observer
                    function (err, results) {
                        if (err) {
                            console.log(err);
                            return;
                        }
                        console.log("finish informing")
                        if(results){
                            console.log(JSON.stringify(results));
                        }
                    });

            }
        },
        getObservers : function () {
            return this.observers
        }

    }
    //The main argument here is when to create a watchdog for file?
    //1.when user specifically requires it 2.watch over every file then
    //TOOK  2

    //create watchdog for every owl file in dir
    var files = fs.readdirSync(dir);
    console.log(files);
    var owlfiles = files.filter((file)=>{return file.match(/^.*\.owl$/)});

    owlfiles.forEach(function (owlfile) {
        let targetPath = path.join(dir, owlfile);
        setWatch(owlfile);
    })


    watcher
        .on('change', function(filepath) {//when dir changed
            log('File ', filepath, 'has been changed');
            if(watchDogs.has(filepath)){ //check if this file is required to be watched
                console.log("Ask watchdog to inform")
                let watchDog = watchDogs.get(filepath);
                watchDog.informAll(informIndi);
            }
        })

    function setWatch(filename) {
        let targetPath = path.join(dir, filename)
        var newdog = new WatchDog(targetPath, filename);
        watchDogs.set(targetPath, newdog);
    }

    function registerAll(observerUri, receiveData) {
        console.log("register all")
        owlfiles.forEach(function (file) {
            let targetPath = path.join(dir, file);
            console.log("ergister for " + targetPath)
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