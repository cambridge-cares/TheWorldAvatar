/**
 */
const EventEmitter = require('events');
class Ev extends EventEmitter {}
const changeInformer = require('./groupChangeWatcher')
const linkReader = require('./fileConnection2Way')
const async = require('async')
const csv = require('csv-parser')
const fs =  require('fs')
var epResourcer =  function (url,qstr) {
    this.url  = url
    this.dataList = []
    this.resourceList= []
    this.qStr = qstr
}

epResourcer.prototype = {
    ///this sets new data
    getqstr: function () {
      return this.qStr
    },
    getUrl: function () {
      return this.url
    },
    getResourceList:function () {
      return this.resourceList
    },
    add2ResourceList:function (iri) {
        this.resourceList.push(iri)
    },
    setData: function(data){
        "use strict";
        this.dataList = data
    },
    getData: function () {
      return this.dataList
    },
    //calls by watchDog's inform function. Send over data one wants to give out
    sendData: function (dataSwitch) {
        return new Promise((resolve, reject)=>{
            "use strict";
            resolve([null, this.dataList])//[data0,data1]
    
        })
    }
}

/**
 *
 * @param callout: callback function that does the inform
 */
var endPointChangeEmitter = function (callout) {
    this.EPchangeEmitter= new Ev()
    this.epNameList = []
    this.epResourcerList= []
    this.informer = changeInformer(this.EPchangeEmitter,callout)
}



endPointChangeEmitter.prototype = {
    listen: function (interval) {
        /**
         * for every interval time, query all endpoints and update result
         * @type {endPointChangeEmitter}
         */
        let me = this;
        console.log('start listening')
        function pollWhileListen() {
                me.poll(function (err, results) {
                    console.log(results)
                    for (idx = 0; idx < results.length; idx++){
                            let parentChildList = results[idx];
                            let subscribers = me.epResourcerList[idx].getResourceList()
                            console.log('subscribers: ', subscribers)
                            let data = [];
                            let parentChildMap = new Map();
                            parentChildList.forEach((item)=>{
                                "use strict";
                                if (!(item.source in parentChildMap)){
                                    parentChildMap.set(item.source, [])
                                }
                                parentChildMap.get(item.source).push(item)
                            })
                        parentChildList.forEach((link)=> {
                                if (subscribers.includes(link.source)){//source is subscirbed
                                    if(parentChildMap.get(link.source).some((item)=> subscribers.includes(item.target))){//this source has some target also subscribed
                                        data.push(link)
                                    }
                                }}
                                
                                )
                            let endpoint = me.epNameList[idx];
                            me.epResourcerList[idx].setData({endpoint, data});
                            //emit a change event to tell watchdog
                            me.EPchangeEmitter.emit('change', me.epNameList[idx]);//event name, watchdog id
                            //when event receives, watchdog will run sendData function of the resource to get data
                        
                    }
                })
            }
        
            setTimeout(pollWhileListen,10000);
            setInterval(pollWhileListen, interval)
    },

    registerSubsriber: function(epUri, resourceIds, qstr, observerUrl){
        "use strict";
        if (!(epUri in this.epNameList)){//=> Resourcer not exists yet
            //create new resourcer, assign a watchdog to guard it
            let resourcer = new epResourcer(epUri, qstr)
            console.log(this.epNameList)
            this.epNameList.push(epUri)
            this.epResourcerList.push(resourcer)
            this.informer.setWatch(epUri, resourcer)
            this.informer.register(epUri, observerUrl,true)
        }
        let idxResourcer = this.epResourcerList.findIndex((resource) => resource.getUrl() === epUri)
        let resourcer2Register = this.epResourcerList[idxResourcer]
        let list = resourcer2Register.getResourceList()
        for (let idR of resourceIds){
            if (!(idR in list)){
                resourcer2Register.add2ResourceList(idR)
            }
        }
 
        console.log('nameList: '+this.epNameList)
        console.log('resourceList '+list)
        },
    
    //todo:inportant!!!!!
    deregister: function(){
    "use strict";
    //when a resource has no more watcher, deregister it
    
},
    
    /**
     * execute single query
     * @param url
     * @param callback
     */
    singlePoll : function (params,callback) {
        "use strict";
        linkReader.processSingle(params).then((results)=>{
            console.log("read connections");
            callback(null, results);
            //console.log('poll results')
            //console.log(results)
        }).catch(err=>console.log(err));
    }
    ,
    /**
     * Loop query for all registered endpoitns
     */
    poll: function (callback) {
        me = this;
            if(me.epNameList.length >0) {
                let params = me.epResourcerList.map(resource=> {return{useSharp:true,topnode:resource.getUrl(), supQuery: resource.getqstr()}})
                console.log('paramters'+params)
            async.map(params, me.singlePoll, function (err, resultArr) {
                if(err) {
                    console.log(err);
                    callback(err)}
                callback(null, resultArr)
            } )
            }
            
    }
    
}


    

module.exports = endPointChangeEmitter;

