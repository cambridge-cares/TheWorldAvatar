/**
 * Created by Shaocong on 6/6/2017.
 */
/**
 * This is a interface to:
 * watch change on a file in node
 * when file changed, notify this change to all register url by post to these urls
 *
 */

var fs = require("fs");
var express = require('express');
var request = require('request');
var async = require('async');
var config = require('../config');
var db = require('../dbtest')();

//var loc = __dirname+'/test.xml';

function watcher(options) {

    let self = this;
    self.observers = [];//register as  an observer/informee
    self.results = {};//informee results
    let fileLoc = options.location;
    let contentType = options.contentType;

    /**
     * Main function. start watch on file
     * @param listener: fired each time update information has been sent to informees
     */
    self.setWatch = function (listener) {
        fs.watchFile(fileLoc, (curr, prev) => { //initiate watch file change
                //time has truly elapsed //now you know file is changed
                if (curr.mtime.getTime() !== prev.mtime.getTime()) {//
                    //send the file directly to it
                    console.log("file change @ " + fileLoc);
                    /*send modified file data to registered url*/
                    db.getData(function (err, data) { // get data from database

                        if (err) {
                            listener(err);
                            return;
                        }
                        console.log("observers num: " + observers.length);
                        ;
                        if (observers.length > 0) {
                            async.concat(observers, inform,//send modified file to each observer

                                function (err, results) {
                                    if (err) {
                                        console.log(err);
                                        // if any of the saves produced an error, err would equal that error
                                        listener(err);
                                        return;
                                    }
                                    listener(null, results); //send processing results: if success or not
                                });


                            function inform(informee, callback) {

                                request.post({//http post to informee url
                                    url: informee, body: JSON.stringify({name:config.fileName, data: data}), headers: {
                                        'content-type': contentType
                                    }
                                }, function (err, response, body) {

                                    if (err) {

                                        callback(null, {informee: informee, result: err.message});
										return;
                                        // throw err;
                                    }

                                    console.log({informee: "success"});

                                    callback(null, {informee: informee, result: "success"});
                                });
                            }

                        }
                    })
                }
            }
        );
    };


    /*Register a new observer*/
    self.register = function (newObserver) {

        self.observers.push(newObserver);
    }
    /*Deregister an existing observer*/
    self.deregister = function (observer2Delete) {
        let success = false;
        for (let i = 0; i < self.observers.length; i++) {
            if (observers[i] === observer2Delete) {
                observers.splice(i, 1);
                return true;
            }
        }

        if(!success){
            console.log("can not find this url");//TODO: err handling
            return false;
        }
    }


    /*return all current observers*/
    self.getRegistered = function () {
        return self.observers;
    }

    return {register: self.register, setWatch: self.setWatch, getRegistered: self.getRegistered}
}

module.exports = watcher;

