/**
 * This module is a common module that implements cache logic
 */
//TODO: test this

const redis = require('redis');
const REDIS_PORT = process.env.REDIS_PORT || 6379;
const client = redis.createClient(REDIS_PORT);
const express = require('express');



function CacheRouter(router) {
    if(!router){
        throw new Error("No router to be wrapped")
    }

    /*default options for post/get function*****/
   const defaultOpts = {//default options
        expiredTime : 30,
        req2args : (req)=>{return null},
        sendResult : (result, res)=>{res.json(result)}
    };

    /***
     * Utility: set cache
     * @param name  key to store
     * @param time  expire time
     * @param value value to store
     */
    function setCache(name, time,value) {
        client.setex(name, time, value);
    }

    /**
     * Utility: retrieve data with specific func then set cache
     * @param getDataFn   function to retrieve data, return cb(err, result)
     * @param time
     * @param keys
     */
    function  getDataSetCache(getDataFn, time, ...keys) {
        //TODO: make it take args list, not time
        getDataFn((err, result)=>{
            if(err){
                return;
            }
            let name = args2Key(keys);
            //TODO: this name...
            setCache(result, name, time);
        }, ...keys)
    }

    /**
     * Utility: retrieve cache from store
     * args: ...keys, cb
     */
    function getCache(cb, ...args) {
        let key = args2Key(args)
        console.log(key)
        client.get(key, cb);
    }

    /**
     * Utility: convert list of args to cache key
     * @param argsList
     * @returns {*|string|Socket}
     */
    function args2Key(argsList) {
        return argsList.join('');
    };


    /**
     * Utility:handler factory to create normal/cache handler for router
     * @param req2args  function(req): extract from http req args to supply to get data function(i.e. id for a record)
     * @param sendResult function(result, res): take result frpm get data func to presentation supplied to htto res
     * @param getData  function(function(err, result), ...args): retreive data anyhow, with a cb to call and a list of args
     * @returns {Handler} function(req, res, next) a router handler
     * @constructor
     */
    function HandlerFact(req2args, sendResult, getData) {

        let Handler = function (req, res, next) {
            let args = req2args(req);

            if(args.constructor !== Array){
                args = [args];
            }
            console.log(args);
            getData(resultCB, ...args);
             function resultCB(err, result) {
                if(err){
                    next(err)
                    return;
                }
                if(result!= null){
                    sendResult(result, res);
                }else{
                    next(new Error("data is null"));
                }
            }
        }
       return Handler;
    };


    let post = function (path, dataFn, opts) {
        let mOpts = Object.assign({}, defaultOpts, opts);
        let time = mOpts.expiredTime;

        let normalHandler = HandlerFact(mOpts.req2args, mOpts.sendResult, getDataSetCache.bind(null,dataFn, time));
        let cacheHandler = HandlerFact(mOpts.req2args, mOpts.sendResult, getCache)
        router.post(path, cacheHandler, normalHandler)
    }

    let get = function (path, dataFn, opts) {
        let mOpts = Object.assign({}, defaultOpts, opts);
        let time = mOpts.expiredTime;

        let normalHandler = HandlerFact(mOpts.req2args, mOpts.sendResult, getDataSetCache.bind(null,dataFn, time));
        let cacheHandler = HandlerFact(mOpts.req2args, mOpts.sendResult, getCache)
        router.get(path, cacheHandler, normalHandler)
    }

    return {post, get}

}

module.exports  = CacheRouter;
