/**
 * This module is a common module that implements cache logic
 *  A cache is a temporary store of a piece of retrieved data, so next time it is requested, we need not to go through original DB retrieving/ calculation again.
 *  This cache module is based on express middleware.
 *  The store used is Redis. Needs Redis server to use.
 *  The key is currently generated by simple joins the path and every parameters supplied.
 *  Express router has a next parameter, which basically delegates the processing to next middleware, in a chain kind of logic.
 *  We utlizes that feature by using cache before normal handler, so if cache can not find a record, we go to normal retriever
 *  it also dissects a normal express router into three steps(because two are shared both by normal/cache handler, We dissects it to be three functions, so we can write a unified module for both kind of handlers
 )
 *  The three steps are:
 *  1. extract params  from http request
 *  2. supply params to a function that gets the data, however the specific way
 *  3. put the retrieved data to representation, send out through http response
 *  As we can see, 1 and 3 are completely the same for both cache and normal handler, the only difference happens in step2, that is the retriever function
 */
//TODO: test this

const redis = require('redis');
const REDIS_PORT = process.env.REDIS_PORT || 6379;
const client = redis.createClient(REDIS_PORT);
const express = require('express');


/***
 * Module:
 * a wrapper with methods to add path to a router with cache
 * @param router router to add this new path with cache
 * @returns {{post: post, get: get}} functions, like normal get/post of express, but implements a cache middleware as well
 * @constructor
 */
function CacheRouter(router) {
    if(!router){
        throw new Error("No router to be wrapped")
    }

    /*default options for post/get function*****/
   const defaultOpts = {//default options
        expiredTime : 36,
        req2args : (req)=>{return []},
        sendResult : (result, res)=>{res.json(JSON.parse(result))}
    };


    /**
     * Private Utility: retrieve data with specific func then set cache
     * @param getDataFn   function to retrieve data, return cb(err, result)
     * @param time
     * @param keys
     */
    function  getDataSetCache(path, getDataFn, time, cb,...keys) {
        console.log(arguments)
        console.log(getDataFn)
        console.log(keys)
        console.log(time)
        getDataFn((err, result)=>{
            if(err){
                cb(err);
                return;
            }
            let name = args2Key( path, getDataFn.name, keys);


            //TODO: redis only takes string/hash, so we have an extra stringify step here, be careful as it migth cause complexities
            if(typeof  result+"" !== "string"){
                result = JSON.stringify(result)
            }
            console.log(result)
            console.log("SET CACHE key:" + name)
            client.setex(name, time, result);

            cb(null, result)
        }, ...keys)
    }

    /**
     * Private Utility: retrieve cache from store
     * args: ...keys, cb
     */
    function getCache(path, fnname, cb, ...args) {
        let key = args2Key( path, fnname,args)
        console.log("use key to get cache:")
        console.log(key)
        client.get(key, cb);
    }

    /**
     * Private Utility: convert list of args to cache key
     * @param argsList
     * @returns {*|string|Socket}
     */
    function args2Key( path, fnname, args) {
        return Array.from(arguments).join('');
    };


    /**
     * Private Utility:handler factory to create normal/cache handler for router
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

            console.log("Args: ")
            console.log(args)
            getData(resultCB, ...args);
             function resultCB(err, result) {
                if(err){
                    console.log(err)
                    next(err)
                    return;
                }
                if(result!= null){
                    sendResult(result, res);
                }else{//proceed to cache
                    console.log("not found")
                    next();
                }
            }
        }
       return Handler;
    };


    /**
     * Public method
     * a wrapped express post function
     * @param path   path of router
     * @param dataFn  dataFn called originally to retreive data
     * @param opts   opts. see defaultOpts for available opts: [req2args, expiredTime, sendResult]
     * @returns router
     */
     function post(path, dataFn, opts) {
        console.log("post func")
        let mOpts = Object.assign({}, defaultOpts, opts);
        let time = mOpts.expiredTime;

        let normalHandler = HandlerFact(mOpts.req2args, mOpts.sendResult, getDataSetCache.bind(null,path, dataFn, time));
        let cacheHandler = HandlerFact(mOpts.req2args, mOpts.sendResult, getCache.bind(null, path, dataFn.name))
        router.post(path, cacheHandler,normalHandler )
        return router
    }
    /**
     * Public method
     * a wrapped express get function
     * @param path   path of router
     * @param dataFn  dataFn called originally to retreive data
     * @param opts   opts. see defaultOpts for available opts
     * @returns router
     */
     function get(path, dataFn, opts) {
        let mOpts = Object.assign({}, defaultOpts, opts);
        console.log("options:")
        console.log(mOpts)
        console.log("Call funct: ")
        console.log(dataFn.name)
        let time = mOpts.expiredTime;

        let normalHandler = HandlerFact(mOpts.req2args, mOpts.sendResult, getDataSetCache.bind(null,path, dataFn, time));
        let cacheHandler = HandlerFact(mOpts.req2args, mOpts.sendResult, getCache.bind(null, path, dataFn.name))
        router.get(path,cacheHandler, normalHandler )
        return router
    }

    return {post, get}

}

module.exports  = CacheRouter;