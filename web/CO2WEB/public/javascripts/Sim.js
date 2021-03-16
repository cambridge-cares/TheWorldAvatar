/**
 * The PROTOTYPE module that runs simulation through API*/

console.log(async)


function Sim(args){
    this.setOption(args);
    this.getOutputInfo((err)=>{
        if(err){
            console.log(err)
            return;
        }
        this.setSubscribe();
    })
}


Sim.prototype = {
    
    setOption: function (opts) {
        if(!opts.inputMap || !opts.subscribes || !opts.socket || !opts.simPath ){
            throw new Error("Import paras(sim inputvalues | subscribing endpoints | socket |simulation API path) not specified");
        }
        this.inputMap = opts.inputMap;//this is the map of only value

        this.subscribes = opts.subscribes;
        this.socket = opts.socket;
        this.simPath = opts.simPath;

        this.displayInputInit = opts.displayInputInit || null;
        this.displayInputUpdate = opts.displayInputUpdate || null;
        this.displayOutputInit = opts.displayOutputInit || null;

        this.disSimResB4DBUpdateFn = opts.disSimResB4DBUpdateFn || null;
        this.outputUpdateSuccessCBFn = opts.outputUpdateSuccessCBFn || null;
        this.updateOutput2DBFlag = opts.updateOutput2DBFlag || false;
        this.outputMap = opts.outputMap || null;
        this.errorCB = opts.errorCB || function (err) {
                console.log(err)
            };
        this.initInputCounter = 0;
    },
    /**
    set  data update event subscription
    ***/
    setSubscribe: function () {
        let msocket = this.socket;
        console.log(msocket)
        msocket.emit("join", JSON.stringify(this.subscribes));


        msocket.on('initial',  (idata)=>{//when receive initial data
            console.log(idata);
            this.initInputCounter++;
            this.updateInputs(idata);
    //        this.initInputCounter++;

            if(this.initInputCounter === this.subscribes.length){//get all inputs
                console.log("got all inputs")
                if(this.displayInputInit){
                    console.log("now init input display")
                    this.displayInputInit(this.inputMap);
                }
                if(this.displayOutputInit){
                    console.log("now init output display")

                    this.sendSimulation(this.valueMap2variableArr(this.inputMap), (result)=>{
                        let time = this.extractTime()
                        console.log(time)
                        this.updateOutputMapValue(result, time);
                        this.displayOutputInit(this.outputMap);
                    }, this.errorCB);//=>great!sent the inputvalues to simulation API


                }
            }
        });
        msocket.on('update',  (udata)=>{//when receive data updates
            //need to corrspond the updated data with kept copy - by name
            console.log("get update event");
            console.log(udata.data);

            if(this.updateInputs(udata.data)){//input data truly has been updated?

                if(this.displayInputUpdate) {//okay to fire this immedietly
                    this.displayInputUpdate(udata);
                }
                //TODO: however, this
                this.sendSimulation(dataMap, this.simulationResultCB, this.errorCB);//=>great!sent the inputvalues to simulation API
            }
        });
    },

    getInputMap : function () {
      return this.inputs;
    },
    extractTime : function () {
        let timeSeriesD = Object.values(this.inputMap).find((datum)=>{return datum.seriestype === true });
       return timeSeriesD.value.map((datum)=>{return datum.time})
    },
    /**
     * map output map to a list of uri and coressponding attrs to be sent to server
     */
    packOutputVSameUrl : function () {
        let objs = {}, arr =[];
        Object.values(this.outputMap).forEach((output) => {
            objs[output.uri] = objs[output.uri] ? objs[output.uri] : [];
            objs[output.uri].push(output.name);
        });
        for(let uri in objs){

            arr.push({uri: uri, names:objs[uri]});
        }
        return arr;
    }
    ,

    /***
    format convertion for value objects
    ***/
    valueMap2variableArr : function (valueMapObj) {

        let values = Object.values(valueMapObj);
        let groups= [];
        let first = true, minlen;
        //check length first
        //if only have length one - fix point, otherwise use smallest length
        values.forEach(function (item) {
            let value = item.value;
            if(value instanceof Array ){
                if(first){
                    minlen = value.length;
                } else if(value.length < minlen)
                {
                    minlen = value.length;
                }

            }
        })

        for(let idx = 0 ;idx < minlen; idx++){

            let vArr =[];
            values.forEach((item)=>{
                let value = item.value;
                if(value instanceof  Array){
                    vArr.push(parseFloat(value[idx].value));
                } else {
                    vArr.push(parseFloat(value))
                }
            })

            groups.push(vArr);
        }
        console.log("packed variables")
        console.log(groups)
        return groups
    },
    /***
    send simulation to API
     * If it is up to me I'd prefer POST, but now for convinience, better keep same format of API, with GET and input as params
     * @param variables
     * @constructor
     */
    sendSimulation: function (variables, successCB, errCB) {
        var queryString = "?input=";
        for (var i = 0; i < variables.length; i++) {
            if (i == 0) {
                queryString = queryString + JSON.stringify(variables[i]);

            } else {
                queryString = queryString + "+" + JSON.stringify(variables[i]);
            }
        }

        console.log(variables)
        console.log(queryString)
        $.ajax({
            url: "http://www.theworldavatar.com:82/" +this.simPath + queryString,

            success: (response)=>{
                successCB(response);
            },
            error: (err)=>{
                //Do Something to handle error
                //errCB(err);
				console.log(err);
				console.log(errCB);
            }
        });
    },
    /***
    simulation callback
    ***/
    simulationResultCB : function (sResult) {
        if(this.disSimResB4DBUpdateFn){//display before Update
            this.disSimResB4DBUpdateFn(sResult);//
        }
        if(this.updateOutput2DBFlag ){//update the sim results to db
            let updates =this.processResultStr(sResult);
            this.updateResult(updates, this.outputUpdateSuccessCBFn, this.errorCB);
        }
    },
    /***
     * separately retreive information about output : datatype, p, unit...
     */
    getSingleOutputInfo : function (item, CB) {
        $.ajax({//TODO:　CHANGE THIS IN FUTURE
            url: "http://www.theworldavatar.com:82/getSpecAttr"  ,
method:"POST",
            data: JSON.stringify(item),
            contentType: "application/json; charset=utf-8",
            success: (response)=>{
                console.log("@@@@@@@@@@@@")
                console.log(response)
                CB(null, response);
            },
            error: (err)=>{
                CB(err);
            }
        });
    },

    getOutputInfo : function (cb) {
     let packed = this.packOutputVSameUrl();

    async.map(packed, this.getSingleOutputInfo,  (err, result)=>{
        if(err){
            console.log("Err get Output info");
            cb(err);
            return;
        }

        //TODO: did not get  value here bug
        console.log("update output map after get inital output Info")
        //update result to outputmap
        console.log("@@@@@@@@@@@@")
        this.updateOutputMap(result);
        cb();
    });
    },

    /**
     * update ajax result to outputmap, specifically adding info about p, datatype and unit, or anything defined in the API getSpecAttr
     * @param result
     */
    updateOutputMapValue : function (result, times) {
        console.log(this.outputMap)
        console.log("simulation result: ")
        console.log(result.length)
        if(result.length > times.length ){
            throw new Error("Input output legnth does not match");
        }
        for(let name in this.outputMap) {
            this.outputMap[name].value = null;
        }
        let idxRes = 0;
            result.forEach((datum)=>{
            let idxOutput = 0;
        for(let name in this.outputMap){
            this.outputMap[name].value =  this.outputMap[name].value?this.outputMap[name].value:[];
            this.outputMap[name].value.push({value:datum[idxOutput], time: times[idxRes]});
            idxOutput++;
        }
                idxRes++;
    })

        console.log(this.outputMap)

    },

/**
update output map kept in memory
***/
    updateOutputMap:function (result) {
      console.log(this.outputMap);
	  result[0] = JSON.parse(result[0])
	  if(!result[0]|| result[0].constructor !== Array){
		  return;
	  }
        result[0].forEach((datum)=>{
          if(datum.name in this.outputMap){
              Object.assign(this.outputMap[datum.name], datum);
          }
        })
        console.log(this.outputMap)

    },

    updateInputs : function (newData) {
        let dataMap = this.inputMap;
        let modifiedFlag = false;
        for(let dataP of newData){
            if(dataP&& dataP.name && dataP.name in dataMap && dataMap[dataP.name].value!==dataP.value){
                dataMap[dataP.name] = dataP;
                modifiedFlag = true;
            }
        }
        return modifiedFlag
    },




    /***
     * Old API sends result as string, and uri is not specified, thus is why an extra output map is required, this function is kept to process this API, while new API will return result without extra piece of info
     * @param response
     * @returns {[*,*]}
     */
    processResultStr: function(response){
        //check format , or split into json format if result is a string
        if(typeof  response === "string"){
            let lines = resultStr.split('#');
            lines.forEach((line)=>{
                let nvpair =line.split('$');
                if (nvpair.length < 2){
                    return;
                }
                let name = nvpair[0].trim(), value = nvpair[1];
                if(name in outputMap){
                    outputMap[name]["value"] = value;
                    outputMap[name]["p"] = "http://www.theworldavatar.com/OntoCAPE/OntoCAPE/upper_level/system.owl#numericalValue";
                    outputMap[name]["datatype"] = "float";
                }
            })
            let attrObjs =  Object.values(this.outputMap).filter((attr)=>{return attr.value!==null||attr.value!==undefined});
            console.log("updates: ");
            console.log(attrObjs);

        }
        return attrObjs.length >0 ? attrObjs : response;
    },

    updateResult : function (result, successCB, errCB) {
        let CallUpdate = new CallSPARQLUpdate({
            modifications: result,
            successCB: successCB,
            errCB :　errCB,
        });
        CallUpdate.callUpdate();
    },
};