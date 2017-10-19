/**
 * Created by Shaocong on 10/10/2017.
 * The module that runs simulation through API*/
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
        if(!opts.inputvalues || !opts.subscribes || !opts.socket || !opts.simPath ){
            throw new Error("Import paras(sim inputvalues | subscribing endpoints | socket |simulation API path) not specified");
        }
        this.inputvalues = opts.inputvalues;//this is the map of only value
        this.inputsAll = [];//this keeps all original objects

        this.subscribes = opts.subscribes;
        this.socket = opts.socket;
        this.simPath = opts.simPath;

        this.initDisplayFn = opts.initDisplayFn || null;
        this.disSimResB4DBUpdateFn = opts.disSimResB4DBUpdateFn || null;
        this.outputUpdateSuccessCBFn = opts.outputUpdateSuccessCBFn || null;
        this.updateOutput2DBFlag = opts.updateOutput2DBFlag || false;
        this.outputMap = opts.outputMap || null;
        this.errorCB = opts.errorCB || null;
        this.initInputCounter = 0;
    },
    setSubscribe: function () {
        let socket = this.socket;
        socket.emit("join", JSON.stringify(this.subscribes));

        socket.on('initial', function (idata) {
            console.log(idata);
            this.updateInputs(idata);
            console.log(this.inputvalues);
            this.inputsAll.push(idata)
    //        this.initInputCounter++;
            if(this.inputAll.length === this.inputvalues.length){
                if(this.initDisplayFn){
                    console.log("now init display")
                    this.initDisplayFn(this.inputAll);
                }
            }
        });
        socket.on('update', function (udata) {
            //need to corrspond the updated data with kept copy - by name
            console.log("get update event");
            console.log(udata.data);
            if(this.updateInputs(udata.data)){//input data truly has been updated?
                console.log(dataMap);
                this.sendSimulation(dataMap, this.simulationResultCB, this.errorCB);//=>great!sent the inputvalues to simulation API
            }
        });
    },

    getInputMap : function () {
      return this.inputs;
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
     * If it is update to me I'd prefer POST, but now for convinience, better keep same format of API, with GET and input as params
     * @param variables
     * @constructor
     */
    sendSimulation: function (variables, successCB, errCB) {
        var queryString = "?Input=";
        for (var i = 0; i < variables.length; i++) {
            if (i == 0) {
                queryString = queryString + variables[i].value;

            } else {
                queryString = queryString + "+" + variables[i].value;
            }
        }

        $.ajax({
            url: "http://www.theworldavatar.com/" +this.simPath + queryString,

            success: (response)=>{
                successCB(response);
            },
            error: (err)=>{
                //Do Something to handle error
                errCB(err);
            }
        });
    },
    simulationResultCB : function (sResult) {
        if(this.disSimResB4DBUpdateFn){//display without Update?
            this.disSimResB4DBUpdateFn(sResult);
        }
        if(this.updateOutput2DBFlag ){//WANT to update the sim results to db?
            let updates =this.processResultStr(sResult);
            this.updateResult(updates, this.outputUpdateSuccessCBFn, this.errorCB);
        }
    },
    /***
     * separately retreive information about output : datatype, p, unit...
     */
    getSingleOutputInfo : function (CB) {
        $.ajax({
            url: "localhost:3000/getSpecAttr"  ,
//TODO:　modifythis?
            data: JSON.stringify({uri: muri}),
            contentType: "application/json; charset=utf-8",
            success: (response)=>{
                CB(null, response);
            },
            error: (err)=>{
                //Do Something to handle error
                CB(err);
            }
        });
    },

    getOutputInfo : function (cb) {
     let packed = this.packOutputVSameUrl();
    async.map(packed, this.getSingleOutputInfo, function (err, result) {
        if(err){
            console.log("Err get Output info");
            cb(err);
            return;
        }
        //update result to outputmap
        this.updateOutputMap(result);
        cb();
    });
    },

    /**
     * update ajax result to outputmap, specifically adding info about p, datatype and unit, or anything defined in the API getSpecAttr
     * @param result
     */
    updateOutputMap : function (result) {
    result.forEach((datum)=>{
        if(datum.name in this.outputMap){
            Object.assign(this.outputMap[datum.name], datum);//merge two
        }
    })

        console.log(this.outputMap)

    },

    updateInputs : function (newData) {
        let modifiedFlag = false;
        for(let dataP of newData){
            if(dataP&& dataP.name && dataP.name in dataMap && dataMap[dataP.name]!==dataP.value){
                dataMap[dataP.name] = dataP.value;
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