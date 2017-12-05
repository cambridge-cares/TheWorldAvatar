
var socket = io();

(function MauSimPlot() {
    var mausimplot= {};
    var inputs = {
        "V_St-006_Temperature_SP" : 0,//series data!!!!!
        "V_RoomHumiditySPOfMAU-C7-1":0,
        "V_VolumetricF_St-006":20,
        "V_OutsideRelativeHumidityOfMAU-C7-1":0,//series data
        "V_OutsideTemperatureOfMAU-C7-1":0 //series data
    };

    var subsrcibes = [{uri:"http://www.theworldavatar.com/BMS/MAU-C7-1.owl", withData:true}
        ,{uri:"http://www.theworldavatar.com/BMS/DDC_R_1_1.owl", withData:true}
        ,{uri:"http://www.theworldavatar.com/BMS/MAU-C7-1_OAH_sensor1.owl", withData:true}
        ,{uri:"http://www.theworldavatar.com/BMS/MAU-C7-1_OAT_sensor1.owl", withData:true}

    ];

    var outputMap = {
        "V_TotalCoolingDutyOfMAU-C7-1":{uri : "http://www.theworldavatar.com/BMS/MAU-C7-1.owl", name:"V_TotalCoolingDutyOfMAU-C7-1"}
        ,"V_TotalSensibleCoolingDutyOfMAU-C7-1":{uri : "http://www.theworldavatar.com/BMS/MAU-C7-1.owl", name:"V_TotalSensibleCoolingDutyOfMAU-C7-1"}
        ,"V_TotalLatentCoolingDutyOfMAU-C7-1":{uri : "http://www.theworldavatar.com/BMS/MAU-C7-1.owl", name:"V_TotalLatentCoolingDutyOfMAU-C7-1"}
        ,"V_CalcVolumetricF_CHW-001":{uri : "http://www.theworldavatar.com/BMS/MAU-C7-1.owl", name:"V_CalcVolumetricF_CHW-001"}
        ,"V_HeatDutyOfMAU-HE-C7-1_3":{uri : "http://www.theworldavatar.com/BMS/MAU-C7-1.owl", name:"V_HeatDutyOfMAU-HE-C7-1_3"}

    };


    /*Init plot =V=***/

    $(document).ready(function () {


        const inputPanelEl = $("#input-plot-panel");
        const inputSetTable = $("table tr:last");
        const loadingSign = $("#loading-sign")
        console.log(inputSetTable)

        console.log(inputPanelEl)
        const outputPanelEl = $("#output-plot-panel");

        let mauInputPlots;

        let mauOutputPlots = new MultiPlots({
            length: Object.keys(outputMap).length,
            parentEl: outputPanelEl
        })


        //let's diplay initial input Plots!
        mausimplot.displayInputInit = function(initData) {
            //TODO: add titles for plots
            console.log("initial inputs:!!!!!!!!!!")
            console.log(Object.values(initData))
            //TODO: how to display fix point data? should be a sentence
            //TODO: first, determine if it is fixed=point or time-series data

            let values = Object.values(initData);
            let setPoints = [], seriesvalues = [];

            values.forEach((item)=>{
                if(item.value instanceof  Array){
                    seriesvalues.push(item);
                }else{
                    setPoints.push(item)
                }
            })

            console.log("!!!!!!!!!!!!!!!!!!!!!")
            console.log(inputPanelEl)
            mauInputPlots = new MultiPlots({
                length : seriesvalues.length,
                parentEl :inputPanelEl
            });
            mauInputPlots.initPlots(seriesvalues);
            mausimplot.displaySetInput(setPoints);
            //TODO: show waiting png until done
        }

        mausimplot.displaySetInput = function (setPArr) {
            setPArr.forEach((singleSetP)=>{
                mausimplot.displaySingleSetInput(singleSetP)
            })
        }
        mausimplot.displaySingleSetInput = function (singleSetIn) {

            let value = singleSetIn.value;
            let name = singleSetIn.name;
            let  template =`<tr><td>${name}</td><td>${value}</td></tr>`;
            console.log(template)
            inputSetTable.after(template);

        };
        mausimplot.displayinputUpdate = function (updatedDataMap) {
            console.log(updatedDataMap)

        }
        mausimplot.displayOutputInit = function (updatedDataMap) {
            //new Ouput data comes in array
            //output comes in [[5][5][5]]
            console.log("updated output:!!!!!!!!!!!!!!!!!!!")
            console.log(updatedDataMap)
            //TODO:　magically the original retrive info about outputs are not here
            //TODO: time info must either be returned or packed from orginal time,
            //TODO: cont" problem is where this logic should lies
            //need to remap this
            mauOutputPlots.initPlots(Object.values(updatedDataMap));

            loadingSign.css("display","none");
        }


        /*using socket to retrieve input update autoly, then run Simulation thru API**********/
        var MAUSim = new Sim({
            inputMap: inputs,
            outputMap:outputMap,
            subscribes :subsrcibes,
            socket : socket,
            simPath : "MAU",
            displayInputInit : mausimplot.displayInputInit,
            displayOutputInit:mausimplot.displayOutputInit,
            displayinputUpdate :"",
            disSimResB4DBUpdateFn:"@_@",
            errorCB : "@_@"
        });

        //TODO:　add a name
        //TODO: add a loading bar





    })


    /*Err display***/


})();
