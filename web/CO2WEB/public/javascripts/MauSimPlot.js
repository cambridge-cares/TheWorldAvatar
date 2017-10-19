/**
 * Created by Shaocong on 10/11/2017.
 */
var socket = io;

(function MauSimPlot() {
    var inputs = {
        "V_St-006_Temperature_SP" : 0,//series data!!!!!
        "V_RoomHumiditySPOfMAU-C7-1":0,
        "V_VolumetricF_St-006":0,
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

    const inputPanelEl = $("#input-plot-panel");
    const outputPanelEl = $("#output-plot-panel");

    let mauInputPlots = new MultiPlots({
    length : inputs.length,
        parentEl :inputPanelEl
    });

    let mauOutputPlots = new MultiPlots({
        length: Object.keys(outputMap).length,
        parentEl: outputPanelEl
    })
    /*using socket to retrieve input update autoly, then run Simulation thru API**********/
    var MAUSim = new Sim({
        inputs: [],
        subscribes :[],
        socket : socket,
        simPath : "runMAU",
        initDisplayFn : MauSimPlot.initDisplayFn,
        disSimResB4DBUpdateFn:"@_@",
        errorCB : "@_@"
    });

    //let's diplay initial input Plots!
    MauSimPlot.initDisplayFn = function(initData) {
        mauInputPlots.initPlots(initData);
        console.log(initData)
        //ajax initial data to MAU for plotting
        MAUSim.sendSimulation(MAUSim.getInputMap(), (result)=>{
            //TODO:  need to contain the original uunit and stuff
            mauOutputPlots.initPlots(this.remapMAUResult(result));
        }, (err)=>{
            //TODO:　err handling
        })

    }

    MauSimPlot.remapMAUResult = function (result) {
        let reMapped = result.map((dataset)=>{

        })
    }
    MauSimPlot.displayOutput = function (outputdata) {
        //new Ouput data comes in array
        //output comes in [[5][5][5]]





    }




    /*Err display***/


})();
