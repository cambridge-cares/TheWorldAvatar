   
/***
Implements the Sim prototype. for Mau.
***/
    const toggleDisplay = elemId => {
        let x = document.getElementById(elemId);
        if (x.style.display !== 'block') {
            x.style.display = 'block';
        } else {
            x.style.display = 'none';
        }
    };

    $("#readme-button").click(function() {
        toggleDisplay("readme-text");
    });

    document.addEventListener("click", function(evt) {
        var readmeButtonElement = document.getElementById('readme-button'),
            readmeTextElement = document.getElementById('readme-text'),
            targetElement = evt.target;  // clicked element

        if (targetElement == readmeButtonElement || targetElement == readmeTextElement) {
            return; //readme-button or readme-text is clicked. do nothing.
        }

        if(readmeTextElement.style.display === 'block') {
            readmeTextElement.style.display = 'none';
        }
    });
	
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
            console.log("initial inputs:!!!!!!!!!!")
            console.log(Object.values(initData))


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
        }

        mausimplot.displaySetInput = function (setPArr) {
            setPArr.forEach((singleSetP)=>{
                mausimplot.displaySingleSetInput(singleSetP)
            })
        }
        mausimplot.displaySingleSetInput = function (singleSetIn) {

            let value = singleSetIn.value;
            let name = singleSetIn.name;
            let unit = singleSetIn.unit;
            if (name.includes("Humidity")){
                unit = "%";
            }else if (name.includes("Temperature")){
                unit = "Celsius";
            }else{
                unit = "1/m";
            }

            let  template =`<tr><td>${name}</td><td>${value}</td><td>${unit}</td></tr>`;
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





    })


    /*Err display***/


})();
