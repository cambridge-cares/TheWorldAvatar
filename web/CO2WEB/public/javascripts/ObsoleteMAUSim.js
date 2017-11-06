
var socket = io();
//join the inputvalues

//when update, ajax to backend for MAU, get result and show
//Use the line-graph module to visualize it
socket.emit("join", JSON.stringify([{uri:"http://www.theworldavatar.com/BMS/MAU-C7-1.owl", withData:true}
    ,{uri:"http://www.theworldavatar.com/BMS/DDC_R_1_1.owl", withData:true}
    ,{uri:"http://www.theworldavatar.com/BMS/MAU-C7-1_OAH_sensor1.owl", withData:true}
    ,{uri:"http://www.theworldavatar.com/BMS/MAU-C7-1_OAT_sensor1.owl", withData:true}

    ]));

var dataMap = {
    "V_St-006_Temperature_SP": 0,
    "V_RoomHumiditySPOfMAU-C7-1":0,
    "V_VolumetricF_St-006":0,
    "V_OutsideRelativeHumidityOfMAU-C7-1":0,
    "V_OutsideTemperatureOfMAU-C7-1":0,
};


//need to keep a copy of initial, data
socket.on('initial', function (idata) {
//extract data we need by name
    console.log(idata);
    updateDataMap(idata);

    console.log(dataMap);

});
socket.on('update', function (udata) {
    //need to corrspond the updated data with kept copy - by name

    console.log("get update event");
    console.log(udata.data);
    if(updateDataMap(udata.data)){
        console.log(dataMap);
        SendSimulationQuery(dataMap);
    }
//TODO: check update event
    //on update event, ajax to simulation service
    //when ajax result backs, ajax to endpoint for update
});