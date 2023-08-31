




/**/
    /*socket io */
    var socket = io();


let initialData;
var newDataObj ={};

$(document).ready(function () {


    /*line graph*******************************/

    var dataObj = {};
    /*settings*************/
    dataObj["names"] = ["FH-01"];
    dataObj["colors"] = ["green"];
    dataObj["scale"] = "linear";

    var l1;


    var isInitialized = false;
    socket.on('initial', function(idata){
        console.log(idata);
        //TODO: check this is indeed updated Data point
        initialData = JSON.parse(idata);
        //wrap initial data into data obj
        dataObj["start"] = initialData[0].time;
        dataObj["end"] = initialData[initialData.length - 1].time;
        dataObj["step"] = Math.round( (dataObj.end - dataObj.start) / initialData.length);
        dataObj["values"] = [initialData.map((item)=>{ return item.value})];

        l1 = new LineGraph({containerId: 'graph1', data: dataObj});
        console.log(dataObj["start"]);
        console.log(dataObj["end"]);
        console.log(dataObj["step"]);
        console.log(dataObj["values"]);
        isInitialized = true;
        newDataObj["start"] = initialData[initialData.length - 1].time;
        newDataObj["names"] = ["FH-01"];

    });



    socket.on('update', function(data){
        //TODO: check this is indeed updated Data point

        data = JSON.parse(data);
        console.log("!!!!!!")
        console.log(data);

        if(isInitialized) {
            var newData = data[data.length - 1];

            updateGraph(newData);
        }

    });



    function updateGraph(newDataP){


        newDataObj.values = [];
        newDataObj.values[0] = [parseFloat(newDataP.value)];
        newDataObj["end"] =  newDataP.time;
        newDataObj["step"] = newDataObj["end"] - newDataObj["start"] + Math.round(Math.random()*500);

        console.log("new value:" + JSON.stringify(newDataObj["values"]));
        console.log("step:" + newDataObj["step"]);
        console.log("end:" +  newDataObj["end"] );
        console.log("start:" +  newDataObj["start"] );

        newDataObj["start"]  = newDataObj["end"];


        l1.slideData(newDataObj);

    }
});










