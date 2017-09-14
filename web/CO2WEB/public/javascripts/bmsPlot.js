




/**/
    /*socket io */
    var socket = io();
socket.emit("join", JSON.stringify(["http://www.theworldavatar.com/VAV-E7-28_DS_sensor1.owl"]));


let initialData;
var newDataObj ={};

$(document).ready(function () {


    /*line graph*******************************/

    var dataObj = {};
    /*settings*************/
    dataObj["names"] = ["Air-Flow rate = "];
    dataObj["colors"] = ["green"];
    dataObj["scale"] = "linear";

    var l1;


    var isInitialized = false;
    socket.on('initial', function(idata){// receive initial event from 
        console.log(JSON.stringify(idata));
        //TODO: check this is indeed updated Data point
		if(idata){ //check if this data is not null 
        //let parsedData = idata;
        //let name = parsedData['name'];
          //  initialData = parsedData['data'];
            console.log(idata);
            //wrap initial data into data obj
            dataObj["start"] = idata[0].time;
            dataObj["end"] = idata[idata.length - 1].time;
            dataObj["step"] = Math.round((dataObj.end - dataObj.start) / idata.length);
            dataObj["values"] = [idata.map((item) => {
                return item.value
            })];

            l1 = new LineGraph({containerId: 'graph1', data: dataObj});//init this graph
            console.log(dataObj["start"]);
            console.log(dataObj["end"]);
            console.log(dataObj["step"]);
            console.log(dataObj["values"]);
            isInitialized = true;
            newDataObj["start"] = idata[idata.length - 1].time;
            newDataObj["names"] = ["FH-01"];

		} else {
			//TODO: init one without previous data?
		}
    });



    socket.on('update', function(data){
        //TODO: check this is indeed updated Data point

        let parsedData = data;
        let name = parsedData['filename'];

            data = parsedData['data'];
            console.log("!!!!!!")
            console.log(data);

            if (isInitialized) {
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

        l1.slideData(newDataObj);
        newDataObj["start"]  = newDataObj["end"];


    }
});










