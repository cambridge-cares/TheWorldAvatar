




/**/
    /*socket io */
    var socket = io();


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
        let parsedData = JSON.parse(idata);
        let name = parsedData['name'];
        if(name==="FH-01.owl") {//check the update comes from correct source
            initialData = parsedData['data'];
            console.log(initialData);
            //wrap initial data into data obj
            dataObj["start"] = initialData[0].time;
            dataObj["end"] = initialData[initialData.length - 1].time;
            dataObj["step"] = Math.round((dataObj.end - dataObj.start) / initialData.length);
            dataObj["values"] = [initialData.map((item) => {
                return item.value
            })];

            l1 = new LineGraph({containerId: 'graph1', data: dataObj});//init this graph
            console.log(dataObj["start"]);
            console.log(dataObj["end"]);
            console.log(dataObj["step"]);
            console.log(dataObj["values"]);
            isInitialized = true;
            newDataObj["start"] = initialData[initialData.length - 1].time;
            newDataObj["names"] = ["FH-01"];
        }
		} else {
			//TODO: init one without previous data?
		}
    });



    socket.on('update', function(data){
        //TODO: check this is indeed updated Data point

        let parsedData = JSON.parse(data);
        let name = parsedData['name'];
        if(name==="FH-01.owl") {
            data = parsedData['data'];
            console.log("!!!!!!")
            console.log(data);

            if (isInitialized) {
                var newData = data[data.length - 1];

                updateGraph(newData);
            }
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










