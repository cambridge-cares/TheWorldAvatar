




/**/

//socket.emit("join", JSON.stringify(["http://www.theworldavatar.com/BCA_RT_sensor1.owl"]));


var newDataObj ={};



$(document).ready(function () {
    /*socket io */
    var socket = io();
/*select event******************************************/
//TODO: leave previous
let sensorChosen;

    $("select#sensor-select").on('change', function () {
        var  sensorSelected = $("select#sensor-select option:checked").val();
        console.log(sensorSelected);
        //leave previous chosen sensor subscribing room
		$("#graph1").empty();

        if(sensorChosen) {
            console.log("leave old room:" + sensorChosen)
            socket.emit("leave", JSON.stringify([sensorChosen]));
        }//join new room join through socket
        console.log("into new room: " + sensorSelected);
        socket.emit("join", JSON.stringify([{uri:sensorSelected, withData:true}]));
        sensorChosen = sensorSelected;
    });



    /*line graph*******************************/

    var newDataObjs = {};
    /*settings*************/
    //dataObj["names"] = ["value = "];
     var settings= {
         "colors" :["green"],//array, since one plot can contain multiple lines
     "scale" : "linear",
         "names" : ["Values="]
     }

    var plots = {};

     var graphPanel = $("#graph1")
    console.log(graphPanel)

    var isInitialized = false;
    socket.on('initial', function(idata){// receive initial event from 
        console.log(JSON.stringify(idata));
        //TODO: check this is indeed updated Data point
		if(idata && idata.length > 0){ //check if this data is not null
        //let parsedData = idata;
        //let name = parsedData['name'];
          //  initialData = parsedData['data'];
         //We are gonna initiate multi graph...fuck me
            $("#graph1").empty();

            idata.forEach((seriesVar) =>{
                console.log("a new series")
                if(!seriesVar.seriestype){//This is not a series data
                    return;//=>ignore
                }
                let dataObj = {};
                Object.assign(dataObj, settings);

                //wrap initial data into data obj
                dataObj["yunit"] = seriesVar.unit;
                let valueSeries = seriesVar["value"]
                dataObj["start"] = valueSeries[0].time;
                dataObj["end"] = valueSeries[valueSeries.length - 1].time;
                dataObj["step"] = Math.round((dataObj.end - dataObj.start) / valueSeries.length);
                console.log(dataObj["start"])
                console.log(dataObj["end"])
                console.log(dataObj["step"])

                dataObj["values"] = [valueSeries.map((item) => {
                    return typeof item.value === "string"?parseFloat(item.value): item.value;
                })];

                console.log(dataObj);
                let l1 = initSinglePlot(graphPanel, seriesVar.name, dataObj);//init this graph
                let newDataObj = {
                    "start": dataObj["end"],
                    "names" : ["value="]
            }
                plots[seriesVar.name] = l1;

                newDataObjs[seriesVar.name] = newDataObj;
              //  newDataObj["names"] = ["FH-01"];
            });

            isInitialized = true;


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
                console.log("update pleasse")
                data.forEach((seriesvar)=>{
                    let name = seriesvar.name, valueSeries = seriesvar.value;
                    let newData  = valueSeries[valueSeries.length - 1];
                    updateGraph(name, newData);
                });
            }


    });

    function jqSelParse(myid) {
        console.log(myid)
        return myid.replace(/(:|\.|\[|\]|,|=|@|\/)/g, "\\$1");

    }

    function initSinglePlot(parentel,name, dataObj) {
        let id = "plot-"+name;
        parentel.append("<p id='"+id+"'></p>");
        let el = $("#"+jqSelParse(id));
        console.log(el);
        el.css({
            position:"relative",
            height:"400px"
        });
        var l1 = new LineGraph({containerId: jqSelParse(id), data: dataObj});//init this graph
        console.log("finish initing new plot")
        return l1;
    }


    function updateGraph(name, newDataP){

        let newDataObj = newDataObjs[name];
        let l1 = plots[name];
        if(!newDataObj || !l1){
            return;
        }
        newDataObj.values = [];
        newDataObj.values[0] = [parseFloat(newDataP.value)];
        newDataObj["end"] =  newDataP.time;
        newDataObj["step"] = newDataObj["end"] - newDataObj["start"] ;//+ Math.round(Math.random()*500)???

        console.log("new value:" + JSON.stringify(newDataObj["values"]));
        console.log("step:" + newDataObj["step"]);
        console.log("end:" +  newDataObj["end"] );
        console.log("start:" +  newDataObj["start"] );

        l1.slideData(newDataObj);
        newDataObjs[name]["start"]  = newDataObj["end"];
         console.log("updated next start:")
        console.log(newDataObjs[name]["start"]);

    }
});










