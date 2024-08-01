/**
 * Rewrriten with separated plot module
 */


$(document).ready(function () {
    /*socket io */
    var socket = io();
    /*select event******************************************/
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

    /*settings*************/
    var plots = {};

    var graphPanel = $("#graph1")
    console.log(graphPanel)
    let mMultiPlot;

    var isInitialized = false;
    socket.on('initial', function(idata){// receive initial event from
        console.log(JSON.stringify(idata));
        if(idata && idata.length > 0){ //check if this data is not null
            //let parsedData = idata;
            //let name = parsedData['name'];
            //  initialData = parsedData['data'];
            //We are gonna initiate multi graph...fuck me
            graphPanel.empty();

            mMultiPlot = new MultiPlots({
                length: idata.length,
                parentEl : graphPanel
            });
            mMultiPlot.initPlots(idata);
            isInitialized = true;
        } else {
        }
    });



        socket.on('update', function(data){//received data update event

        let parsedData = data;
        let name = parsedData['filename'];

        data = parsedData['data'];
        console.log("!!!!!!")
        console.log(data);

        if (isInitialized) {
            console.log("update pleasse")
            mMultiPlot.updatePlots(data);
        }


    });


});










