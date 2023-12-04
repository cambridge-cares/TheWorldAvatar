/**
 * Rewrriten with separated plot module
 */


/*line graph*******************************/

/*settings*************/
var plots = {};

let mMultiPlot;

let isInitialized = false;

let updateInterval = 300000;//milisec


$(document).ready(function () {
    let graphPanel = $("#graph1");
    /*select event******************************************/
    let sensorChosen;
    let updateTimer;
    $("select#sensor-select").on('change', function () {
        var  sensorSelected = $("select#sensor-select option:checked").val();
        console.log(sensorSelected);
        if(sensorChosen === sensorSelected) {
            return;//do nothing
        }
        $("#graph1").empty();
    
        stopUpdateTimer(updateTimer);
        console.log("into new sensor: " + sensorSelected);
        //get initial data
        $.ajax({
            method: "POST",
            url: "/bmsplot/initSensor",
            data: JSON.stringify({sensor:sensorSelected}),
            contentType: "application/json; charset=utf-8",
        })
            .done(function( idata ) {
                if(idata && idata.length > 0){ //check if this data is not null
                    //let parsedData = idata;
                    //let name = parsedData['name'];
                    //  initialData = parsedData['data'];
                    graphPanel.empty();
                    mMultiPlot = new MultiPlots({
                        length: idata.length,
                        parentEl : graphPanel
                    });
                    mMultiPlot.initPlots(idata);
                    isInitialized = true;
                    startUpdateTimer(sensorChosen)
                }            });
    });
    
    
    
    function updateGraph(url) {
        //TODO
                //mMultiPlot.updatePlots(data);
            
    }
    
    function startUpdateTimer(urls) {
        return setInterval(updateGraph(urls), updateInterval);
    }
    
    function stopUpdateTimer(timer) {
        if (timer !== null)  clearInterval(timer);
    }
    
});










