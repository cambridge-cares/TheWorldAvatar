var stack = "http://137.132.22.165:3838/";

function visualiseTimeSeriesData(rawJSON) {
    $("#loaderAnimationWrapper").hide();
    console.log("response received");
    let meta = rawJSON["meta"];
    let time = rawJSON["time"];

    if (meta != null) console.log("Got a meta object!");        // meta is ignored for now
    if (time != null) console.log("Got a time object!");

    document.getElementById("metaTimeContainer").innerHTML = "";

    if (time != null) {
        // sort data based on the measurement's key
        time_temp = {};
        for (i = 0; i < time.length; i++) {
            time_temp[time[i]["data"][0]] = time[i];
        }

        time_temp = Object.entries(time_temp).sort();
        time = time_temp.map(([key, value]) => value);

        // change locale
        for (i = 0; i < time.length; i++) {
            time[i]['time'] = time[i]['time'].map(dateString => timeZoneConversion(dateString));
        }

        console.log(time);

        // Plot data
        timeseriesHandler.parseData(time);

        timeseriesHandler.showData("metaTimeContainer");

        console.log("selected id " + prevSelected);
        $("#time-series-select").val(prevSelected);
        timeseriesHandler.update(prevSelected);

        $("#time-series-select").change(function() {
            console.log("selection is " + $("#time-series-select").val());
            timeseriesHandler.update($("#time-series-select").val());
        });

    } else {
        $("#notAvailaleP").show();
        console.warn("No 'time' node found, skipping timeseries visualisation.");
    }
}

function timeZoneConversion(timeStamp) {
    const date = new Date(timeStamp);
    const localTimezoneOffset = date.getTimezoneOffset() * 60000;
    const localTimestamp = new Date(date.getTime() - localTimezoneOffset);
    return localTimestamp.toISOString().slice(0, -5) + "Z";
}

function loadSelectedEquipment() {

    var FIAUri = stack + "feature-info-agent/get";
    var params = {
            "iri": jsObject.getEquipmentIri()
    };
    console.log("sending request to " + FIAUri + " with params " + params.iri);
    $.ajax({
            dataType: "json",
            url: FIAUri,
            data: params,
            success: visualiseTimeSeriesData,
            error: getJSONFailure
    })

}

function getJSONFailure() {
    $("#loaderAnimationWrapper").hide();
    $("#notAvailaleP").show();
    console.warn("Could not get valid response from the agent");
}

function refreshChart() {
    console.log("Refresh chart called");
    prevSelected = $("#time-series-select").val()
    loadSelectedEquipment();
}


var timeseriesHandler = new TimeseriesHandler();
var prevSelected = 0;

$(document).ready(function () {
    loadSelectedEquipment();
});