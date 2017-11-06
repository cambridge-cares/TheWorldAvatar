/**
 */
//result template: {"tupleNumber": 120, "flowNumber": "10", "flowList": [[1.0, 2.0], [1.0, 3.0], [1.0, 5.0], [3.0, 2.0], [4.0, 1.0], [4.0, 2.0], [4.0, 3.0], [4.0, 5.0], [5.0, 2.0], [5.0, 3.0]], "modifyList": [[895.0, 1.0, 2.0], [1512.0, 1.0, 5.0], [638.0, 4.0, 1.0]], "saveNumber": 3045.0}

//TODO:　GET THSI FORM BACK in future now only for testing
    /**
const data = {"tupleNumber": 120, "flowNumber": "10", "flowList": [[1.0, 2.0], [1.0, 3.0], [1.0, 5.0], [3.0, 2.0], [4.0, 1.0], [4.0, 2.0], [4.0, 3.0], [4.0, 5.0], [5.0, 2.0], [5.0, 3.0]], "modifyList": [[895.0, 1.0, 2.0], [1512.0, 1.0, 5.0], [638.0, 4.0, 1.0]], "saveNumber": 3045.0};
     **/
var hwMap = new PopupMap({useCluster:false});



//when button clicked, run simulation
$(document).ready(function () {
    $("#run-btn").click(function () {
        console.log("Start sim")
         HWSimulation();

    });
});



function HWSimulation() {
    //todo:need refresh map?
    //first, ajax

    $.ajax({
        url: window.location.href + "/simulation",
        method: "GET",
        //  contentType: "application/json; charset=utf-8",
        success: function (data) {
     console.log(data)
            hwMap.drawAnimatedLines(data.modifyList)
            //TODO: need a mechanism to clear every lines before redrawing
        },
        error: function () {
            console.log("Can not get location")
        }
    });



}

















//for each in modify list, draw a line with animation








