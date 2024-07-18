(function PPMapAlt(){
		
    var ppMap = new PopupMap({useCluster:true});


    $(document).on('input', 'input', function () {//when user makes input
        console.log("input changed");
        cleanMsg();
        let el = $(this), value = el.val();
        if (value === "") {
            return;
        }

        let attrid = el.attr("id");

        if (!validateInput(value)) {
            self.displayMsg(errMsgBox, "Wrong datatype.", "warning");
        }
    });

    //get each value defined
    let costFactor = $("#costF")
    let riskFactor = $("#riskF")
    let excessFactor = $("#excessF")

    //TODO: submit button that sends out simulation
    let runBtn = $("#run-btn");
    let selectedId = 0 ;

    updatePredefined(selectedId)
    $("select#predefined-select").on('change', function () {
         selectedId = parseInt($("select#predefined-select option:checked").val()) - 1;
        updatePredefined(selectedId)


    })

    runBtn.click(function () {
        //ajax out
        ppSimulation(costFactor.val(), riskFactor.val(), excessFactor.val(), selectedId);

    })

    function updatePredefined(selectedId){
        switch(selectedId){
            case 0:
                costFactor.val(0.5)
                riskFactor.val(0.5)
                excessFactor.val(0.15)
                break;
            case 1:
                costFactor.val(0)
                riskFactor.val(1)
                excessFactor.val(0.1)
                break; 
            case 2:
                costFactor.val(0.5)
                riskFactor.val(0.5)
                excessFactor.val(0.25)
                break;                 

        }
    }


    //TODO: register for changes if want blinking effect of modification

    function ppSimulation(riskFactor, costFactor, excessFactor, predefinedId){
        $.ajax({
            url: window.location.href + "/simulation",
            method: "POST",
            contentType: "application/json; charset=utf-8",
            data:JSON.stringify({riskFactor, costFactor, excessFactor, predefinedId}),
            success: function (data) {
                console.log("success!")
                //TODO: according to new data, send modification request to endpoints
                //  addTextDisplay(data);
                let attrPairs = {};

                data.forEach((item)=>{
                    console.log(typeof item)

                    let attrArr = []
                    for(let key of Object.keys(item)){
                        console.log(key)
                        let value = typeof item[key]==="object"?JSON.stringify(item[key]):item[key]
                        attrArr.push({name:key, value:value})
                    }
                    attrPairs[item.uri] = attrArr
                })
                console.log(data)
                ppMap.updateMarkers(data, attrPairs)

            },
            error: function () {
                console.log("Can not get location")
            }
        });

    }


    //TODO: validate this
    function validateInput() {
        return true;
    }

    /*Msg***/
    let errMsgPanel = $("");

    function msgTemplate (msg, type) {
        return "<p class='alert alert-" + type + "'>" + msg + "</p>";
    }
    function displayMsg(msg, type) {
        //TODO: swithc type
        cleanMsg();
        errMsgPanel.append(msgTemplate(msg, type));

    }

    //TODO: define err msg panel
    function cleanMsg() {
        errMsgPanel.html("");
    }
})();
