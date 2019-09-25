(function PPMapAlt(){
		
    var ppMap = new PopupMap({useCluster:true});

    var anotherURL = 'https://sites.google.com/site/kmlfilescares/kmltest1/testfinal.kml';
    ppMap.drawKML(anotherURL);

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


    //TODO: submit button that sends out simulation
    let runBtn = $("#run-btn");
    let selectedId = 0 ;

    // updatePredefined(selectedId)
    $("select#predefined-select").on('change', function () {
         selectedId = parseInt($("select#predefined-select option:checked").val()) - 1;
        // runKML(selectedId)


    })

    runBtn.click(function () {
        runKML(selectedId);

    })

    //TODO: register for changes if want blinking effect of modification
    function runKML(predefinedId){
        console.log('predefinedID = ', predefinedId)
        ppMap.clearAnimatedLines();
        if (predefinedId == '0') {
            ppMap.drawLines( { "electricalnetwork":'http://www.jparksimulator.com/kb/sgp/jurongisland/jurongislandpowernetwork/JurongIslandPowerNetwork.owl#JurongIsland_PowerNetwork' });
            // Won't use this method when it refreshes?
        }
        else if (predefinedId == '1') {
            ppMap.drawLines({"jpscontext":{"scenariourl":"http://localhost:8080/jps/scenario/testPOWSYSCoordinateOPF"},"electricalnetwork":"http://www.jparksimulator.com/kb/sgp/jurongisland/jurongislandpowernetwork/JurongIslandPowerNetwork.owl#JurongIsland_PowerNetwork"});
        }
        else if (predefinedId == '2'){
            ppMap.drawLines({"jpscontext":{"scenariourl":"http://localhost:8080/jps/scenario/testPOWSYSCoordinatePF"},"electricalnetwork":"http://www.jparksimulator.com/kb/sgp/jurongisland/jurongislandpowernetwork/JurongIslandPowerNetwork.owl#JurongIsland_PowerNetwork"});
        
        }
        
        var anotherURL = 'https://sites.google.com/site/kmlfilescares/kmltest1/testfinal.kml';
        ppMap.drawKML(anotherURL);
    }

    //TODO: validate this
    function validateInput() {
        return true;
    }
    //change filter according to radio button
    // $(document).ready(function(){
    //     $('input[type=radio][name=myRadios]').change(function() {
    //         if (this.value == 'Current') {
    //             kmlLayer.setMap(null);
    //             var anotherURL = 'https://sites.google.com/site/kmlfilescares/kmltest1/test2.kml';
    //             ppMap.drawLines();
    //             ppMap.drawKML(anotherURL);
    //         }
    //         else if (this.value == 'Modified') {
    //             kmlLayer.setMap(null);
    //             var anotherURL = 'https://sites.google.com/site/kmlfilescares/kmltest1/icontest.kml';
    //             ppMap.drawKML(anotherURL);
    //         }
            
    //     });
        
    // });
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
