(function PPMapAlt(){
		
    var ppMap = new PopupMap({useCluster:true});

    var anotherURL1 = 'https://sites.google.com/site/kmlfilescares/kmltest1/testfinal.kml';
    var anotherURL2 = 'https://sites.google.com/site/kmlfilescares/kmltest1/testfinal2.kml';
    var val =  parseFloat($("#co2Value").text());

    
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
         selectedId = parseInt($("select#predefined-select option:checked").val());
         console.log(selectedId)


    })

    runBtn.click(function () {
        runKML(selectedId);
    })

    //TODO: register for changes if want blinking effect of modification
    function runKML(predefinedId){
        console.log('predefinedID = ', predefinedId)
        ppMap.clearAnimatedLines();
        ppMap.clearMarkers();
        if (predefinedId == '0') {
            
            iriofnetwork = 'http://www.jparksimulator.com/kb/sgp/jurongisland/jurongislandpowernetwork/JurongIslandPowerNetwork.owl#JurongIsland_PowerNetwork';
            // Won't use this method when it refreshes?
            kmlURL = anotherURL1;
        }
        else if (predefinedId == '1') {
            iriofnetwork = 'http://localhost:8080/jps/scenario/testPOWSYSNuclearStartSimulationAndProcessResultAgentCallForTestScenario/read?query=%7B%22scenarioresource%22%3A%22http%3A%2F%2Fwww.jparksimulator.com%2Fkb%2Fsgp%2Fjurongisland%2Fjurongislandpowernetwork%2FJurongIslandPowerNetwork.owl%23JurongIsland_PowerNetwork%22+%7D';
            kmlURL = anotherURL2;
        }
        ppMap.drawLines( { "electricalnetwork":iriofnetwork });
        ppMap.drawMarkers( { "electricalnetwork":iriofnetwork });
        console.log(arrSum);
        refreshLayer(iriofnetwork, kmlURL);
        kmlURL = null;
        setTimeout(function(){
            distotalemission(arrSum);
        }, 5000);
    }
    function refreshLayer(iriofnetwork, kmlURL){
        if (kmlLayer){
            kmlLayer.setMap(null);
         }
        drawGenerator(iriofnetwork, kmlURL);
        console.log('Check that it should have refreshed. ')
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
    function distotalemission(result){
        $("#co2Value").text(result);
    }
    //TODO: define err msg panel
    function cleanMsg() {
        errMsgPanel.html("");
    }
})();
