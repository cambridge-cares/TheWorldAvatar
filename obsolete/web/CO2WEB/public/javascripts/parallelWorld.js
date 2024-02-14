
(function PPMapAlt(){
	//initialize the Google Map	
    var ppMap = new PopupMap({useCluster:true});
    // just in case the kml file not loading in Claudius, use google sites. 
    //  var anotherURL1 = "https://sites.google.com/site/kmlfilescares/kmltest1/testfinalBASE.kml";
    // var anotherURL2 = "https://sites.google.com/site/kmlfilescares/kmltest1/testfinaltestPOWSYSNuclearStartSimulationAndProcessResultAgentCallForTestScenario10.kml";
    var anotherURL1 =  'http://theworldavatar.com/OntoEN/testfinalbase.kml';
    var anotherURL2 = 'http://theworldavatar.com/OntoEN/testfinaltestPOWSYSNuclearStartSimulationAndProcessResultAgentCallForTestScenario10.kml';
    //display the emission every five seconds. 
    setInterval(function(){
        distotalemission();
    }, 5000);
    
    $(document).on('input', 'input', function () {//when user makes input
        console.log("input changed");
        cleanMsg();
        let el = $(this), value = el.val();
        if (value === "") { 
            return;
        }

        let attrid = el.attr("id");

        
    });

    //submit button that sends out simulation
    let runBtn = $("#run-btn");
    let selectedId = 0 ;
    carbonTB = document.getElementById("carbontax");
   
    //updatePredefined(selectedId)
    $("select#predefined-select").on('change', function () {
         selectedId = parseInt($("select#predefined-select option:checked").val());
         console.log(selectedId)


    })
    //add function to run Btn
    runBtn.click(function () {
        runKML(selectedId);
    })
    // once map is instantiated, run base scenario
    var checkExist = setInterval(function() {
        if ($('#map').length) {
           console.log("Exists!");
           runKML('0');
           clearInterval(checkExist);
        }
     }, 100); // check every 100ms
    
     /** calls lines, markers, and kml and display co2 by making the respective queries. 
      * 
      * @param {String} predefinedId selection: Base Scenario = 0, Test Scenario = 1
      */
    function runKML(predefinedId){
        console.log('predefinedID = ', predefinedId);
        infowindow = new google.maps.InfoWindow({
            content: '<h2>Sup!</h2>'
        });
        ppMap.clearAnimatedLines();
        clearMarkers();
        if (predefinedId == '0') {
            
            kmlURL = anotherURL1;
            scenario = "base";
            carbonTB.value = 5
            // appPrefix = prefix1;
        }
        else if (predefinedId == '1') {
            kmlURL = anotherURL2;
            
            carbonTB.value=170;
            // scenario = "testCoordinateRetroFitNuclearAgentCall20";
            scenario = "testPOWSYSNuclearStartSimulationAndProcessResultAgentCallForTestScenario10";
            // appPrefix = prefix2;
        }
            
        json = { "electricalnetwork":iriofnetwork ,"flag": scenario }
        document.getElementById("loader").style.display = "block";
        ppMap.drawLines(json );
        drawMarkers(json);
        setTimeout(refreshLayer(json, kmlURL), 30000);
        setTimeout(displayCO2(json), 20000);
        kmlURL = null;
        
        
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

    /** Connect the emission to that on the table
     * 
     */
    function distotalemission(){
        $("#co2Value").text(actualCarbon.toFixed(2));
        $("#co2Value2").text(designCarbon.toFixed(2));
        $("#co2ValueYr").text(actualCarbonYr.toFixed(2));
        $("#co2ValueYr2").text(designCarbonYr.toFixed(2));
        $("#wildPercentage").text(wildPercentage.toFixed(2));
        $("#wildPercentage2").text(wildPercentage2.toFixed(2));
        $("#NucGen").text(dict["nuclear"]);
        $("#OilGen").text(dict["oil"]);
        $("#NatGasGen").text(dict["gas"]);
    }
    //TODO: define err msg panel
    function cleanMsg() {
        errMsgPanel.html("");
    }
})();


    





