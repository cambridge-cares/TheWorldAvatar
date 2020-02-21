var batteryInfo ="PREFIX j1:<http://www.theworldavatar.com/ontology/ontopowsys/PowSysRealization.owl#> "
		+ "PREFIX j2:<http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#> "
		+ "PREFIX j3:<http://www.theworldavatar.com/ontology/ontopowsys/model/PowerSystemModel.owl#> "
		+ "PREFIX j4:<http://www.theworldavatar.com/ontology/meta_model/topology/topology.owl#> "
		+ "PREFIX j5:<http://www.theworldavatar.com/ontology/ontocape/model/mathematical_model.owl#> "
		+ "PREFIX j6:<http://www.theworldavatar.com/ontology/ontocape/chemical_process_system/CPS_behavior/behavior.owl#> "
		+ "PREFIX j7:<http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/space_and_time/space_and_time_extended.owl#> "
		+ "PREFIX j8:<http://www.theworldavatar.com/ontology/ontocape/material/phase_system/phase_system.owl#> "
		+ "PREFIX j9:<http://www.theworldavatar.com/ontology/ontopowsys/PowSysBehavior.owl#> "
		+ "PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#> "
		
		+ "SELECT ?entity ?V_x ?V_x_unit ?V_y ?V_y_unit ?V_ActivePowerInjection_of_VRB ?V_ActivePowerInjection_of_VRB_unit "
		+ "WHERE {?entity   j9:hasActivePowerInjection ?cap ."
		+"?cap j2:hasValue ?vcap ."
		+"?vcap  j2:numericalValue ?V_ActivePowerInjection_of_VRB ."
		+"?vcap  j2:hasUnitOfMeasure ?V_ActivePowerInjection_of_VRB_unit ."

		+ "?entity   j7:hasGISCoordinateSystem ?coorsys ."

		+ "?coorsys  j7:hasProjectedCoordinate_y  ?y  ."
		+ "?y  j2:hasValue ?vy ." 
		+ "?vy  j2:numericalValue ?V_y ."
		+ "?vy  j2:hasUnitOfMeasure ?V_y_unit ."

		+ "?coorsys  j7:hasProjectedCoordinate_x  ?x  ."
		+ "?x  j2:hasValue ?vx ." 
		+ "?vx  j2:numericalValue ?V_x ."//longitude
		+ "?vx  j2:hasUnitOfMeasure ?V_x_unit ."//longitude

        + "}";
  
(function PPMapAlt(){
    var ppMap = new PopupMap({useCluster:true});
    //refresh value of carbon emission every 5 minutes
    setInterval(function(){
        distotalemission();
    }, 5*60*1000);
    //when user changes input, create notification
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


    //TODO: register for changes if want blinking effect of modification
    function runKML(predefinedId){
        console.log('predefinedID = ', predefinedId);
        infowindow = new google.maps.InfoWindow({
            content: '<h2>Sup!</h2>'
        });
        ppMap.clearAnimatedLines();
        clearMarkers();
            
        json = { "electricalnetwork":iriofnetwork ,"flag": scenario }
        document.getElementById("loader").style.display = "block";
        ppMap.drawLines(json );
        drawMarkers(json);
        refreshLayer(json, kmlURL);
        displayCO2(json);
        kmlURL = null;
        
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
    function distotalemission(){
        $("#co2Value").text(actualCarbon.toFixed(2));
        $("#co2Value2").text(designCarbon.toFixed(2));
        $("#co2ValueYr").text(actualCarbonYr.toFixed(2));
        $("#co2ValueYr2").text(designCarbonYr.toFixed(2));
        $("#wildPercentage").text(wildPercentage.toFixed(2));
        $("#wildPercentage2").text(wildPercentage2.toFixed(2));
        $("#OilGen").text(dict["oil"]);
        $("#NatGasGen").text(dict["gas"]);
    }
    //TODO: define err msg panel
    function cleanMsg() {
        errMsgPanel.html("");
    }
})();