var scenario;
var prefix = "http://localhost:8080";
iriofnetwork = 'http://www.jparksimulator.com/kb/sgp/jurongisland/jurongislandpowernetwork/JurongIslandPowerNetwork.owl#JurongIsland_PowerNetwork';

(function PPMapAlt(){
		
    var ppMap = new PopupMap({useCluster:true});
    var anotherURL1 = 'https://sites.google.com/site/kmlfilescares/kmltest1/testfinalBASE.kml';
    var anotherURL2 = 'https://sites.google.com/site/kmlfilescares/kmltest1/testfinaltestPOWSYSNuclearStartSimulationAndProcessResultAgentCallForTestScenario.kml';
    // var anotherURL1 =  'http://theworldavatar.com/OntoEN/testfinalBASE.kml';
    // var anotherURL2 = 'http://theworldavatar.com/OntoEN/testfinaltestPOWSYSNuclearStartSimulationAndProcessResultAgentCallForTestScenario.kml';
    var val =  parseFloat($("#co2Value").text());
    setInterval(function(){
        distotalemission(arrSum);
    }, 5000);
    
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
    let resetBtn = $("#reset-btn");
    let selectedId = 0 ;
   
    // updatePredefined(selectedId)
    $("select#predefined-select").on('change', function () {
         selectedId = parseInt($("select#predefined-select option:checked").val());
         console.log(selectedId)


    })

    runBtn.click(function () {
        runKML(selectedId);
    })
    resetBtn.click(function(){
        keys = Object.keys(sessionStorage);
        i = keys.length;
        console.log("CHECK THIS OUT")
        while (i--){
            var retrievedObject = sessionStorage.getItem(keys[i]);
            console.log(" i: "+ i);
            updateOwlFile(keys[i], JSON.parse(retrievedObject), 'kml');
        } 
    })

    //TODO: register for changes if want blinking effect of modification
    function runKML(predefinedId){
        console.log('predefinedID = ', predefinedId)
        ppMap.clearAnimatedLines();
        ppMap.clearMarkers();
        if (predefinedId == '0') {
            
            kmlURL = anotherURL1;
            scenario = "BASE";
            // appPrefix = prefix1;
        }
        else if (predefinedId == '1') {
            kmlURL = anotherURL2;
            scenario = "testPOWSYSNuclearStartSimulationAndProcessResultAgentCallForTestScenario";
            // appPrefix = prefix2;
        }
            
        json = { "electricalnetwork":iriofnetwork ,"flag": scenario }
        console.log(arrSum);
        ppMap.drawLines(json );
        ppMap.drawMarkers( json);
        refreshLayer(json, kmlURL);
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
    function distotalemission(result){
        $("#co2Value").text(result);
    }
    //TODO: define err msg panel
    function cleanMsg() {
        errMsgPanel.html("");
    }
})();
var arrSum = 0.00;
var  kmlLayer;
function drawGenerator(data, anotherURL){
    var d = new Date();
    var n = d.getTime();
    var kmljson = {};
    var kmlurl = prefix + '/JPS_POWSYS/ENVisualization/createKMLFile'; 
    kmljson["electricalnetwork"] = data["electricalnetwork"];
    kmljson["n"] = String(n);
    kmljson["flag"] =  data["flag"];
    kmlurl += "?query=" + encodeURIComponent(JSON.stringify(kmljson));      


    var request = $.ajax({
        url: kmlurl,
        type: 'GET',
        data: kmljson,
        contentType: 'application/json; charset=utf-8',
        success: function(){  
        },
        error: function(ts) {
            alert(ts.responseText);
        }   
    });

    request.done( function(data) {
    console.log ("success create request");
    kmlLayer = new google.maps.KmlLayer({
        // url: 'http://www.theworldavatar.com/OntoEN/testfinal.kml',//In other cases, will eventually be read and overwritten here. NO PROBLEM!
        url: anotherURL+ "?r="+(new Date()).getTime(), //this is completely necessary for cache-busting. 
        suppressInfoWindows: false,
        map: map
    });


        kmlLayer.addListener('click', function(kmlEvent) {
            setKMLMenu(kmlEvent)
        });             
        
    });

    request.fail(function(jqXHR, textStatus) {
    });
}

function sendRequest(url,callback) {
    console.log(url)
$.ajax({
    url: "http://theworldavatar.com:82/getAttrList",
    method: "POST",
    data: JSON.stringify({uri: url}),
    contentType: "application/json; charset=utf-8",

success: function (attrPairs) {
            callback(attrPairs);
        }

});
}

function setKMLMenu(kmlEvent){
    var data = kmlEvent.featureData;
    var nameString = data.name.substr(1);
    var names = nameString.split('[');
    var buttonsList = '<p>Please select the Entity you would like to modify</p>';
    for(var index in names)
    {
        var name = names[index];

        buttonsList = buttonsList + '<div><label>' + name.split('#')[1] + '</label>' +
            '<button onclick="selectEBus(event)" style= "cursor: pointer;" id="' + name + '"> > </span></div>'
    }

    buttonsList = '<div id="buttonContainer">'+ buttonsList +'</div><hr/><div id="inputsContainer"></div>';
    // set the content of the popup window.
    kmlEvent.featureData.infoWindowHtml = '<div>' + buttonsList + '</div>';

}
function selectEBus(event) {
    selectedId =  event.srcElement.id; //this needs to be saved on a local version, and not towards here. 
    data = {"scenarioresource":selectedId}
    console.log()
    // var url = 'http://www.theworldavatar.com/kb/sgp/jurongisland/jurongislandpowernetwork/' +selectedId.split('#')[1]; //will read from here. 
    var url = 'http://localhost:8080/jps/scenario/'+scenario+'/read?query=' + encodeURIComponent(JSON.stringify(data));
    var kmljson = {};
    var kmlurl = prefix + '/JPS_POWSYS/ENVisualization/readGenerator'; 
    kmljson["electricalnetwork"] = iriofnetwork;
    kmljson["flag"] = scenario;
    kmljson['selectedID'] = selectedId;
    kmlurl += "?query=" + encodeURIComponent(JSON.stringify(kmljson));   
    console.log(kmlurl);
    var request = $.ajax({
        url: kmlurl,
        type: 'GET',
        data: kmljson,
        contentType: 'application/json; charset=utf-8',
        success: function(){  
        },
        error: function(ts) {
            alert(ts.responseText);
        }   
    });
    request.done( function(data) {
        console.log ("success create request");
        var inputsHTML = '';
        var obj0 = JSON.parse(data)[0];
        // response = sortByKey(obj0[0],'name');
        console.log(obj0)
        var nameSet = [];

        for(var item in obj0)
        {
            var pair = obj0[item];
            console.log(pair);
            if (pair['value'].includes('vemission')){
                var inputLine = '<tr><td><label> Actual CO2 Emission </label></td><td><input class="input_class" data-dataType="' + pair['datatype'] + '" value="' + pair['value'] + '" style="float: right;"></td><td> tonnes/hr   </td></tr>';
            
            }
            else if(!pair['value'].includes('.owl'))
            {
            console.log(pair['name']);
            var inputLine = '<tr><td><label>' + pair['name'] +'</label></td><td><input class="input_class" data-dataType="' + pair['datatype'] + '" value="' + pair['value'] + '" style="float: right;"></td><td>   </td></tr>';
            inputsHTML = inputsHTML + inputLine;
            nameSet.push(pair['name']);

            }
        }


        var div = document.getElementById('inputsContainer');
        div.innerHTML = '<table data-type="kml" data-url='+ url +' id="inputsTable">' + inputsHTML + '</table><br/><button onclick="SubmitTable(this)">OPF</button><button onclick="SubmitTable(this)">PF</button>'+
        '<img id="myProgressBar" style="width:100px;height:100px;display:none" src="https://media.giphy.com/media/3oEjI6SIIHBdRxXI40/giphy.gif"/><br/>'

    });

}
function SubmitTable(e) {


    console.log("-----------------------------------")
    for(var c = 0; c < 15; c++){
        console.log("\n")
    }
    console.log('e',e.innerHTML)
    console.log("-----------------------------------")
    opt = e.innerHTML;
    var table = document.getElementById('inputsTable');
    var rows = table.firstElementChild.childNodes;
    var url = table.getAttribute('data-url');
    var type = table.getAttribute('data-type');
    console.log('type',type);

    var JSONArray  = {};

    var proceed = true;

    for(var i = 0; i < rows.length; i++)
    {
        var row = rows[i];
        var name = row.getElementsByTagName('label')[0].innerText;
        var value = row.getElementsByTagName('input')[0].value;
        
        
        if(name.includes('EBus-001')){ // This is a slack bus, the magnitude is always 1 and the angle is always 0
            //console.log("label forbidden= "+label);
            if(name.includes('VoltageMagnitude')|| name.includes('Vm_EBus')) {
                if (value !== 1){
                    alert('The value of the voltage magnitude and Vm for a slack bus should always be 1 kV (in p.u format)')
                    proceed = false;
                }
            }
            
            if (name.includes('VoltageAngle')|| name.includes('Va_EBus')){
                if (value !== 0){
                    alert('The value of the voltage angle and Va for a slack bus should always be 0 degree')
                    proceed = false;
                }
            }
        }
        else{ // This is a load bus 
        //console.log("label forbidden= "+label);
            if(name.includes('VoltageMagnitude')|| name.includes('Vm_EBus')){
                if( value > 1.05 || value <= 0.95){
                    alert('The value of the voltage magnitude and Vm should be between 0.95 and 1.05 kV (in p.u format)')
                    proceed = false;
                }
            }           
        }
        
        
        
        
        

        
        var datatype = row.getElementsByTagName('input')[0].getAttribute('data-dataType');
        console.log('value',value,'name',name,'url',url);
        JSONArray[name] = {name: name,value:value, datatype: datatype }
    }



    if(proceed){
        var progress = document.getElementById('myProgressBar');
        progress.style.display = 'block';
        updateOwlFile(url,JSONArray,type);
    }


}
function updateOwlFile(filename,JSONArray,_type) {

    console.log('number',Object.keys(JSONArray).length);
    console.log('JSONArray',Object.keys(JSONArray));
    console.log('filename=',filename);
    console.log('type=',_type);

    var allItemsArray = [];
    var indexCounter = 0;
    var temp = [];
    for(var item in JSONArray){
            if(((indexCounter!== 0) && (indexCounter % 10 === 0)) || (indexCounter === parseInt(Object.keys(JSONArray).length - 1)) )
            {
                if((indexCounter === parseInt(Object.keys(JSONArray).length - 1)))
                {
                    //allItemsArray.push(temp);
                    //temp = [];
                    temp.push(item)
                    allItemsArray.push(temp);
                }
                else
                {
                    allItemsArray.push(temp);
                    temp = [];
                    temp.push(item)
                }


            }
            else
            {
                temp.push(item)
            }

            indexCounter++;
        }

    console.log(allItemsArray);


    var asyncLoop = function(o){
        var i=-1,
            length = o.length;

        var loop = function(){
            i++;
            if(i===length){o.callback(); return;}
            o.functionToLoop(loop, i);
        };
        loop();//init
};


asyncLoop({
    length : Math.ceil(Object.keys(JSONArray).length / 10),
    functionToLoop : function(loop, i){


        var sampleUpdate = [];
        var uri = [];

        var Session = allItemsArray[i];
        console.log('Session',Session);
        for(var j = 0; j < Session.length; j++)
        {
            var item = Session[j];
            var obj = JSONArray[item];

            console.log('obj',obj,i,j);

            var targetIRI = obj.name;
            var dataType = obj.datatype;

            if(dataType === 'int')
            {
                dataType = 'integer'
            }


            console.log('dataType',dataType);

            var base = filename + '#';
            base = base.replace('/OntoEN','');
            base=base.replace('theworldavatar','jparksimulator'); //because in electrical it use jparksimulator instead of theworldavatar
            var value = obj.value;
            console.log(targetIRI);
            console.log(base);
            
            if(targetIRI)
            {

                             
                var deleteUpdate = "DELETE WHERE {<" + base + targetIRI + "> <http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#numericalValue> " + "?o.}";
                var insertUpdate = "INSERT DATA {<" + base + targetIRI + "> <http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#numericalValue> " + "\""+value + "\"^^<http://www.w3.org/2001/XMLSchema#"+dataType+">.}";
                    
                    

                console.log('deleteUpdate',deleteUpdate);
                console.log('insertUpdate',insertUpdate);

                sampleUpdate.push(deleteUpdate);
                sampleUpdate.push(insertUpdate);
                
                uri.push(filename);
                uri.push(filename);
                
            }
        }

        var myUrl = 'http://www.theworldavatar.com/Service_Node_BiodieselPlant3/SPARQLEndPoint?uri='
            + encodeURIComponent(JSON.stringify(uri)) + '&update='
            + encodeURIComponent(JSON.stringify(sampleUpdate)) + '&mode=update';
            console.log("url="+myUrl);
        var request = $.ajax({
            url: myUrl,
            type: 'GET',
            contentType: 'application/json; charset=utf-8'
        });

        request.done(function(data) {
            console.log('data received', data);
            loop();


        });

        request.fail(function(jqXHR, textStatus) {
            // your failure code here
        });


    },
    callback : function(){
        console.log('all done in callback');
        //var path = "C:@TOMCAT@webapps@ROOT@OntoEN@startSimulation.bat>" + filename.split('.com/')[1].split('.owl')[0] + '>' + opt;


        //var url = 'http://www.theworldavatar.com/Service_Node_BiodieselPlant3/startScript?path=' + encodeURIComponent(path);
        // var url = 'http://localhost:8080/JPS_POWSYS/ENAgent/startsimulation'+opt;
        var url = 'http://www.theworldavatar.com/JPS_POWSYS/ENAgent/startsimulation'+opt;
        
        var request = $.ajax({
            url: url,
            type: 'GET',
            data: { "electricalnetwork":iriofnetwork },
            contentType: 'application/json; charset=utf-8'
        });

        request.done(function(data) {
            
            console.log('simulation finished');
            var url = 'http://www.theworldavatar.com/kb/sgp/jurongisland/jurongislandpowernetwork/' + selectedId;

            
            sendRequest(url,function (response) {
            
                console.log('simulation finished url', url)
                response = sortByKey(response,'name');
                var inputsHTML = '';
                
                var nameSet = [];
                
                for(var item in response){
                    var pair = response[item];

                    if(pair['value'].includes('.owl'))
                    {

                    }
                    else{
                    
                    if(nameSet.includes(pair['name'])){
                        
                    }
                    else{
                    
                                                
                    console.log(pair['name']);
                    var inputLine = '<tr><td><label>' + pair['name'] +'</label></td><td><input data-dataType="' + pair['datatype'] + '" value="' + pair['value'] + '" style="float: right;"></td><td>' + pair['unit'] + '</td></tr>';
                        inputsHTML = inputsHTML + inputLine;
                        nameSet.push(pair['name'])                              
                    }

                    }
                }

                var _div;
                if(_type==='kml'){
                    console.log('---kml');
                    _div = document.getElementById('inputsContainer');
                }
                else{
                    console.log('---line');
                    _div = document.getElementById('something');
                }
                
                _div.innerHTML = '<table data-url='+ url +' id="inputsTable">' + inputsHTML + '</table><br/><button onclick="SubmitTable(this)">OPF</button><button onclick="SubmitTable(this)">PF</button>' +
'<img id="myProgressBar" style="width:100px;height:100px;display:none" src="https://media.giphy.com/media/3oEjI6SIIHBdRxXI40/giphy.gif"/><br/>';
                //location.reload(); //temp
            });



        });

        request.fail(function(jqXHR, textStatus) {
            // your failure code here
        });

    }
});

}
function sortByKey(array, key) {
return array.sort(function(a, b) {
    var x = a[key]; var y = b[key];
    return ((x < y) ? -1 : ((x > y) ? 1 : 0));
});
}

function constructLineMenu(id,callback){
    var url = 'http://www.theworldavatar.com/kb/sgp/jurongisland/jurongislandpowernetwork' + id;
    selectedId =   id;

    console.log('url',url);
    sendRequest(url,function (response) {

        var inputsHTML = '';
        for(var item in response)
        {
            var pair = response[item];
            if(pair['value'].includes('.owl'))
            {

            }
            else{

                console.log(pair['name']);
                var inputLine = '<tr><td><label>' + pair['name'] +'</label></td><td><input data-dataType="' + pair['datatype'] + '" value="' + pair['value'] + '" style="float: right;"></td><td>' + pair['unit'] + '</td></tr>';
                inputsHTML = inputsHTML + inputLine;
            }
        }


        var div = document.createElement('div');
        div.id = 'something';
        div.style='height:500px';
        
        div.innerHTML = '<table data-type="line" data-url='+ url +' id="inputsTable">' + inputsHTML + '</table><br/><button onclick="SubmitTable(this)">OPF</button><button onclick="SubmitTable(this)">PF</button>'+
            '<img id="myProgressBar" style="width:100px;height:100px;display:none" src="https://media.giphy.com/media/3oEjI6SIIHBdRxXI40/giphy.gif"/><br/>';
        callback(div);

    });
}
