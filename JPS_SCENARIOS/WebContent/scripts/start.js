$(function() {	

  var selected = [];
  var loadeddata;
  var table = $("#tablescenarios").DataTable();
  var host = "http://www.theworldavatar.com";
  //var host = "http://localhost:8080";
  var listScenariosUrl = host + "/jps/scenariomanagement/list"
  var selectedScenario;
  var selectedOperation;
  var showscenarios = false;
  
  hideLowerPanels();
 
  $("#loadAgents").click(function() {
	selectedScenario = null;
	showscenarios = false;
    loadData(showscenarios);
  });
  
  $("#loadScenarios").click(function() {
	selectedScenario = null;
	showscenarios = true;
	loadData(showscenarios);
  });

  function loadData() {	  
    $.ajax({
      type: 'GET',
      url: listScenariosUrl,
      contentType: "text/plain",
      dataType: 'json',
      success: function (data) {
        populateDataTable(data);
      },
      error: function (e) {
        console.log("There was an error with your request...");
        console.log("error: " + JSON.stringify(e));
      }
    });
  }
  
  // populate the data table with JSON data
  function populateDataTable(data) {
    console.log("populating data table...");
    
    loadeddata = data;
    
    // clear the table before populating it with more data
    //$("#tablescenarios").DataTable().clear();
    table.clear();
    $('#tableparams2').DataTable().clear();
    
    hideLowerPanels();
    if (showscenarios) {
  	  $('#containernewscenario').show();
    }
    
    var scenarios = loadeddata.result;
    for(var i in scenarios) {
      var name = scenarios[i].name;
      var type = scenarios[i].type;
      if (showscenarios && (type != 'scenario')) {
    	  continue;
      }
      if (!showscenarios && (type != 'agent') && (type != 'composed')) {
    	  continue;
      }
      
      var id = scenarios[i].id;
      var irilink = '<a href="' + id + '" target="_blank">' + id + '</a>';
    
     // warning: don't use table here because there is a difference
     // between dataTable() and DataTable()
     // see https://www.datatables.net/reference/api/ 
     $('#tablescenarios').dataTable().fnAddData( [
        name,
        type,
        irilink
      ]);
    }
    
    if (selectedScenario) {
    	scenarioname = selectedScenario.name;
    	showOperations(scenarioname)
    }
  }
  
  $('#createscenariobutton').click(function() {
	  
	  
	  selectedScenario = null;
	  
	  var scenarioname = $('#newscenarioname').val();
	  
	  var url = host + "/jps/scenario/" + scenarioname;
	  
	    $.ajax({
	        type: 'GET',
	        url: url,
	        contentType: "text/plain",
	        dataType: 'text',
	        success: function (data) {
	          loadData(data);
	        },
	        error: function (e) {
	          console.log("There was an error with your request...");
	          console.log("error: " + JSON.stringify(e));
	        }
	      });
  });
  
  $('#tablescenarios tbody').on('click', 'tr', function () {
      var data = table.row( this ).data();
      //alert( 'You clicked on '+data[0]+'\'s row' );
	  var selectedname = data[0];
	  $('#newscenarioname').val(selectedname);
      showOperations(selectedname);
  } );
  
  function showOperations(scenarioname) {
	  
	  //alert('hello' + scenario.service[0].hasOperation.hasHttpUrl);
	  
	  showLowerPanels()
	  
	  
	  $('#tableparams2').DataTable().clear();
	  
	  // find object in json array that correspond to the selected row by name comparison
	  var scenarios = loadeddata.result;
	  for(var i in scenarios) {
	    var name = scenarios[i].name;
	    if (name === scenarioname) {
	    	selectedScenario = scenarios[i]
	    	
	    	$('#selectedscenario').text(selectedScenario.name)
	    	
	    	break;
	    }
	  }
	  
	  var operations = selectedScenario.service; 
	  for (i=0; i<=14; i++) {
		//$("#op"+i).prop('checked', false);
		if (i < operations.length) {
			var currentOperation = operations[i].hasOperation;
			var opName = null;
			var scenarioWithoutHttpUrl = ((selectedScenario.type === 'scenario') && !currentOperation.hasHttpUrl);
			if (scenarioWithoutHttpUrl || (selectedScenario.type === 'composed')) {
				var opid =  currentOperation.id;
				var index = opid.lastIndexOf('#');
				opName = 'compose (' + opid.substring(index+1) + ')';
			} else {
				var httpUrl = currentOperation.hasHttpUrl;
				var index = httpUrl.lastIndexOf('/');
				opName = httpUrl.substring(index+1);
			}
			$("#op"+i).show()
			$("#op"+i).val(opName);
			$("#op"+i+"label").show()
			$("#op"+i+"label").html(opName);
		} else {
			$("#op"+i).hide()
			$("#op"+i).val("");
			$("#op"+i+"label").hide();
			$("#op"+i+"label").html("");
		}
	  }
	  
	  // pre select the first operation
	  selectedOperation = operations[0].hasOperation;
	  $("#op"+0).prop('checked', true);
	  populateParams();
  }
  
  $('input[type="radio"]').click(function () {	  
	  var opName = $(this).attr("value");
	  var operations = selectedScenario.service;	  
	  for (i=0; i<operations.length; i++) {
		var currentOperation = operations[i].hasOperation;
		if (!currentOperation.hasHttpUrl && opName.startsWith("compose")) {
			selectedOperation=currentOperation;
			break;
		} else if (currentOperation.hasHttpUrl.endsWith(opName)) {
			selectedOperation=currentOperation;
			break;
		}
	  }
	  
	  populateParams();
  });

  
  function populateParams() {
    console.log("populating params for " + selectedOperation.hasHttpUrl);
    
    // does not work
    //$('#tableparams2').DataTable().clear();
    $('#tableparams2').dataTable().fnClearTable(); 
    
    var numberin = 0;
 	var params = selectedOperation.hasInput;
    for(var i in params) {
      numberin = numberin + 1;
      var inout = "in";
      var name = params[i].hasName;
      var type = params[i].hasType;
	  var typelink = '<a href="' + type + '" target="_blank">' + type + '</a>';
	  var isarray = params[i].isArray;
	  if(isarray==null){
		  isarray=false
	  }
	  console.log("input isArray= "+isarray);
	  var value =  "<input type=\"text\" id=\"inparamvalue" + i + "\">";

      if (name === "scenarioagent") {
    	value =  getSelectBoxForScenarioAgent()
      } 
    	  
     // warning: don't use table here because there is a difference
     // between dataTable() and DataTable()
     // see https://www.datatables.net/reference/api/ 
     $('#tableparams2').dataTable().fnAddData( [
        inout,
    	name,
        typelink,
        isarray,
        value
      ]);
    }
    
    // add usecaseurl as input param
    var type = "http://www.w3.org/2001/XMLSchema#string";
	var typelink = '<a href="' + type + '" target="_blank">' + type + '</a>';
	var value =  "<input type=\"text\" id=\"inparamvalue" + numberin + "\">";
    $('#tableparams2').dataTable().fnAddData( [
        "in",
    	"usecaseurl",
        typelink,
        false,
        value
     ]);
    
	var params = selectedOperation.hasOutput;
    for(var i in params) {
      var inout = "out";
      var name = params[i].hasName;
      var type = params[i].hasType;
      var typelink = '<a href="' + type + '" target="_blank">' + type + '</a>';
      var isarray = params[i].isArray;
	  if(isarray==null){
		  isarray=false
	  }
      console.log("output isArray= "+isarray);
      var value = "---";
      
      
     // warning: don't use table here because there is a difference
     // between dataTable() and DataTable()
     // see https://www.datatables.net/reference/api/ 
     $('#tableparams2').dataTable().fnAddData( [
        inout,
    	name,
        typelink,
        isarray,
        value
      ]);
    
    }
    
  }
  
  function getSelectBoxForScenarioAgent() {
	  var box = "<select id=\"selectboxscenarionagent\">"
		+ "<option value=\"\">Select an agent</option>";
	  
	    var scenarios = loadeddata.result;
	    for(var i in scenarios) {
	        var name = scenarios[i].name;
	        var type = scenarios[i].type;
	        var id = scenarios[i].id;
	        
	        if (type === 'agent' || type === 'composed') {
	        	box += "<option value=\"" + id + "\">" + name + "</option>";
	        }
	    }
		
	  box += "</select>";
	  return box;
  }
  
  $('#showlinkbutton').click(function() {
	  showLink(false);
  });
  
  $('#executelinkbutton').click(function() {
	  showLink(true, false);
  });
  
  $('#executelinkandshowresultbutton').click(function() {
	  showLink(true, true);
  });
	
  
  
  
  function showLink(execute, showresult) {
	  var url = selectedOperation.hasHttpUrl;
	  
	  if (selectedScenario.type === 'composed') {
		  url = 'http://www.theworldavatar.com/JPS_COMPOSITION/execute';
	  } else if ((selectedScenario.type === 'scenario' && !url)) {
		  var id = selectedScenario.id;
		  var index = id.lastIndexOf('.owl');
		  url = id.substring(0, index) + '/compose';
  	  }
	  
	  var urlencoded = url;

	  var json = {}
	  var numberInParams = 0;
	  var paramsdata = $('#tableparams2').DataTable().rows().data();
	  if (selectedOperation.hasInput) {
		  
		 
		  if ((url !== null) && url.endsWith('mock')) {
			  
			  var paramvalue = $('#selectboxscenarionagent').val();
			  json["scenarioagent"] = paramvalue;

		  } else {
			  
			  if (selectedScenario.type === 'composed') {
				  json["agent"] = selectedScenario.id;
			  } 
		  
			  numberInParams = selectedOperation.hasInput.length;
			  console.log("no " + numberInParams);
			  for (i=0; i<numberInParams; i++) {
				  var paramkey = paramsdata[i][1];
				  var paramvalue = $('#inparamvalue' + i).val();
				  // the next line does not work since it adds "paramkey": "...Berlin" instead of "city": "...Berlin"
				  //json.paramkey = paramvalue;  
				  console.log("key=" + paramkey + ", value=" + paramvalue);
				  
				  if (paramvalue && paramvalue.length > 0) {
					
				    try {
				        paramvalue = JSON.parse(paramvalue);
				    } catch (e) {
				    }
					  
				  	json["" + paramkey] = paramvalue;
			  	  }
			  }
		  }
  	  }
	  
	  
	  // add usecaseurl 
	  var paramkey = paramsdata[numberInParams][1];
	  var paramvalue = $('#inparamvalue' + numberInParams).val();
	  // the next line does not work since it adds "paramkey": "...Berlin" instead of "city": "...Berlin"
	  //json.paramkey = paramvalue;  
	  console.log("key=" + paramkey + ", value=" + paramvalue);
	  if (paramvalue && paramvalue.length > 0) {
		    try {
		        paramvalue = JSON.parse(paramvalue);
		    } catch (e) {
		    }	  
		  	//json["" + paramkey] = paramvalue;
		  	json["jpscontext"] = {"usecaseurl": paramvalue}
	  }
	  
	  url += "?query=" + JSON.stringify(json);
	  urlencoded += "?query=" + encodeURIComponent(JSON.stringify(json));

	  $("#linkforaction").prop('href', urlencoded);
	  $("#linkforaction").text(url);
	  
	  
	  if (execute) {
		    $.ajax({
		        type: 'GET',
		        url: urlencoded,
		        contentType: "text/plain",
		        dataType: 'text',
		        success: function (data) {
		          if (showresult) {
		          	alert(data);
		          }
			      opid = selectedOperation.hasHttpUrl
			      console.log("my url " + opid + " endswith " + opid.endsWith('mock'));
			      if (opid.endsWith('mock')) {
			    	  loadData();
		    	  }	
		        },
		        error: function (e) {
		          console.log("There was an error with your request...");
		          console.log("error: " + JSON.stringify(e));
		        }
		      });
	  }
  }
  
  function hideLowerPanels() {
	  $('#tableparams2').dataTable().fnClearTable();
	  $('#containernewscenario').hide();
	  $('#containeroperations').hide();
	  $('#containertableparams2').hide();
	  $('#containerlinkbuttons').hide();
	  $('#newscenarioname').val('');
	  $('#linkforaction').text('');
  }
  
  function showLowerPanels() {
	  $('#containeroperations').show();
	  $('#containertableparams2').show();
	  $('#containerlinkbuttons').show();
	  $('#newscenarioname').val('');
	  $('#linkforaction').text('');
  }
  
});
