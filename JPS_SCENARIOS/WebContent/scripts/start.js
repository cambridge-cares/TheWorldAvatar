$(function() {	

  var selected = [];
  var loadeddata;
  var table = $("#tablescenarios").DataTable();
  var listScenariosUrl = "http://localhost:8080/JPS_SCENARIO/scenariomanagement/listscenarios"
  var selectedScenario;
  var selectedOperation;
 
  $("#loadData").click(function() {	
    loadData();
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
    
    var scenarios = loadeddata.result;
    for(var i in scenarios) {
      var name = scenarios[i].name;
      var type = scenarios[i].type;
      var id = scenarios[i].id;
      var irilink = '<a href="' + id + '">' + id + '</a>';
    
     // warning: don't use table here because there is a difference
     // between dataTable() and DataTable()
     // see https://www.datatables.net/reference/api/ 
     $('#tablescenarios').dataTable().fnAddData( [
        name,
        type,
        irilink
      ]);
    }
  }
  
  $('#createscenariobutton').click(function() {
	  
	  var scenarioname = $('#newscenarioname').val();
	  
	  var url = "http://localhost:8080/JPS_SCENARIO/scenario/" + scenarioname;
	  
	    $.ajax({
	        type: 'GET',
	        url: url,
	        contentType: "text/plain",
	        dataType: 'json',
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
      
      // find object in json array that correspond to the selected row by name comparison
      //var selectedScenario;
	  var selectedname = data[0];
	  var scenarios = loadeddata.result;
	  for(var i in scenarios) {
	    var name = scenarios[i].name;
	    if (name === selectedname) {
	    	selectedScenario = scenarios[i]
	    	break;
	    }
	  }
      
       $('#tableparams2').DataTable().clear();
      showOperations(selectedScenario);
  } );
  
  function showOperations(scenario) {
	  
	  //alert('hello' + scenario.service[0].hasOperation.hasHttpUrl);
	  
	  var operations = scenario.service;	  
	  for (i=0; i<=4; i++) {
		//$("#op"+i).prop('checked', false);
		if (i < operations.length) {
			var httpUrl = operations[i].hasOperation.hasHttpUrl;
			var index = httpUrl.lastIndexOf("/");
			var opName = httpUrl.substring(index+1);
			$("#op"+i).show()
			$("#op"+i).val(opName);
			$("#op"+i+"label").show()
			$("#op"+i+"label").html(opName);
		} else {
			$("#op"+i).hide()
			$("#op"+i+"label").hide()			
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
		if (currentOperation.hasHttpUrl.endsWith(opName)) {
			selectedOperation=currentOperation;
			break;
		}
	  }
	  
	  populateParams();
  });

  
  function populateParams() {
    console.log("populating params for " + selectedOperation.hasHttpUrl);
    
    $('#tableparams2').DataTable().clear();
    
	var params = selectedOperation.hasInput;
    for(var i in params) {
      var inout = "in";
      var name = params[i].hasName;
      var type = params[i].hasType;
      var typelink = '<a href="' + type + '">' + type + '</a>';
      var value =  "<input type=\"text\" id=\"inparamvalue" + i + "\">";
      
     // warning: don't use table here because there is a difference
     // between dataTable() and DataTable()
     // see https://www.datatables.net/reference/api/ 
     $('#tableparams2').dataTable().fnAddData( [
        inout,
    	name,
        typelink,
        value
      ]);
    }
    
	var params = selectedOperation.hasOutput;
    for(var i in params) {
      var inout = "out";
      var name = params[i].hasName;
      var type = params[i].hasType;
      var typelink = '<a href="' + type + '">' + type + '</a>';
      var value = "---";
      
      
     // warning: don't use table here because there is a difference
     // between dataTable() and DataTable()
     // see https://www.datatables.net/reference/api/ 
     $('#tableparams2').dataTable().fnAddData( [
        inout,
    	name,
        typelink,
        value
      ]);
    }
  }
  
  $('#showlinkbutton').click(function() {
	  
	  var json = {};
	  var paramsdata = $('#tableparams2').DataTable().rows().data();
	  var numberInParams = selectedOperation.hasInput.length;
	  console.log("no " + numberInParams);
	  for (i=0; i<numberInParams; i++) {
		  var paramkey = paramsdata[i][1];
		  var paramvalue = $('#inparamvalue' + i).val();
		  // the next line does not work since it adds "paramkey": "...Berlin" instead of "city": "...Berlin"
		  //json.paramkey = paramvalue;  
		  console.log("key=" + paramkey + ", value=" + paramvalue);
		  if (paramvalue.length > 0) {
		  	json["" + paramkey] = paramvalue;
	  	}
	  }

	  var url = selectedOperation.hasHttpUrl + "?query=" + JSON.stringify(json);
	  var urlencoded = selectedOperation.hasHttpUrl + "?query=" + encodeURIComponent(JSON.stringify(json));
	  $("#linkforaction").prop('href', urlencoded);
	  $("#linkforaction").text(url);
  });
  
});
