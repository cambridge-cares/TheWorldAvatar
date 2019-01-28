$(function() {	

  var selected = [];
  var loadeddata;
  var table = $("#example").DataTable();
  //var testDataUrl = "https://raw.githubusercontent.com/chennighan/RapidlyPrototypeJavaScript/master/lesson4/data.json"
  var testDataUrl = "http://localhost:8080/JPS_SCENARIO/scenariomanagement/listscenarios"
  var selectedScenario;
  var selectedOperation;
 
  $("#loadData").click(function() {	
    loadData();
  });

  function loadData() {
    $.ajax({
      type: 'GET',
      url: testDataUrl,
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
    //$("#example").DataTable().clear();
    table.clear();
    
    var scenarios = loadeddata.result;
    for(var i in scenarios)
    {
      var name = scenarios[i].name;
      var type = scenarios[i].type;
      var id = scenarios[i].id;
      var irilink = '<a href="' + id + '">' + id + '</a>';
    
     // warning: don't use table here because there is a difference
     // between dataTable() and DataTable()
     // see https://www.datatables.net/reference/api/ 
     $('#example').dataTable().fnAddData( [
        name,
        type,
        irilink
      ]);
    }
  }
  
  
  $('#example tbody').on('click', 'tr', function () {
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
      
      showOperations(selectedScenario);
  } );
 
  function showOperations(scenario) {
	  
	  //alert('hello' + scenario.service[0].hasOperation.hasHttpUrl);
	  
	  clearParameters();
	  
	  var operations = scenario.service;	  
	  for (i=0; i<=4; i++) {
		$("#op"+i).prop('checked', false);
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
	  
	  clearParameters();
	  showParameters();
  });

  
  function showParameters2(scenario) {
	  // show parameters for selected agent / scenario
	  var parameters = scenario.parameters;
	  for (i=0; i<=4; i++) {
		  if (i < parameters.length) {
			  var isInputParam = parameters[i].input;
			  $("#paraminout"+i).prop('checked', isInputParam);
			  $("#paramname"+i).val(parameters[i].name);
			  $("#paramtype"+i).val(parameters[i].type);
		  }
	  }
  }
  
  function clearParameters() {
	  for (i=0; i<10; i++) {
		$("#paraminout"+i).prop('checked', false);
		$("#paramname"+i).val("");
		$("#paramtype"+i).val("");
	  }
  }
  
  function showParameters() {
	  //alert("scenario=" + selectedScenario.name + ", operation=" + selectedOperation.hasHttpUrl);
	  
	  var params = selectedOperation.hasInput;
	  var numberInputParams = params.length;
	  for (i=0; i<numberInputParams; i++) {
		  $("#paraminout"+i).prop('checked', true);
		  $("#paramname"+i).val(params[i].hasName);
		  $("#paramtype"+i).val(params[i].hasType);
	  }
	  
	  params = selectedOperation.hasOutput;
	  if (params) {
		  for (i=0; i<params.length; i++) {
			  var j = i + numberInputParams;
			  $("#paraminout"+j).prop('checked', false);
			  $("#paramname"+j).val(params[i].hasName);
			  $("#paramtype"+j).val(params[i].hasType);
		  }
	  }	
  }
  
});
