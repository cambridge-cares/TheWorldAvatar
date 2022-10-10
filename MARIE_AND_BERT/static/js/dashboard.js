var asking = 0;
// Variables accessed throughout the script
$('document').ready(function(){

	run_full_test()
	run_entity_linking_test()
	run_score_model()
	run_value_lookup()
	   var promises = [];
       make_handshake("pubchem", "json", update_status, promises);
       make_handshake("ontocompchem", "json", update_status, promises);
    // =================== search button and enter in input field =======
    $('#btn_update').click(function (e){
    	console.log('Making handshakes')
        make_handshake("pubchem", "json", update_status, promises);
        make_handshake("ontocompchem", "json", update_status, promises);
        	$.when.apply($, promises).then(function() {
		// Revert button to search icon
	}, function() {
		// Error occurred, dealt with elsewhere
	});
    });

    $('#btn_full_test').click(function (e){
    	run_full_test()
	});

    $('#btn_test_entity_linking').click(function (e){
    	run_entity_linking_test()
	});

    $('#btn_test_score_model').click(function (e){
    	run_score_model()
	});

    $('#btn_test_value_lookup').click(function (e){
    	run_value_lookup()
	});

});

function run_full_test(){
	$("#label_running").html("Running full test ... ")
	console.log('Running full test')
	  $.ajax({url: "/full_test", success: function(result){
    	$("#full_test_result").html(get_time() + '<br/>' + result);
		$("#label_running").html("")

  	}});
}

function run_entity_linking_test(){
	console.log('Running entity linking test')
	$.ajax({url: "/test_entity_linking", success: function(result){
		$("#entity_linking_test_result").html(get_time() + result);
  	}});
}

function run_score_model(){
	console.log('Running score model test')
	$.ajax({url: "/test_score_model", success: function(result){
    	$("#score_model_test_result").html(get_time() + result);
  	}});
}

function run_value_lookup(){
	console.log('Running value lookup test')
	$.ajax({url: "/test_value_lookup", success: function(result){
    	$("#value_lookup_test_result").html(get_time() + result);
  	}});
}



function update_status(target, currentdate){
	console.log('Got the handshake', target)
	let element = $("#" + target + '_states')
	console.log(element)
	element.html('Running        <br/>   ' + currentdate)
	element.css('color', 'green')
}

function update_error(target){
	console.log('Houston, we have a problem')
	let element = $("#" + target + '_states')
	console.log(element)
	element.html('Not running    <br/>       ' + currentdate)
	element.css('color', 'red')
}


function get_time(){
	var currentdate = new Date();
    var datetime = "Last updated: " + currentdate.getDate() + "/"
                + (currentdate.getMonth()+1)  + "/"
                + currentdate.getFullYear() + " @ "
                + currentdate.getHours() + ":"
                + currentdate.getMinutes() + ":"
                + currentdate.getSeconds();
    return datetime
}

function make_handshake(target, resultType, successFunction, promises){

	let datetime = get_time()
	let url = null
	if (target === "pubchem"){
		url = "http://www.theworldavatar.com/blazegraph/namespace/CleanPubChem/sparql"
	}
	else{
		url = "http://www.theworldavatar.com/blazegraph/namespace/ontocompchem/sparql"
	}

	promises.push($.ajax({
		url: url,
		dataType: resultType,
		timeout: (1000 * 60),
		success: function (data) {
			successFunction(target, datetime);
			asking--;
		},
		error: function (xhr, ajaxOptions, thrownError) {
			update_error(target)
			console.log(xhr.status);
			console.log(thrownError);
			asking--;
		}
	}));
}

