var asking = 0;
// Variables accessed throughout the script
$('document').ready(function(){

	run_full_test()
	run_entity_linking_test()
	run_score_model()
	run_value_lookup()
	run_handshake_pubchem()
	run_handshake_ontocompchem()

    // =================== search button and enter in input field =======
    $('#btn_update').click(function (e){
    	console.log('Making handshakes')
 		run_handshake_pubchem()
		run_handshake_ontocompchem()

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

function run_handshake_pubchem(){
	console.log('Making handshake to pubchem')
	$.ajax({url: "/hand_shake_pubchem", success: function(result){
    	$("#pubchem_states").html(get_time() + "<br/>" +result);
  	}});
}

function run_handshake_ontocompchem(){
	console.log('Making handshake to ontocompchem')
	$.ajax({url: "/hand_shake_ontocompchem", success: function(result){
    	$("#ontocompchem_states").html(get_time() + "<br/>" +result);
  	}});
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
