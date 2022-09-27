var asking = 0;
// Variables accessed throughout the script
$('document').ready(function(){
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
});






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


function make_handshake(target, resultType, successFunction, promises){

	var currentdate = new Date();
    var datetime = "Now: " + currentdate.getDate() + "/"
                + (currentdate.getMonth()+1)  + "/"
                + currentdate.getFullYear() + " @ "
                + currentdate.getHours() + ":"
                + currentdate.getMinutes() + ":"
                + currentdate.getSeconds();



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
			console.log(xhr.status);
			console.log(thrownError);
			asking--;
		}
	}));
}

