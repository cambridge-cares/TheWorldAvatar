var compositeService = {};
var serviceList = {};
var executionChain = {};

var hostname = window.location.href;
var defaultRealService = {"class": "go.GraphLinksModel",
    "nodeDataArray": [
        {"key":1, "text":"Composite_Service_11Yju7k1", "category":"Service", "fullIRI":"http://www.theworldavatar.com/Composite_Service_11Yju7k1"},
        {"text":"Operation_pexDwAC", "category":"Operation", "fullIRI":"http://www.theworldavatar.com/Operation_pexDwAC", "httpUrl":"http://www.theworldavatar.com/JPS_COMPOSITION/CoordinateToWeather", "key":-2},
        {"text":"MessageContent_Input_xzbAvBW", "category":"MessageContent_Input", "fullIRI":"http://www.theworldavatar.com/MessageContent_Input_xzbAvBW", "key":-3},
        {"text":"Mandatory_MessagePart_CghedAK", "category":"Mandatory_MessagePart", "fullIRI":"http://www.theworldavatar.com/Mandatory_MessagePart_CghedAK", "key":-4, "params":{"hasValue":"", "hasDatatype":"", "type":"http://test.com/ontology/Region"}},
        {"text":"MessageContent_Output_18YRk5SC", "category":"MessageContent_Output", "fullIRI":"http://www.theworldavatar.com/MessageContent_Output_18YRk5SC", "key":-5},
        {"text":"Mandatory_MessagePart_15wGxcwo", "category":"Mandatory_MessagePart", "fullIRI":"http://www.theworldavatar.com/Mandatory_MessagePart_15wGxcwo", "key":-6, "params":{"hasValue":"", "hasDatatype":"", "type":"http://test.com/ontology/ADMSSimulation"}}
    ],
    "linkDataArray": [
        {"from":1, "to":-2},
        {"from":-2, "to":-3},
        {"from":-3, "to":-4},
        {"from":-2, "to":-5},
        {"from":-5, "to":-6}
    ]};
// This function creates a hashcode basing on the current date in milliseconds
function toHex(input) {
    var hash = "",
        alphabet = "0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHICKLMPOPQRSTUVWXWYZ", // alphanumeric pool
        alphabetLength = alphabet.length;
    do {
        hash = alphabet[input % alphabetLength] + hash;
        input = parseInt(input / alphabetLength, 10);
    } while (input);
    return hash;
}

// This function levers the hashcode function to gibe create instances unique ids
function IRIGenerator(category) {
    var d = new Date();
    d = d.getMilliseconds() * 5155265999;
    return category + '_' + toHex(d);

}

// This function provides the user options of classes to create basing to what the instance's parent class is.
function DropdownDataGenerator(parentCategory) {
    var currentCategories = classMap[parentCategory];
    var result = [];
    if (Array.isArray(currentCategories)) {
        for (var idx in currentCategories) {
            var category = currentCategories[idx];
            result.push({name: category, value: category, selected: (idx === '0')}); // by default, select the first element in the dropdown list
        }
        return result;
    } else {
        return false; // indicates that there is no alternatives, only one option available
    }
}

// This function translate the gojs object to msm json format .
function convertNodeObjToMSMObj(testObj) {
    var nodeDataArray = testObj.nodeDataArray;
    var linkDataArray = testObj.linkDataArray;

    var mandatory_messageParts = [];
    var optional_messageParts = [];
    var inputs = [];
    var outputs = [];
    var operations = [];
    var service = locateElement(1);
    appendToMainObject(service);

    function locateAllChildren(key) {
        // given the key of a node, find all children connected to the node.
        var result = [];
        var currentElement = locateElement(key);

        // Here you create part of the object


        for (var idx in linkDataArray) {
            var element = linkDataArray[idx];

            if (element.from === parseInt(key.toString())) {
                result.push(element.to);
            }
        }

        for (var idx in result) {
            var element = result[idx];
            locateAllChildren(element);
        }
        if (currentElement.category.includes('MessagePart')) {
            // create object of a parameter {'uri': 'http://...', 'value': 'xxx', 'datatypeValue': 'xxx', 'type': 'xxx'}

            var uri = currentElement.fullIRI;
            var params = currentElement.params;
            var hasValue = params.hasValue;
            var hasDatatype = params.hasDatatype;
            var type = params.type;

            var newParameter = {
                'uri': uri,
                'value': hasValue,
                'datatypeValue': hasDatatype,
                'type': type
            };


            if (currentElement.category === 'Mandatory_MessagePart') {
                mandatory_messageParts.push(JSON.stringify(newParameter));
            }
            else {
                optional_messageParts.push(JSON.stringify(newParameter));
            }
        }

        if (currentElement.category.includes('MessageContent')) {

            var new_optional_messageParts = [];
            var new_mandatory_messageParts = [];
            for (var idx_part_mandatory in mandatory_messageParts) {
                new_mandatory_messageParts.push(JSON.parse(mandatory_messageParts[idx_part_mandatory]));
            }

            for (var idx_part_optional in optional_messageParts) {
                new_optional_messageParts.push(JSON.parse(optional_messageParts[idx_part_optional]));
            }

            var newMessageContent = {
                'uri': currentElement.fullIRI,
                'mandatoryParts': new_mandatory_messageParts,
                'optionalParts': new_optional_messageParts
            };


            mandatory_messageParts = [];
            optional_messageParts = [];


            if (currentElement.category === 'MessageContent_Input') {
                inputs.push(JSON.stringify(newMessageContent));
            } else {
                outputs.push(JSON.stringify(newMessageContent));
            }

        }

        if (currentElement.category.includes('Operation')) {

            var newOperation = {
                'uri': currentElement.fullIRI,
                'inputs': inputs,
                'outputs': outputs,
                'httpUrl': currentElement.httpUrl
            };
            inputs = [];
            outputs = [];

            var opt = replaceAll(JSON.stringify(newOperation), "'", "");
            opt = replaceAll(opt, "\"{", "{");
            opt = replaceAll(opt, "}\"", "}");
            opt = opt.replace(/\\/g, "");
            operations.push(opt);
        }


    }


    function locateElement(key) {
        // give the key, find the node in the nodeDataArray
        for (var idx in nodeDataArray) {
            var element = nodeDataArray[idx];
            if (element.key === parseInt(key.toString())) {
                return element
            }
        }
    }

    function appendToMainObject(element) {
        var children = locateAllChildren(element.key);
        for (var idx in children) {
            var child = children[idx];
            locateAllChildren(child);
        }

    }

    var operationsString = JSON.stringify(operations);
    operationsString = replaceAll(operationsString, "\"{", "{");
    operationsString = replaceAll(operationsString, "}\"", "}");
    operationsString = operationsString.replace(/\\/g, "");

    var serviceString = JSON.stringify({'uri': service.fullIRI, 'operations': operationsString}, 4);
    serviceString = replaceAll(serviceString, "\"{", "{");
    serviceString = replaceAll(serviceString, "}\"", "}"); //"[
    serviceString = replaceAll(serviceString, "]\"", "]").replace(/"\[/g, "[");
    serviceString = serviceString.replace(/\\/g, "");
    return serviceString

}

// This function send out a request in the form of JSON object
function sendRequest(agentInJSON, RequestUrl) {

}

// Add a newly defined service to the pool of candidate services
function addToServicePool() {
	
	

	
    $.ajax({
        method: "POST",
        url: hostname + "AddToServicePool",
        data: convertNodeObjToMSMObj(myDiagram.model)
    }).done(function () {
        alert("Added another service to the pool");
    });

}

// Send out a service in the form of JSON to the server, ServiceWriter will be used to translate it into rdf(s) format
function convertJSONToOWL() {
    $.ajax({
        method: "POST",
        url: hostname + "ServiceConvertToOWL",
        data: convertNodeObjToMSMObj(myDiagram.model),
        timeout: 3000
    })
        .done(function (msg) {
            document.getElementById("mySavedModel").value = msg;
        })();
}

function goToVisualization(event) {

    var baseUrl = event.srcElement.parentElement.getAttribute('data');
    baseUrl = baseUrl.replace('fworldavatar.com', window.location.host);
    window.location.href = baseUrl + '?data=' + JSON.stringify(executionChain);
}

// The compose a composite
function composeService() {

    $("#myDiagramDiv").width("0%");
    $("#myDiagramDiv2").height("700px");

	// var result_xf = {"linkFromPortIdProperty":"fromPort","linkToPortIdProperty":"toPort","nodeDataArray":[{"key":"Inputs","inputs":[],"outputs":[{"name":"Reaction Mechanism"},{"name":"Region"}],"loc":"200 400"},{"key":"GetPlantsInRegion","inputs":[{"name":"Region"}],"outputs":[{"name":"Plant"}],"loc":"600 300"},{"key":"RegionToCity","inputs":[{"name":"Region"}],"outputs":[{"name":"city"}],"loc":"600 500"},{"key":"SRM Agent","inputs":[{"name":"Engine"},{"name":"Reaction Mechanism"}],"outputs":[{"name":"NonReusable WasteProduct"}],"loc":"900 0"},{"key":"AccuWeather","inputs":[{"name":"city"}],"outputs":[{"name":"WeatherCondition"}],"loc":"900 200","score":"Score: 2.66"},{"key":"BuildingQuery","inputs":[{"name":"Region"},{"name":"city"}],"outputs":[{"name":"BuildingType"}],"loc":"900 400"},{"key":"OpenWeatherMap","inputs":[{"name":"city"}],"outputs":[{"name":"WeatherCondition"}],"loc":"900 600","score":"Score: 5.03"},{"key":"YahooWeather","inputs":[{"name":"city"}],"outputs":[{"name":"WeatherCondition"}],"loc":"900 800","score":"Score: 3.18"},{"key":"ADMS","inputs":[{"name":"Region"},{"name":"WeatherCondition"},{"name":"NonReusable WasteProduct"},{"name":"BuildingType"}],"outputs":[{"name":"Table"}],"loc":"1200 400"}],"linkDataArray":[{"from":"Inputs","to":"GetPlantsInRegion","fromPort":"Region","toPort":"Region","curviness":60},{"from":"Inputs","to":"RegionToCity","fromPort":"Region","toPort":"Region","curviness":60},{"from":"RegionToCity","to":"AccuWeather","fromPort":"city","toPort":"city","curviness":60},{"from":"Inputs","to":"BuildingQuery","fromPort":"Region","toPort":"Region","curviness":60},{"from":"RegionToCity","to":"BuildingQuery","fromPort":"city","toPort":"city","curviness":60},{"from":"GetPlantsInRegion","to":"SRM Agent","fromPort":"Plant","toPort":"Engine","curviness":60},{"from":"RegionToCity","to":"OpenWeatherMap","fromPort":"city","toPort":"city","curviness":60},{"from":"RegionToCity","to":"YahooWeather","fromPort":"city","toPort":"city","curviness":60},{"from":"Inputs","to":"ADMS","fromPort":"Region","toPort":"Region","curviness":60},{"from":"AccuWeather","to":"ADMS","fromPort":"WeatherCondition","toPort":"WeatherCondition","curviness":60},{"from":"OpenWeatherMap","to":"ADMS","fromPort":"WeatherCondition","toPort":"WeatherCondition","curviness":60},{"from":"YahooWeather","to":"ADMS","fromPort":"WeatherCondition","toPort":"WeatherCondition","curviness":60},{"from":"BuildingQuery","to":"ADMS","fromPort":"BuildingType","toPort":"BuildingType","curviness":60},{"from":"SRM Agent","to":"ADMS","fromPort":"NonReusable WasteProduct","toPort":"NonReusable WasteProduct","curviness":60},{"from":"Inputs","to":"SRM Agent","fromPort":"Reaction Mechanism","toPort":"Reaction Mechanism","curviness":60}]};
	
	
	
	
    // visualizeComposition(result_xf);
	
	 
    console.log('original', JSON.stringify(myDiagram.model))
	console.log('model', convertNodeObjToMSMObj(myDiagram.model));
	
    $.ajax({
        method: "POST",
        url: hostname + "CompositionEndpoint",
        data: {'query': convertNodeObjToMSMObj(myDiagram.model)},
        timeout: 5000

    })
        .done(function (msg) {
        	
        	console.log('----- original msg -----')
        	console.log(msg)
            compositeService = msg;
        	
        	
            visualizeComposition(convertComposition(JSON.parse(msg)));
            console.log('-------------- convert ----------------')
            console.log(JSON.stringify(convertComposition(JSON.parse(msg))));

        })
        .fail(function (error) {
            alert('This might not be a valid composite service')
        });
		 
		
		
}

function loadRealComposite() {

    console.log("new .... ");
    console.log(JSON.stringify(myDiagram.model));

	//document.getElementById("mySavedModel").value =  JSON.stringify(defaultRealService);
    //refresh(defaultRealService);

}

function sendToExecutor(){
	//var compositeServiceObj = JSON.parse(compositeService);
	//compositeServiceObj['eliminationList'] = serviceList;
	$("#visualizationSelection").height('700px');
	$("#myDiagramDiv2").width("60%");
    $("#buttonRow").hide();
    $('.cards').show();
    $('#visualizationSelectionOutput').show();
    $('#visualizationSelection').show();
	
	var compositeServiceObj = composite_agent;
	console.log("input for executor= ");
	console.log(JSON.stringify(compositeServiceObj));
 

    $.ajax({
        method: "POST",
        url: hostname + "ExecutionEndpoint",
        data: {'query':JSON.stringify(compositeServiceObj)},
        timeout: 2000

    })
        .done(function (msg) {
            executionChain = JSON.parse(msg);
            console.log("===== execution chain =====")
            console.log(executionChain)

        	console.log("==== Trigger recommendation ====");
        	console.log("Initial inputs")
        	console.log(compositeServiceObj['initialInputs']);
        	// Get the inputs ... Make the recommendation
            let agents = selectInputMap(getInputTypes(compositeServiceObj['initialInputs'])[0]);
            let HTML = generateAgentCardHTML(agents);
            PopulateInputs(HTML);


        })
        .fail(function (error) {
            alert('This might not be a valid composite service')
        });
}

function optimizeService() {
    $.ajax({
        method: "POST",
        url: hostname + "OptimizationEndpoint",
        data: {'query':compositeService},
        timeout: 10000

    })
        .done(function (msg) {
			
			
			serviceList = ["http://www.theworldavatar.com/kb/agents/Service__AccuWeather.owl#Service","http://www.theworldavatar.com/kb/agents/Service__YahooWeather.owl#Service"]
            //serviceList = JSON.parse(msg);
			keys = ['YahooWeather','AccuWeather'];
			console.log("--------------- Service List ---------------")
            console.log(serviceList);
            (function myLoop(i) {
                console.log(i);
                setTimeout(function () {
                    //var key = IRIProcessor(serviceList[i]).replace("Service__").replace('.owl');		
					var key = keys[i];
					console.log('key', key);
                    var node = myDiagram.findNodeForKey(key);
                    myDiagram.remove(node);         //  your code here
                    if (i--) myLoop(i);      //  decrement i and call myLoop again if i > 0
                }, 500)
            })(serviceList.length - 1);

 

        })
        .fail(function (error) {
            alert('This might not be a valid composite service')
        });


}

function replaceAll(text, search, replacement) {
    return text.replace(new RegExp(search, 'g'), replacement);
}

function get_inputs_arrays (){
	
	
}

function seperate_init_inputs(result){
	var inputs_uri = result.initialInputs;
	var processed_uri = [];
	inputs_uri.forEach(function(uri){
		var input_name = uri;
//		try {
//			  input_name = uri.split('#')[1];		
//			}
//		catch(err) {
//		      input_name = uri;
//			}
		processed_uri.push({"name": input_name});
	});
	return processed_uri;
}

function split_agent_name(agent_uri){
	return agent_uri.split('__')[1].split("#")[0].split(".owl")[0];	
}

function search_for_downstream_agent_and_register_connetion(visualization_object, is_initial_input, result, outputs, from_agent_name){
	// go through the "input_mapping" and returns the list of agents ... as well as creating a link
	/*  
	 {
      "from": "Inputs",
      "to": "SRM Agent",
      "fromPort": "Reaction Mechanism",
      "toPort": "Reaction Mechanism",
      "curviness": 60
    }
	 */
	
	let connection = {"from": "", "to": "", "fromPort": "", "toPort": "", "curviness": 60}
	
 
//	for (let output in outputs){
//	outputs.forEach(function(output){	
		let output_uri = "";
	
		if(is_initial_input){
			output_uri = outputs.name;
			
			
			console.log('============== output uri for init inputs ==============')
			console.log(output_uri)
			console.log('========================================================')
		}
		else{
			let outputs = result['output_mapping'][from_agent_name];
			console.log(from_agent_name)
			console.log("############################")
			console.log('outputs', outputs)
			for (let output in outputs){
				output_uri = output;				
			}
		}
		
		for (let agent_uri in result.input_mapping){
			
			agent_name = split_agent_name(agent_uri);			
//			let agent_name = agent_uri;
//			try{
//				 agent_name = split_agent_name(agent_uri);			
//			}
//			catch(err){
//				console.log(err);
//			}
			 
			// console.log(result.input_mapping[agent_uri]);
			// go through the map, see whether the "output" matches anything
			
			for (let input_uri in result.input_mapping[agent_uri]) // check the inputs in every agent
			{
				console.log('=============== input and output comparison =============')
				console.log(input_uri, output_uri)
				console.log('=========================================================')
				
				if (input_uri === output_uri) { 
					// create a connection 
				if (is_initial_input){
						// then "from" is "Input"
					connection["from"] = "Inputs";
					connection["to"]   = agent_name;
					connection["toPort"] = result.input_mapping[agent_uri][input_uri];
					connection["fromPort"] = result['output_mapping'][from_agent_name][output_uri];
		  			} 
				else{
					// all the "to" part stay the same, "from" will be the upstream agent ... 
					connection["from"] = split_agent_name(from_agent_name);
					connection["to"]   = agent_name;
					connection["toPort"] = result.input_mapping[agent_uri][input_uri];
					connection["fromPort"] = result['output_mapping'][from_agent_name][output_uri];
				}
				
				console.log('----------- the connections created ------------------')
				console.log(connection);
				visualization_object['linkDataArray'].push(connection);
				}
			}
				
		}
//	}
	
}

function calculate_loc(layer_index, agent_index){
	let x = (layer_index + 3) * 300;
	let y = agent_index * 100;
	return x.toString() + " " + y.toString();
}

function create_agent_node(agent_uri, result, layer_index, agent_index){
	let agent_node = {"key":"","inputs":[],"outputs":[],"loc":""};
	// let node_inputs_and_outputs = {'inputs': [], 'outputs':[]};
	
	let agent_name=  split_agent_name(agent_uri);	
	agent_node['key'] = agent_name;
	
	console.log('======================= creating an agent node ======================');
	console.log(agent_uri);
	let outputs = result['output_mapping'][agent_uri];
	for (let key in outputs){
//		console.log(key)
//		console.log(outputs[key])
		let output_name = outputs[key];
		agent_node['outputs'].push({"name": output_name});
	}

	let inputs = result['input_mapping'][agent_uri]; 	
	for (let key in inputs){
//		console.log(key)
//		console.log(inputs[key])
		let input_name = inputs[key];
		agent_node['inputs'].push({"name": input_name});
	}
	 
	agent_node['loc'] = calculate_loc(layer_index, agent_index);
	return agent_node;
	 
}

function convertComposition(result) {
	// 1. Construct the object for the initial inputs 
	
	let visualization_object = {'nodeDataArray':[], 'linkDataArray':[], 'linkFromPortIdProperty': 'fromPort', 'linkToPortIdProperty': 'toPort'};
	
    var init_input_node = {
        "key": "Inputs",
        "inputs": [],
        "outputs": [],
        "loc": "200 400"
      }

    init_input_node.outputs = seperate_init_inputs(result);
    visualization_object.nodeDataArray.push(init_input_node); // add the initial node(s) to the visualization object ... 
    search_for_downstream_agent_and_register_connetion(visualization_object, true, result, init_input_node.outputs, null);
    
	var layers = result.layers; // the "layers" array stores a list of URIs for the agents 
	
	/*   an example of layers ... 
	 *   "layers": [
    [
      "http://www.theworldavatar.com/kb/agents/Service__GetPlantsInRegion.owl#Service",
      "http://www.theworldavatar.com/kb/agents/Service__RegionToCity.owl#Service"
    ],
    [
      "http://www.theworldavatar.com/kb/agents/Service__SRMEmissions.owl#Service",
      "http://www.theworldavatar.com/kb/agents/Service__OpenWeatherMap.owl#Service",
      "http://www.theworldavatar.com/kb/agents/Service__AccuWeather.owl#Service",
      "http://www.theworldavatar.com/kb/agents/Service__PowerPlant.owl#Service",
      "http://www.theworldavatar.com/kb/agents/Service__YahooWeather.owl#Service",
      "http://www.theworldavatar.com/kb/agents/Service__BuildingQuery.owl#Service"
    ],
    [
      "http://www.theworldavatar.com/kb/agents/Service__ADMS.owl#Service"
    ],
    []
  ]
	 */
	var layer_index = 0;
	layers.forEach(function (layer) {
		// iterate through all the layers and construct elements in the "nodeDataArray" ... 
		  
		// stop if encounters an empty layer 
		// 1. agents within the first layer are only connected to Inputs 
		
		agent_index =0;
		layer.forEach(function (agent_uri){
			// give the outputs of those agents ... 
			
			// console.log('----------- agent uri ----------')
			// console.log(agent_uri)
			let outputs = [];
			 
			// create the node for an agent ...
			// {"key":"GetPlantsInRegion","inputs":[{"name":"Region"}],"outputs":[{"name":"Plant"}],"loc":"600 300"} 
			// get agents' inputs and outputs in strings ... from result[input_mapping] and result[output_mapping]
			let agent_node = create_agent_node(agent_uri, result,layer_index, agent_index);
			visualization_object["nodeDataArray"].push(agent_node);
			search_for_downstream_agent_and_register_connetion(visualization_object, false, result, outputs , agent_uri)
			agent_index++;
		});
		layer_index++;
	});
	
	
	console.log('========= visualization object ===========')
	console.log(visualization_object)
	console.log('==========================================')
	
	// visualization_object  = {"linkFromPortIdProperty":"fromPort","linkToPortIdProperty":"toPort","nodeDataArray":[{"key":"Inputs","inputs":[],"outputs":[{"name":"Reaction Mechanism"},{"name":"Region"}],"loc":"200 400"},{"key":"GetPlantsInRegion","inputs":[{"name":"Region"}],"outputs":[{"name":"Plant"}],"loc":"600 300"},{"key":"RegionToCity","inputs":[{"name":"Region"}],"outputs":[{"name":"city"}],"loc":"600 500"},{"key":"SRM Agent","inputs":[{"name":"Engine"},{"name":"Reaction Mechanism"}],"outputs":[{"name":"NonReusable WasteProduct"}],"loc":"900 0"},{"key":"AccuWeather","inputs":[{"name":"city"}],"outputs":[{"name":"WeatherCondition"}],"loc":"900 200","score":"Score: 2.66"},{"key":"BuildingQuery","inputs":[{"name":"Region"},{"name":"city"}],"outputs":[{"name":"BuildingType"}],"loc":"900 400"},{"key":"OpenWeatherMap","inputs":[{"name":"city"}],"outputs":[{"name":"WeatherCondition"}],"loc":"900 600","score":"Score: 5.03"},{"key":"YahooWeather","inputs":[{"name":"city"}],"outputs":[{"name":"WeatherCondition"}],"loc":"900 800","score":"Score: 3.18"},{"key":"ADMS","inputs":[{"name":"Region"},{"name":"WeatherCondition"},{"name":"NonReusable WasteProduct"},{"name":"BuildingType"}],"outputs":[{"name":"Table"}],"loc":"1200 400"}],"linkDataArray":[{"from":"Inputs","to":"GetPlantsInRegion","fromPort":"Region","toPort":"Region","curviness":60},{"from":"Inputs","to":"RegionToCity","fromPort":"Region","toPort":"Region","curviness":60},{"from":"RegionToCity","to":"AccuWeather","fromPort":"city","toPort":"city","curviness":60},{"from":"Inputs","to":"BuildingQuery","fromPort":"Region","toPort":"Region","curviness":60},{"from":"RegionToCity","to":"BuildingQuery","fromPort":"city","toPort":"city","curviness":60},{"from":"GetPlantsInRegion","to":"SRM Agent","fromPort":"Plant","toPort":"Engine","curviness":60},{"from":"RegionToCity","to":"OpenWeatherMap","fromPort":"city","toPort":"city","curviness":60},{"from":"RegionToCity","to":"YahooWeather","fromPort":"city","toPort":"city","curviness":60},{"from":"Inputs","to":"ADMS","fromPort":"Region","toPort":"Region","curviness":60},{"from":"AccuWeather","to":"ADMS","fromPort":"WeatherCondition","toPort":"WeatherCondition","curviness":60},{"from":"OpenWeatherMap","to":"ADMS","fromPort":"WeatherCondition","toPort":"WeatherCondition","curviness":60},{"from":"YahooWeather","to":"ADMS","fromPort":"WeatherCondition","toPort":"WeatherCondition","curviness":60},{"from":"BuildingQuery","to":"ADMS","fromPort":"BuildingType","toPort":"BuildingType","curviness":60},{"from":"SRM Agent","to":"ADMS","fromPort":"NonReusable WasteProduct","toPort":"NonReusable WasteProduct","curviness":60},{"from":"Inputs","to":"SRM Agent","fromPort":"Reaction Mechanism","toPort":"Reaction Mechanism","curviness":60}]};
	 
    return visualization_object;
}

function IRIProcessor(IRI) {
	
	
//	console.log('======== IRI ==========')
//	console.log(IRI)
//	console.log('=======================')
	
	if (IRI === undefined){
		return null;
	}
	
	if (IRI.endsWith("#Service")){
		return IRI.split("/").slice(-1)[0].split('#')[0]
	}
	
    if (IRI.includes("#")) {
        return IRI.split("#").slice(-1)[0]
    }
    else {
        return IRI.split("/").slice(-1)[0]
    }
}

function dynamicSort(property) {
    var sortOrder = 1;
    if (property[0] === "-") {
        sortOrder = -1;
        property = property.substr(1);
    }
    return function (a, b) {
        var result = (a[property] < b[property]) ? -1 : (a[property] > b[property]) ? 1 : 0;
        return result * sortOrder;
    }
}