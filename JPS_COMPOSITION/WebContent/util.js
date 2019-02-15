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
    baseUrl = baseUrl.replace('www.theworldavatar.com', window.location.host);
    window.location.href = baseUrl + '?data=' + JSON.stringify(executionChain);
}


// The compose a composite
function composeService() {

    $("#myDiagramDiv").width("0%");
    $("#myDiagramDiv2").height("700px");

    console.log('original', JSON.stringify(myDiagram.model))
	console.log('model', convertNodeObjToMSMObj(myDiagram.model));
	
    $.ajax({
        method: "POST",
        url: hostname + "ServiceCompositionEndpoint",
        data: convertNodeObjToMSMObj(myDiagram.model),
        timeout: 5000

    })
        .done(function (msg) {
        	
        	console.log('----- original msg -----')
        	console.log(msg)
            compositeService = msg;
        	
        	
            visualizeComposition(convertComposition(JSON.parse(msg)));
            console.log('-------------- convert ----------------')
            console.log(convertComposition(JSON.parse(msg)));

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
	var compositeServiceObj = JSON.parse(compositeService);
	compositeServiceObj['eliminationList'] = serviceList;
	$("#visualizationSelection").height('700px');
	$("#myDiagramDiv2").width("60%");
    $("#buttonRow").hide();
    $('.cards').show();
    $('#visualizationSelectionOutput').show();
    $('#visualizationSelection').show();
  

    $.ajax({
        method: "POST",
        url: hostname + "ServiceExecutorEndpoint",
        data: JSON.stringify(compositeServiceObj),
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
        url: hostname + "ServiceOptimizationEndpoint",
        data: compositeService,
        timeout: 10000

    })
        .done(function (msg) {
			
			console.log('----- msg -----')
			console.log(msg);
			if(msg == ''){
				serviceList = ["http://www.theworldavatar.com/kb/agents/Service__AccuWeather.owl#Service",
				"http://www.theworldavatar.com/kb/agents/Service__YahooWeather.owl#Service"];
			}
			else{
				serviceList = JSON.parse(msg);
				console.log(serviceList);
			}

            (function myLoop(i) {
                console.log(i);
                setTimeout(function () {
                    var key = IRIProcessor(serviceList[i]);
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

function convertComposition(result) {


    var visualizationObject = {
        linkFromPortIdProperty: "fromPort",
        linkToPortIdProperty: "toPort",
        nodeDataArray: [],
        linkDataArray: []
    };

    var initOutputs = [];
    result.initialInputs.forEach(function (initInput) {
        initOutputs.push({name: IRIProcessor(initInput.type)});
    });

    var initObject = {key: "Inputs", inputs: [], outputs: initOutputs.sort(dynamicSort("name")), loc: "200 0"};
    visualizationObject.nodeDataArray.push(initObject);

    var initLayer = {services: [{operations: [{outputs: [{mandatoryParts: result.initialInputs}]}], uri: 'Inputs'}]};


    var previousLayers = [initLayer];
    var layers = result.layers;
    layers.forEach(function (layer, layerIndex) {
        previousLayers.push(layer);
        layer.services.forEach(function (service, serviceIndex) {
            var serviceObject = {key: IRIProcessor(service.uri), inputs: [], outputs: [], loc: ""};
            var inputs = [];
            var outputs = [];

            var extra_indent = 0;

            service.operations[0].inputs.forEach(function (input) {
                input.mandatoryParts.forEach(function (model) {
                    // Go and find outputs in the all previous layer and make the connection
                    previousLayers.forEach(function (previousLayer, previousLayerIndex) {
                        previousLayer.services.forEach(function (previousService) {
                            previousService.operations[0].outputs.forEach(function (previousOutput) {
                                previousOutput.mandatoryParts.forEach(function (previousOutputPart) {
                                    if (previousOutputPart.type === model.type) {
                                    	console.log("------- After processor ------- ")
                                    	console.log(IRIProcessor(previousService.uri))
                                        var newConnection = {
                                            from: IRIProcessor(previousService.uri),
                                            to: IRIProcessor(service.uri),
                                            fromPort: IRIProcessor(previousOutputPart.type),
                                            toPort: IRIProcessor(model.type)
                                        };
                                        visualizationObject.linkDataArray.push(newConnection);
                                        if (layerIndex === previousLayerIndex) {
                                            extra_indent = 300;
                                        }

                                    }
                                })
                            })
                        });
                    });
                    inputs.push({name: IRIProcessor(model.type)})
                });
            });
            service.operations[0].outputs.forEach(function (output) {
                output.mandatoryParts.forEach(function (model) {
                    outputs.push({name: IRIProcessor(model.type)})
                })
            });
            serviceObject.inputs = inputs.sort(dynamicSort("name"));
            serviceObject.outputs = outputs.sort(dynamicSort("name"));
            serviceObject.loc = (layerIndex + 1) * 300 + extra_indent + " " + serviceIndex * 200;
            visualizationObject.nodeDataArray.push(serviceObject);
        });

    });

    let nodeDataArray = visualizationObject.nodeDataArray;
    let scoreMap = result['scoreMap'];
    
    console.log('score map', scoreMap);
    for(let idx in nodeDataArray){
    	let nodeData = nodeDataArray[idx];
    	let nodeKey = nodeData['key'];
    	for(let key in scoreMap){
    		if(key.includes(nodeKey)){
    			let scoreArray = scoreMap[key];
    			let sum = 		 parseInt(scoreArray[0]);
    			let number = 	 parseInt(scoreArray[1]);
    			console.log('sum', sum);
    			console.log('num', number);
    			let score = (((sum/ number) / 2132611) * 5).toFixed(2);
    			console.log('score', score);
    			nodeData['score'] = 'Score: ' + score;
    		}
    	}
    	
    	
    }
    
    console.log('visualization object', nodeDataArray);
    
    
    
    return visualizationObject;
}

function IRIProcessor(IRI) {
	
	
	console.log('======== IRI ==========')
	console.log(IRI)
	console.log('=======================')
	
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