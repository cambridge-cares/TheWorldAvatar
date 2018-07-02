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
            // create object of a parameter {'uri': 'http://...', 'value': 'xxx', 'datatypeValue': 'xxx', 'modelReference': 'xxx'}

            var uri = currentElement.fullIRI;
            var params = currentElement.params;
            var hasValue = params.hasValue;
            var hasDatatype = params.hasDatatype;
            var modelReference = params.modelReference;

            var newParameter = {
                'uri': uri,
                'value': hasValue,
                'datatypeValue': hasDatatype,
                'modelReference': modelReference
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
    serviceString = replaceAll(serviceString, "]\"", "]").replace(/"\[/g,"[");
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
        url: "http://localhost:8080/JPS_COMPOSITION/AddToServicePool",
        data: convertNodeObjToMSMObj(myDiagram.model)
    }).done(function () {
       alert("Added another service to the pool");
    });

}


// Send out a service in the form of JSON to the server, ServiceWriter will be used to translate it into rdf(s) format
function convertJSONToOWL() {
    $.ajax({
        method: "POST",
        url: "http://localhost:8080/JPS_COMPOSITION/ServiceConvertToOWL",
        data: convertNodeObjToMSMObj(myDiagram.model),
        timeout: 3000
    })
        .done(function (msg) {
            document.getElementById("mySavedModel").value = msg;
        })();
}


// The compose a composite
function composeService() {
    $.ajax({
        method: "POST",
        url: "http://localhost:8080/JPS_COMPOSITION/ServiceCompositionEndpoint",
        data: convertNodeObjToMSMObj(myDiagram.model),
        timeout: 5000

    })
        .done(function (msg) {
            visualizeComposition(convertComposition(JSON.parse(msg)));
        })
        .fail(function (error) {
            alert('This might not be a valid composite service')
        })
    
    
    
    ;
}

function replaceAll(text, search, replacement) {
    return text.replace(new RegExp(search, 'g'), replacement);
}



function convertComposition(result) {

    console.log(result.initialInputs);

    var visualizationObject = {
        linkFromPortIdProperty: "fromPort",
        linkToPortIdProperty: "toPort",
        nodeDataArray: [],
        linkDataArray: []
    };

    var initOutputs = [];
    result.initialInputs.forEach(function (initInput) {
       initOutputs.push({name: IRIProcessor(initInput.modelReference)});
    });

    var initObject = {key: "Inputs",inputs:[], outputs: initOutputs.sort(dynamicSort("name")), loc: "200 0"};
    visualizationObject.nodeDataArray.push(initObject);

   var initLayer = {services: [{operations: [{outputs:[{mandatoryParts: result.initialInputs}]}], uri: 'Inputs'}]};


    var previousLayers = [initLayer];
    var layers = result.layers;
    layers.forEach(function (layer, layerIndex) {
        previousLayers.push(layer);
        console.log(previousLayers);


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
                                    if(previousOutputPart.modelReference === model.modelReference){
                                        var newConnection = {from: IRIProcessor(previousService.uri), to: IRIProcessor(service.uri),
                                            fromPort: IRIProcessor(previousOutputPart.modelReference), toPort: IRIProcessor(model.modelReference)};
                                        visualizationObject.linkDataArray.push(newConnection);
                                        if(layerIndex === previousLayerIndex){
                                            extra_indent = 400;
                                        }

                                    }
                                })
                            })
                        });
                    });
                    inputs.push({name: IRIProcessor(model.modelReference)})
                });
            });
            service.operations[0].outputs.forEach(function (output) {
                output.mandatoryParts.forEach(function (model) {
                    outputs.push({name: IRIProcessor(model.modelReference)})
                })
            });
            serviceObject.inputs = inputs.sort(dynamicSort("name"));
            serviceObject.outputs = outputs.sort(dynamicSort("name"));
            serviceObject.loc = (layerIndex + 1) * 500 + extra_indent + " " + serviceIndex * 200;
            visualizationObject.nodeDataArray.push(serviceObject);
        });

    });

    return visualizationObject;
}

function IRIProcessor(IRI) {
    console.log('IRI', IRI);
    if (IRI.includes("#")){
        return IRI.split("#").slice(-1)[0]
    }
    else{
        return IRI.split("/").slice(-1)[0]
    }
}

function dynamicSort(property) {
    var sortOrder = 1;
    if(property[0] === "-") {
        sortOrder = -1;
        property = property.substr(1);
    }
    return function (a,b) {
        var result = (a[property] < b[property]) ? -1 : (a[property] > b[property]) ? 1 : 0;
        return result * sortOrder;
    }
}