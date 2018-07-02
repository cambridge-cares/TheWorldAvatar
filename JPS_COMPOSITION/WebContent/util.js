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


function convertJSONToOWL() {
    $.ajax({
        method: "POST",
        url: "http://localhost:8080/JPS_COMPOSITION/ServiceConvertToOWL",
        data: convertNodeObjToMSMObj(myDiagram.model)
    })
        .done(function (msg) {
            document.getElementById("mySavedModel").value = msg;
        });
}



function composeService() {
    $.ajax({
        method: "POST",
        url: "http://localhost:8080/JPS_COMPOSITION/ServiceCompositionEndpoint",
        data: convertNodeObjToMSMObj(myDiagram.model)
    })
        .done(function (msg) {
            document.getElementById("mySavedModel").value = msg;
        });
}

function replaceAll(text, search, replacement) {
    return text.replace(new RegExp(search, 'g'), replacement);
};