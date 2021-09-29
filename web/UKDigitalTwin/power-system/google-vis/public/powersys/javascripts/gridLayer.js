var globalKMLEventinfoWindowHtml;
var selectedId;

var colorMap = ['#99f', '#f99', '#9f9', '#f9f', '#39f'];


// Perform other work here ...

// Set another completion function for the request above



function initMapOverlay(map, lines) {
    console.log('lines', lines);
    var infowindow = new google.maps.InfoWindow({
        content: '<h2>Sup!</h2>'
    });
    for (var index in lines) {


        var _line = lines[index];

        console.log('******initialise lines**********');
        console.log(_line);

        var _name = _line['name'];
        var _type = _line['type'];

        console.log('--_type--', _type);
        var _path = _line['coors'];


        var _thickness = _line['thickness'];
        var lineSymbol = {
            path: google.maps.SymbolPath.FORWARD_OPEN_ARROW,
            scale: 2,
            strokeColor: '#333'
        };


        if (_type === 'distribute') {
            var line = new google.maps.Polyline({
                path: _path,
                strokeWeight: _thickness,
                strokeColor: colorMap[_thickness - 3],
                icons: [{
                    icon: lineSymbol,
                    offset: '100%'
                }],
                map: map,
                title: _name
            });


            line.addListener('click', function () {
                var that = this;
                var content = constructLineMenu(this.title, function (_content) {
                    console.log('content', _content);
                    console.log('title:');
                    console.log(this.title);
                    infowindow.setContent(_content);
                    infowindow.open(map, that);
                });

            });
            animateCircle(line, 1);
        }
        else {

            console.log('_path', _path);
            var transformer = new google.maps.Circle({
                strokeColor: '#00ff00',
                strokeOpacity: 0.8,
                strokeWeight: 2,
                fillColor: '#00ff00',
                fillOpacity: 0.55,
                map: map,
                center: _path[0],
                radius: 30,
                title: _name
            });

            transformer.addListener('click', function () {
                var that = this;
                var content = constructLineMenu(this.title, function (_content) {
                    console.log('content', _content);
                    infowindow.setContent(_content);
                    infowindow.open(map, that);
                });

            });
        }





    }


    var kmlLayer = new google.maps.KmlLayer({
        url: 'http://www.theworldavatar.com/OntoEN/UK_grid.kml',
        suppressInfoWindows: false,
        map: map
    });




    kmlLayer.addListener('click', function (kmlEvent) {
        generalOnclick(kmlEvent, 'kml')
    });
}

function generalOnclick(event, type) {
    switch (type) {
        case 'kml':
            setKMLMenu(event);
            break;

    }
}


function calLengthOfLine(x1, x2, y1, y2) {

    return (x1 - x2)(x1 - x2) + (y1 - y2)(y1 - y2)
}

function constructLineMenu(id, callback) {
    console.log('-0-id-0-', id);
    var url = 'http://www.theworldavatar.com/kb/sgp/jurongisland/jurongislandpowernetwork/' + id + '.owl';
    selectedId = id;

    console.log('url', url);
    sendRequest(url, function (response) {

        var inputsHTML = '';
        for (var item in response) {
            var pair = response[item];
            if (pair['value'].includes('.owl')) {

            }
            else {

                console.log(pair['name']);
                var inputLine = '<tr><td><label>' + pair['name'] + '</label></td><td><input data-dataType="' + pair['datatype'] + '" value="' + pair['value'] + '" style="float: right;"></td><td>' + pair['unit'] + '</td></tr>';
                inputsHTML = inputsHTML + inputLine;
            }
        }


        var div = document.createElement('div');
        div.id = 'something';
        div.innerHTML = '<table data-type="line" data-url=' + url + ' id="inputsTable">' + inputsHTML + '</table><br/><button onclick="SubmitTable()">OPF</button><button onclick="SubmitTable()">PF</button>' +
            '<img id="myProgressBar" style="width:100px;height:100px;display:none" src="http://1.bp.blogspot.com/-sbLGKbobBwM/T6PQ2qFAL_I/AAAAAAAAARA/5iQ4SoAY52c/s320/08.gif"/><br/>';
        callback(div);

    });
}



function setKMLMenu(kmlEvent)//Bus
{
    var data = kmlEvent.featureData;
    var nameString = data.name.substr(1);
    var names = nameString.split('/');

    console.log(data);
    console.log('--------------------');
    console.log(nameString);

    var buttonsList = '<p>Please select the Entity you would like to modify</p>';
    for (var index in names) {
        var name = names[index];

        buttonsList = buttonsList + '<div><label>' + name + '</label>&nbsp;&nbsp;&nbsp;&nbsp;' +
            '<span onclick="selectEBus(event)" style= "cursor: pointer;" id="' + name + '" class="glyphicon glyphicon-menu-right"></span></div>'
    }

    buttonsList = '<div id="buttonContainer">' + buttonsList + '</div><hr/><div id="inputsContainer"></div>';
    // set the content of the popup window.
    kmlEvent.featureData.infoWindowHtml = '<div>' + buttonsList + '</div>';

}


function selectEBus(event) {

    console.log('selectBus is called.');

    selectedId = event.srcElement.id;
    var url = 'http://www.theworldavatar.com/kb/sgp/jurongisland/jurongislandpowernetwork/' + event.srcElement.id + '.owl';

    console.log('url:');
    console.log(url);

    console.log('There should be something');
    sendRequest(url, function (response) {

        var inputsHTML = '';
        console.log(response);
        for (var item in response) {
            var pair = response[item];


            console.log('Item and pair:');
            console.log(item);
            console.log(pair);
            if (pair['value'].includes('.owl')) {

            }
            else {

                console.log(pair['name']);
                var inputLine = '<tr><td><label>' + pair['name'] + '</label></td><td><input data-dataType="' + pair['datatype'] + '" value="' + pair['value'] + '" style="float: right;"></td><td>' + pair['unit'] + '</td></tr>';
                inputsHTML = inputsHTML + inputLine;
            }
        }


        var div = document.getElementById('inputsContainer');
        div.innerHTML = '<table data-type="kml" data-url=' + url + ' id="inputsTable">' + inputsHTML + '</table><br/><button onclick="SubmitTable()">OPF</button><button onclick="SubmitTable()">PF</button>' +
            '<img id="myProgressBar" style="width:100px;height:100px;display:none" src="http://1.bp.blogspot.com/-sbLGKbobBwM/T6PQ2qFAL_I/AAAAAAAAARA/5iQ4SoAY52c/s320/08.gif"/><br/>'
    });

}


function sendRequest(url, callback) {
    $.ajax({
        url: "http://www.theworldavatar.com:82/getAttrList",
        method: "POST",
        data: JSON.stringify({ uri: url }),
        contentType: "application/json; charset=utf-8",
        success: function (attrPairs) {
            callback(attrPairs);
        }
    });
}


function SubmitTable() {

    var table = document.getElementById('inputsTable');
    var rows = table.firstElementChild.childNodes;
    var url = table.getAttribute('data-url');
    var type = table.getAttribute('data-type');
    console.log('type', type);

    var JSONArray = {};

    for (var i = 0; i < rows.length; i++) {
        var row = rows[i];
        var name = row.getElementsByTagName('label')[0].innerText;
        var value = row.getElementsByTagName('input')[0].value;
        var datatype = row.getElementsByTagName('input')[0].getAttribute('data-dataType');
        console.log('value', value, 'name', name, 'url', url);
        JSONArray[name] = { name: name, value: value, datatype: datatype }
    }

    var progress = document.getElementById('myProgressBar');
    progress.style.display = 'block';
    updateOwlFile(url, JSONArray, type);

}

// 1. get all Buses

function updateOwlFile(filename, JSONArray, _type) {

    console.log('number', Object.keys(JSONArray).length);
    console.log('JSONArray', Object.keys(JSONArray));

    var allItemsArray = [];
    var indexCounter = 0;
    var temp = [];
    for (var item in JSONArray) {
        if (((indexCounter !== 0) && (indexCounter % 10 === 0)) || (indexCounter === parseInt(Object.keys(JSONArray).length - 1))) {
            if ((indexCounter === parseInt(Object.keys(JSONArray).length - 1))) {
                allItemsArray.push(temp);
                temp = [];
                temp.push(item)
                allItemsArray.push(temp);
            }
            else {
                allItemsArray.push(temp);
                temp = [];
                temp.push(item)
            }


        }
        else {
            temp.push(item)
        }

        indexCounter++;
    }

    console.log(allItemsArray);


    var asyncLoop = function (o) {
        var i = -1,
            length = o.length;

        var loop = function () {
            i++;
            if (i === length) { o.callback(); return; }
            o.functionToLoop(loop, i);
        };
        loop();//init
    };


    asyncLoop({
        length: Math.ceil(Object.keys(JSONArray).length / 10),
        functionToLoop: function (loop, i) {


            var sampleUpdate = [];
            var uri = [];

            var Session = allItemsArray[i];
            console.log('Session', Session);
            for (var j = 0; j < Session.length; j++) {
                var item = Session[j];
                var obj = JSONArray[item];

                console.log('obj', obj, i, j);

                var targetIRI = obj.name;
                var dataType = obj.datatype;

                if (dataType === 'int') {
                    dataType = 'integer'
                }


                console.log('dataType', dataType);

                var base = filename + '#';
                base = base.replace('/OntoEN', '');
                var value = obj.value;
                if (targetIRI) {

                    var deleteUpdate = 'PREFIX system: <http://www.theworldavatar.com/OntoCAPE/OntoCAPE/upper_level/system.owl#>' +
                        'DELETE WHERE{ <' + base + targetIRI + '> system:numericalValue ?o } ';

                    var insertUpdate = "PREFIX system: <http://www.theworldavatar.com/OntoCAPE/OntoCAPE/upper_level/system.owl#>" +
                        "INSERT DATA {<" + base + targetIRI + "> system:numericalValue " + "\"" + value + "\"^^<http://www.w3.org/2001/XMLSchema#" + dataType + ">}";

                    //console.log('deleteUpdate',deleteUpdate);
                    //console.log('insertUpdate',insertUpdate);

                    sampleUpdate.push(deleteUpdate);
                    sampleUpdate.push(insertUpdate);
                    uri.push(filename);
                    uri.push(filename);
                }
            }

            var myUrl = 'http://www.theworldavatar.com/Service_Node_BiodieselPlant3/SPARQLEndPoint?uri='
                + encodeURIComponent(JSON.stringify(uri)) + '&update='
                + encodeURIComponent(JSON.stringify(sampleUpdate)) + '&mode=update';
            var request = $.ajax({
                url: myUrl,
                type: 'GET',
                contentType: 'application/json; charset=utf-8'
            });

            request.done(function (data) {
                console.log('data received', data);
                loop();


            });

            request.fail(function (jqXHR, textStatus) {
                // your failure code here
            });






        },
        callback: function () {
            console.log('all done in callback');
            var path = "C:@TOMCAT@webapps@ROOT@OntoEN@startSimulation.bat>" + filename.split('.com/OntoEN/')[1].split('.owl')[0];

            console.log(path);

            var url = 'http://www.theworldavatar.com/Service_Node_BiodieselPlant3/startScript?path=' + encodeURIComponent(path);
            var request = $.ajax({
                url: url,
                type: 'GET',
                contentType: 'application/json; charset=utf-8'
            });

            request.done(function (data) {

                console.log('simulation finished');
                var url = 'http://www.theworldavatar.com/kb/sgp/jurongisland/jurongislandpowernetwork/' + selectedId + '.owl';
                console.log('There should be something');
                sendRequest(url, function (response) {

                    var inputsHTML = '';
                    console.log(response);
                    for (var item in response) {
                        var pair = response[item];

                        if (pair['value'].includes('.owl')) {

                        }
                        else {

                            console.log(pair['name']);
                            var inputLine = '<tr><td><label>' + pair['name'] + '</label></td><td><input data-dataType="' + pair['datatype'] + '" value="' + pair['value'] + '" style="float: right;"></td><td>' + pair['unit'] + '</td></tr>';
                            inputsHTML = inputsHTML + inputLine;
                        }
                    }

                    var _div;
                    if (_type === 'kml') {
                        console.log('---kml');
                        _div = document.getElementById('inputsContainer');
                    }
                    else {
                        console.log('---line');
                        _div = document.getElementById('something');
                    }
                    _div.innerHTML = '<table data-url=' + url + ' id="inputsTable">' + inputsHTML + '</table><br/><button onclick="SubmitTable()">OPF</button><button onclick="SubmitTable()">PF</button>' +
                        '<img id="myProgressBar" style="width:100px;height:100px;display:none" src="http://1.bp.blogspot.com/-sbLGKbobBwM/T6PQ2qFAL_I/AAAAAAAAARA/5iQ4SoAY52c/s320/08.gif"/><br/>';
                });



            });

            request.fail(function (jqXHR, textStatus) {
                // your failure code here
            });

        }
    });

}
function animateCircle(line, timeOut) {
    var count = 0;
    window.setInterval(function () {
        count = (count + 1) % 200;

        var icons = line.get('icons');
        icons[0].offset = (count / 2) + '%';
        line.set('icons', icons);
    }, timeOut);
}