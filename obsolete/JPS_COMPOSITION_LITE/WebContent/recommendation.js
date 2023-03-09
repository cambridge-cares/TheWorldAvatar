var inputMap = {
    'http://www.theworldavatar.com/OntoEIP/OntoCAPE/OntoCAPE/upper_level/coordinate_system.owl#Coordinate':
        [
            {
                'src' : 'http://www.theworldavatar.com/JPS_COMPOSITION/MapPointSelection/osm.html',
                'img' : 'https://blog.webkid.io/content/images/old/3d-maps-with-osmbuildings/osmb-thumb.jpg',
                'description': 'Select Point from OSMBuilding Map'
            },

            {
                'src' : 'http://www.theworldavatar.com/JPS_COMPOSITION/MapPointSelection/google.html',
                'img' : 'MapPointSelection/mapo.png',
                'description': 'Select Point from Google Map'

            }
        ],
    'http://www.theworldavatar.com/ontology/ontocitygml/OntoCityGML.owl#EnvelopeType':
        [
            {
                'src' : 'http://www.theworldavatar.com/JPS_COMPOSITION/regionSelection',
                'img' : 'regionSelection/thumb.PNG',
                'description': 'Select Region from Google Map'

            },

            {
                'src' : 'http://www.theworldavatar.com/JPS_COMPOSITION/regionSelection/osmRegion.html',
                'img' : 'https://blog.webkid.io/content/images/old/3d-maps-with-osmbuildings/osmb-thumb.jpg',
                'description': 'Select Region from OSMBuilding Map'

            },
        ]
};

var cardTemplate = '<div class="card" style="display: inline-block">\n' +
    '                <div class="blurring dimmable image">\n' +
    '                    <div class="ui inverted dimmer">\n' +
    '                        <div class="content">\n' +
    '                            <div class="center">\n' +
    '                                <div class="ui primary button">Add Friend</div>\n' +
    '                            </div>\n' +
    '                        </div>\n' +
    '                    </div>\n' +
    '                    <img src="{0}">\n' +
    '                </div>\n' +
    '                   <div class="content"    data="{1}" onclick="goToVisualization(event)">\n' +
    '                    <a class="header"      data="{1}"> {2}</a>\n' +
    '                </div>\n' +
    '               </div>';


var outputMap = {};
function selectInputMap(type) {
    return inputMap[type];
}
function getInputTypes(initialInputs) {

    var types = [];
    for (var idx in initialInputs) {
        var input = initialInputs[idx];
        types.push(input['type'])
    }

    return types;
}
String.prototype.format = function (args) {
    var str = this;
    return str.replace(String.prototype.format.regex, function(item) {
        var intVal = parseInt(item.substring(1, item.length - 1));
        var replace;
        if (intVal >= 0) {
            replace = args[intVal];
        } else if (intVal === -1) {
            replace = "{";
        } else if (intVal === -2) {
            replace = "}";
        } else {
            replace = "";
        }
        return replace;
    });
};
String.prototype.format.regex = new RegExp("{-?[0-9]+}", "g");

function generateAgentCardHTML(agentList) {

    var result = '';

    for(var idx in agentList){
        var agent = agentList[idx];

        result = result + '\n' + cardTemplate.format([agent['img'], agent['src'], agent['description']]);
    }
    return result;
}

function PopulateInputs(HTML){
    $('#inputRecommendation').html(HTML);
}

