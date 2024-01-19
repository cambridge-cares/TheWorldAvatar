/*
var EbusList = [];
for(var i = 1; i < 209; i++)
{
    var paddedIndex = pad(i,3);
    var _uri = "http://www.theworldavatar.com/OntoEN/" + 'EBus-' + paddedIndex + '.owl';
    EbusList.push(_uri);
}

EbusList.forEach(function(listItem){
    console.log(listItem);
    request(listItem, function(response) {
        console.log(response)
    });
});

alert('Finished');

*/


function request(uri,callback)
{
    console.log('uri',uri);

    var googleProjection = 'EPSG:4326'; //google
    var ourProjection = 'EPSG:3857';//our
    $.ajax({
        url: "http://www.theworldavatar.com:82/getAttrList",
        method: "POST",
        data: JSON.stringify({uri: uri}),
        contentType: "application/json; charset=utf-8",
        success: function (attrPairs) {

            var xCoorRegex = new RegExp('V_x_.*');
            var yCoorRegex = new RegExp('V_y_.*');
            var x = '';
            var y = '';

            for (var attr in attrPairs)
            {
                var attrPair = attrPairs[attr];
                if(xCoorRegex.exec(attrPair.name))
                {
                    x = attrPair.value;
                }

                if(yCoorRegex.exec(attrPair.name))
                {
                    y = attrPair.value;
                }
            }

            return callback(proj4(ourProjection,googleProjection, [x,y]));

        }
    });

}

function pad(n, width, z) {
    z = z || '0';
    n = n + '';
    return n.length >= width ? n : new Array(width - n.length + 1).join(z) + n;
}


function writeToKml(coordinates)
{

}
