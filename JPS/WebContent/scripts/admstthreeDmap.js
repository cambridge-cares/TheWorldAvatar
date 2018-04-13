var initadms3dmap  = function (list) {
    
    //var loc = window.location.pathname;
    //var dir = loc.substring(0, loc.lastIndexOf('/'));
    //console.log(dir)
    
    
    
    var osmb = new OSMBuildings({
        baseURL: './OSMBuildings',
        zoom: 19.4,
        minZoom: 10,
        maxZoom: 25,
        rotation: 0.6,
        tilt: 45.0,
        // position: { latitude:1.304270, longitude:103.774396 },
        // change to variable latitude and longitude
        position: { latitude: 52.039479093801496, longitude: 4.357006480574263 }, // around den Haag
        // position: { latitude:1.3042168, longitude:103.7745893 }, // around NUS
        state: true, // stores map position/rotation in url
        effects: [], // effects: ['shadows']
        attribution: '� 3D <a href="https://osmbuildings.org/copyright/">OSM Buildings</a>'
    }).appendTo('map');
    
    osmb.addMapTiles(
        'https://{s}.tiles.mapbox.com/v3/osmbuildings.kbpalbpk/{z}/{x}/{y}.png',
        {
            attribution: '� Data <a href="https://openstreetmap.org/copyright/">OpenStreetMap</a> � � Map <a href="https://mapbox.com/">Mapbox</a>'
        }
    );
    
    // function fillPopup()
    // {
    //     var muri = "http://www.theworldavatar.com/BMS/06_buildings.owl";
    //
    //     $.ajax({
    //         url: "http://www.theworldavatar.com:82/getAttrList",
    //         method: "POST",
    //         data: JSON.stringify({uri: muri}),
    //         contentType: "application/json; charset=utf-8",
    //         success: function (attrPairs) {}
    //         });
    // }
    
    // console.log(fillPopup());
    
    //osmb.addGeoJSONTiles('https://{s}.data.osmbuildings.org/0.2/anonymous/tile/{z}/{x}/{y}.json');
    
    var fieldset = document.getElementById("fieldsetCheckbox");
    var divMap = document.getElementById("map");
    divMap.removeChild(divMap.childNodes[0]);
    divMap.appendChild(fieldset);
    
    // --- Rendering 3D building models --- //
    // var $SCRIPT_ROOT = {{ request.script_root|tojson|safe }};
    /*
    var list = [
        "http://www.theworldavatar.com/107_buildings.owl#Building001",
        "http://www.theworldavatar.com/107_buildings.owl#Building002",
        "http://www.theworldavatar.com/107_buildings.owl#Building003",
        "http://www.theworldavatar.com/107_buildings.owl#Building004",
        "http://www.theworldavatar.com/107_buildings.owl#Building005",
        "http://www.theworldavatar.com/107_buildings.owl#Building006",
        "http://www.theworldavatar.com/107_buildings.owl#Building007",
        "http://www.theworldavatar.com/107_buildings.owl#Building008",
        "http://www.theworldavatar.com/107_buildings.owl#Building009",
        "http://www.theworldavatar.com/107_buildings.owl#Building010",
        "http://www.theworldavatar.com/107_buildings.owl#Building011",
        "http://www.theworldavatar.com/107_buildings.owl#Building012",
        "http://www.theworldavatar.com/107_buildings.owl#Building013",
        "http://www.theworldavatar.com/107_buildings.owl#Building014",
        "http://www.theworldavatar.com/107_buildings.owl#Building015",
        "http://www.theworldavatar.com/107_buildings.owl#Building016",
        "http://www.theworldavatar.com/107_buildings.owl#Building017",
        "http://www.theworldavatar.com/107_buildings.owl#Building018",
        "http://www.theworldavatar.com/107_buildings.owl#Building019",
        "http://www.theworldavatar.com/107_buildings.owl#Building020",
        "http://www.theworldavatar.com/107_buildings.owl#Building021",
        "http://www.theworldavatar.com/107_buildings.owl#Building022",
        "http://www.theworldavatar.com/107_buildings.owl#Building023",
        "http://www.theworldavatar.com/107_buildings.owl#Building024",
        "http://www.theworldavatar.com/107_buildings.owl#Building025",
        "http://www.theworldavatar.com/107_buildings.owl#Building026",
        "http://www.theworldavatar.com/107_buildings.owl#Building027",
        "http://www.theworldavatar.com/107_buildings.owl#Building028",
        "http://www.theworldavatar.com/107_buildings.owl#Building029",
        "http://www.theworldavatar.com/107_buildings.owl#Building030",
        "http://www.theworldavatar.com/107_buildings.owl#Building031",
        "http://www.theworldavatar.com/107_buildings.owl#Building032",
        "http://www.theworldavatar.com/107_buildings.owl#Building033"
    ]
    **/
    // change to variable url path
    $.getJSON('/JPS/ADMSHelper',
        {
            listOfIRIs: JSON.stringify(list)
        },
        function(data) {
            // console.log(data);
            var geojson = data;
            var arrayLength = geojson.length;
            
            for (var i = 0; i < arrayLength; i++) {
                
                try {
                    osmb.addGeoJSON(geojson[i]);
                }
                catch(err) {
                    console.log(err.name)
                }
                // finally {
                //     console.log(i);
                //     console.log(geojson[i]);
                // }
            }
        });
    
    //***************************************************************************
    
    osmb.highlight('w420847275','#00ff00');
    
    
    osmb.on('pointerdown', function(e) {
        // related to the tilt timers
        clearInterval(decreaseTiltTimer);
        clearInterval(increaseTiltTimer);
        
        var output = document.getElementById("output");
        
        if (output.style.display !== "none") {
            
            var id = osmb.getTarget(e.detail.x, e.detail.y, function(id) {
                
                var coordinates = osmb.unproject(e.detail.x, e.detail.y);
                
                var longitude = coordinates["longitude"].toFixed(5);
                var latitude = coordinates["latitude"].toFixed(5);
                
                document.getElementById("longitude").innerHTML = longitude;
                document.getElementById("latitude").innerHTML = latitude;
                
                var coordinatesArray = []
                
                if (!isNaN(latitude)) {
                    coordinatesArray.push(latitude);
                }
                
                if (!isNaN(longitude)) {
                    coordinatesArray.push(longitude);
                }
                
                if (coordinatesArray.length > 0) {
                    // todo: change to variable URL path
                    $.getJSON('http://localhost:8080' + '/JPS/ADMSOutput',
                        {
                            coordinatesLonLat: JSON.stringify(coordinatesArray)
                        },
                        function(data) {
                            // console.log(data);
                            var concentrations = data;
                            
                            document.getElementById("concentration0").innerHTML = concentrations[2];
                            document.getElementById("concentration10").innerHTML = concentrations[3];
                            document.getElementById("concentration20").innerHTML = concentrations[4];
                            document.getElementById("concentration30").innerHTML = concentrations[5];
                            
                        });
                }
                
                // if(id == 'w420847275')
                // {
                //     // alert('clicked')
                //     window.location.assign("http://www.theworldavatar.com/BMSIndoor")
                //
                // }
            });
        }
    });
    //61.0089718,132.8325304
    
    // osmb.addOBJ('002.obj', { latitude: 61.0089718, longitude: 132.8325304}, { id: "my_object_1"});
    
    osmb.on('pointermove', function(e) {
        osmb.getTarget(e.detail.x, e.detail.y, function(id) {
            if (id) {
                document.body.style.cursor = 'pointer';
                osmb.highlight(id, '#ff0000');
                //osmb.highlight('w420847275','#00ff00');
                
            } else {
                document.body.style.cursor = 'default';
                osmb.highlight(null);
                osmb.highlight('w420847275','#00ff00');
            }
        });
    });
    
    //***************************************************************************
    
    
    var twoDimCheckbox = document.querySelector('input[value="2d"]');
    var decreaseTiltTimer;
    var increaseTiltTimer;
    
    twoDimCheckbox.onchange = function() {
        if(twoDimCheckbox.checked) {
            clearInterval(increaseTiltTimer);
            
            var tilt = osmb.getTilt();
            
            function decreaseTilt() {
                tilt--;
                if (tilt != 0) {
                    osmb.setTilt(tilt);
                }
            }
            
            decreaseTiltTimer = setInterval(decreaseTilt, 15);
            decreaseTiltTimer;
            
            var output = document.getElementById("output");
            
            output.style.display = "inline";
            
        } else {
            clearInterval(decreaseTiltTimer);
            
            var tilt = osmb.getTilt();
            
            function increaseTilt() {
                tilt++;
                if (tilt < 45) {
                    osmb.setTilt(tilt);
                }
            }
            
            increaseTiltTimer = setInterval(increaseTilt, 15);
            increaseTiltTimer;
            
            var output = document.getElementById("output");
            
            if (output.style.display !== "none") {
                output.style.display = "none";
                
                document.getElementById("longitude").innerHTML = "";
                document.getElementById("latitude").innerHTML = "";
                document.getElementById("concentration0").innerHTML = "";
                document.getElementById("concentration10").innerHTML = "";
                document.getElementById("concentration20").innerHTML = "";
                document.getElementById("concentration30").innerHTML = "";
            }
        }
    };
    
    //***************************************************************************
    
    var controlButtons = document.querySelectorAll('.control button');
    
    for (var i = 0, il = controlButtons.length; i < il; i++) {
        controlButtons[i].addEventListener('click', function(e) {
            var button = this;
            var parentClassList = button.parentNode.classList;
            var direction = button.classList.contains('inc') ? 1 : -1;
            var increment;
            var property;
            
            if (parentClassList.contains('tilt')) {
                property = 'Tilt';
                increment = direction*10;
            }
            if (parentClassList.contains('rotation')) {
                property = 'Rotation';
                increment = direction*10;
            }
            if (parentClassList.contains('zoom')) {
                property = 'Zoom';
                increment = direction*1;
            }
            if (property) {
                osmb['set'+ property](osmb['get'+ property]()+increment);
            }
        });
    }
}