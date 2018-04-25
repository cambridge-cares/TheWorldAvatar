var initadms3dmap  = function (list) {

		proj4.defs("EPSG:28992","+proj=sterea +lat_0=52.15616055555555 +lon_0=5.38763888888889 +k=0.9999079 +x_0=155000 +y_0=463000 +ellps=bessel +towgs84=565.417,50.3319,465.552,-0.398957,0.343988,-1.8774,4.0725 +units=m +no_defs");

    var osmb = new OSMBuildings({
        baseURL: './OSMBuildings',
        zoom: 19.4,
        minZoom: 10,
        maxZoom: 25,
        rotation: 0.6,
        tilt: 45.0,
        // change to variable latitude and longitude
        position: { latitude: 52.07690, longitude: 4.29089 }, // around The Hague
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

    var fieldset = document.getElementById("fieldsetCheckbox");
    var divMap = document.getElementById("map");
    divMap.removeChild(divMap.childNodes[0]);
    divMap.appendChild(fieldset);

    // --- Rendering 3D building models --- //

    $.getJSON('/JPS/ADMSHelper',
        {
            listOfIRIs: JSON.stringify(list)
        },
        function(data) {
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

                var coordinatesArray = []

                if (!isNaN(latitude) && !isNaN(longitude)) {

										var convertedCoordinates = proj4('EPSG:28992', [parseFloat(longitude), parseFloat(latitude)]);

										coordinatesArray.push(convertedCoordinates[0]); // latitude
										coordinatesArray.push(convertedCoordinates[1]); // longitude

										document.getElementById("longitude").innerHTML = convertedCoordinates[1];
										document.getElementById("latitude").innerHTML = convertedCoordinates[0];

                    $.getJSON('/JPS/ADMSOutput',
                        {
                            coordinatesLonLat: JSON.stringify(coordinatesArray)
                        },
                        function(data) {
                            var concentrations = data;

                            document.getElementById("concentration0").innerHTML = concentrations[2];
                            document.getElementById("concentration10").innerHTML = concentrations[3];
                            document.getElementById("concentration20").innerHTML = concentrations[4];
                            document.getElementById("concentration30").innerHTML = concentrations[5];

                        });
                }
            });
        }
    });

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
