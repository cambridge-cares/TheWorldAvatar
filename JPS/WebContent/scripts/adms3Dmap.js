const controlButtonsSetter = osmb => {
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
};


const initadms3dmap  = (list, osmb, location, coordinatesMid) => {

    const position = {};
    // if(location === "The Hague"){
    //     position.latitude = 52.07690;
    //     position.longitude = 4.29089;
    // } else if (location === "Berlin"){
    // 	position.latitude = 52.51461;
    // 	position.longitude = 13.23966;
    // }
        
    $.getJSON('/JPS/ADMSPowerPlantCentrePointGetter',
    	{
    		location
    	},
    	data => {
    		const geojson = data;
    		try {
    			console.log(JSON.stringify(geojson, null, 4));
    			osmb.addGeoJSON(geojson);
    		} catch (err) {
    			console.log(err.name);
    		}
    	});
    	
    position.latitude = coordinatesMid[0];
    position.longitude = coordinatesMid[1];

    osmb.setPosition(position);
    osmb.setZoom(15.7);
	osmb.setTilt(45.0);
	
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
//                    console.log(JSON.stringify(geojson[i], null, 4));
                }
                catch(err) {
                    console.log(err.name)
                }
            }
        });

    //***************************************************************************


    // osmb.highlight('w420847275','#00ff00');
    //
    // osmb.on('pointermove', function(e) {
    //     osmb.getTarget(e.detail.x, e.detail.y, function(id) {
    //         if (id) {
    //             document.body.style.cursor = 'pointer';
    //             osmb.highlight(id, '#ff0000');
    //             //osmb.highlight('w420847275','#00ff00');
    //
    //         } else {
    //             document.body.style.cursor = 'default';
    //             osmb.highlight(null);
    //             osmb.highlight('w420847275','#00ff00');
    //         }
    //     });
    // });
};
