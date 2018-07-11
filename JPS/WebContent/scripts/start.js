$(function(){

    //***************************************************************************
	// default position of map is set at The Hague
	const position = {
	    latitude: 52.07690,
        longitude: 4.29089
    };
    //***************************************************************************

	
    //***************************************************************************
    // initial map
	const osmb = new OSMBuildings({
        baseURL: './OSMBuildings',
        zoom: 5,
        minZoom: 10,
        maxZoom: 25,
        rotation: 0.6,
        tilt: 45.0,
        position: position,
        state: true, // stores map position/rotation in url
        effects: [], // effects: ['shadows']
        attribution: '� 3D <a href="https://osmbuildings.org/copyright/">OSM Buildings</a>'
    }).appendTo('map');
    //***************************************************************************

	
    //***************************************************************************
    // buttons to tilt camera angle, rotate the map, and zoom in/out
    controlButtonsSetter(osmb);
    //***************************************************************************

    
    //***************************************************************************
    osmb.addMapTiles(
        'https://{s}.tiles.mapbox.com/v3/osmbuildings.kbpalbpk/{z}/{x}/{y}.png',
        {
            attribution: '� Data <a href="https://openstreetmap.org/copyright/">OpenStreetMap</a> � � Map <a href="https://mapbox.com/">Mapbox</a>'
        }
    );
    //***************************************************************************

    
    //***************************************************************************
    const twoDimCheckbox = document.querySelector('input[value="2d"]');
    let decreaseTiltTimer;
    let increaseTiltTimer;
    
    osmb.on('pointerdown', function(e) {
        // related to the tilt timers
        clearInterval(decreaseTiltTimer);
        clearInterval(increaseTiltTimer);

        var output = document.getElementById("output");

        if (output.style.display !== "none") {

            var id = osmb.getTarget(e.detail.x, e.detail.y, function(id) {

                var coordinates = osmb.unproject(e.detail.x, e.detail.y);
                console.log(coordinates);

                var longitude = coordinates["longitude"].toFixed(5);
                var latitude = coordinates["latitude"].toFixed(5);

                var coordinatesArray = [];

                if (!isNaN(latitude) && !isNaN(longitude)) {

                    var convertedCoordinates = proj4('EPSG:28992', [parseFloat(longitude), parseFloat(latitude)]);

                    coordinatesArray.push(convertedCoordinates[0]); // latitude
                    coordinatesArray.push(convertedCoordinates[1]); // longitude

                    document.getElementById("longitude").innerHTML = convertedCoordinates[1];
                    document.getElementById("latitude").innerHTML = convertedCoordinates[0];

                    // $.getJSON('/JPS/ADMSOutput',
                    //     {
                    //         coordinatesLonLat: JSON.stringify(coordinatesArray)
                    //     },
                    //     function(data) {
                    //         var concentrations = data;
                    //
                    //         document.getElementById("concentration0").innerHTML = concentrations[2];
                    //         document.getElementById("concentration10").innerHTML = concentrations[3];
                    //         document.getElementById("concentration20").innerHTML = concentrations[4];
                    //         document.getElementById("concentration30").innerHTML = concentrations[5];
                    //
                    //     });
                }
            });
        }
    });
    
    twoDimCheckbox.onchange = function(){
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
    }
    //***************************************************************************

    
    //***************************************************************************
    $("#reaction-select").on('change',function () {
        //show a map of reactions
        console.log("select changed")
        $.ajax('/JSON/chemsrm.json').done(function (reactionlist) {
            //todo: init building
            console.log(reactionlist.length)
            var parent  = $('#reaction-list');
            console.log(parent)
            for(let reaction of reactionlist){
                parent.append('<li>'+reaction+'</li>')
            }
        }).fail(function () {
            console.log("error")
        })
    })
    //***************************************************************************

    
    //***************************************************************************
    $('#start').click(function(){
        //$('#start').attr("disabled", true);
        
        var xmax = parseInt($('#xupper').val());
        var xmin = parseInt($('#xlower').val());
        var ymax = parseInt($('#yupper').val());
        var ymin = parseInt($('#ylower').val());
//        console.log(xmin +" "+xmax + " " + ymin + " " + ymax)
    
//        approximate becasue texture only work on power2(has to be 1:1,1:2,1:4...)
        [xmin, xmax, ymin, ymax] = appro2ratio(xmin, xmax, ymin, ymax);

        const location = $("#location option:selected").text();
        $.ajax('http://www.theworldavatar.com/JPS/ADMSCoordinationAgent?coordinates='+encodeURIComponent(JSON.stringify({'xmin':xmin,'xmax':xmax, 'ymin':ymin, 'ymax':ymax}).replaceAll('"',"'"))).done(function (bdnlist) {
            //todo: init building
//            console.log(JSON.parse(bdnlist))
            initadms3dmap(JSON.parse(bdnlist), osmb, location);
            
        }).fail(function () {
            console.log("error")
        })
    })
    //***************************************************************************

    
    //***************************************************************************
    $("#location").on("change", () => {
        const location = $("#location option:selected").text();

        if(location === "Berlin"){
            osmb.setPosition({
                latitude: 52.51461,
                longitude: 13.23966
            });
            $("#xlower").val("");
            $("#xupper").val("");
            $("#ylower").val("");
            $("#yupper").val("");
        }else if(location === "The Hague"){
            osmb.setPosition({
                latitude: 52.07690,
                longitude: 4.29089
            });
            $("#xlower").val("79480");
            $("#xupper").val("85000");
            $("#ylower").val("454670");
            $("#yupper").val("460000");
        }
    })
    //***************************************************************************
});

// approximate to ratio 1:1 or 1:2
function appro2ratio(xmin, xmax, ymin, ymax){
    x = xmax - xmin;
    y = ymax - ymin;
    ratio = x/y;
    if(ratio >= 2){
        ymax = ymin + x/2;
    } else if( ratio <2 && ratio > 1){
        ymax = ymin + x;
    } else if (ratio <=1 && ratio > 1/2){
        xmax =xmin + y;
    }else {
        xmax = xmin + y /2;
    }
    
    return [xmin, xmax, ymin, ymax];
    
}

String.prototype.replaceAll = function(search, replacement) {
    var target = this;
    return target.replace(new RegExp(search, 'g'), replacement);
};