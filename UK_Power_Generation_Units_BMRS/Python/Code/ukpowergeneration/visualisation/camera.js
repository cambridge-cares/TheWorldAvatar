
const ROW_TEMPLATE = `
<div id="animationRow">
    <select id="animationType" onchange="typeChange(this)">
        <option value="fly">Fly</option>
        <option value="rotate">Rotate</option>
    </select>
    <div id="innerRow"></div>
</div>
`;

const FLY_CONTROLS = `
<input type="number" id="lngInput" class="long" placeholder="Longitude" min="-180" max="180">
<input type="number" id="latInput" class="long" placeholder="Latitude" min="-90" max="90">
<input type="number" id="pitchInput" class="short" placeholder="Pitch" min="0" max="60">
<input type="number" id="bearingInput" class="short" placeholder="Bearing" min="0" max="360">
<input type="number" id="zoomInput" class="short" placeholder="Zoom" step="0.5" min="1" max="20">
<input type="number" id="delayInput" class="short" placeholder="Delay" step="100" min="0">
<input type="number" id="curveInput" class="short" placeholder="Curve" step="0.1" min="0.1">
<input type="number" id="speedInput" class="short" placeholder="Speed" step="0.1" min="0.1">
<div id="deleteRow" onclick="deleteRow(this)">
    <img src="./img/close.png"/>
</div>
`;

const ROTATE_CONTROLS = `
<input type="number" id="lngInput" class="long" placeholder="Longitude" min="-180" max="180">
<input type="number" id="latInput" class="long" placeholder="Latitude" min="-90" max="90">
<input type="number" id="pitchInput" class="short" placeholder="Pitch" min="0" max="60">
<input type="number" id="zoomInput" class="short" placeholder="Zoom" step="0.5" min="1" max="20">
<input type="number" id="delayInput" class="short" placeholder="Delay" step="100" min="0">
<input type="number" id="durationInput" class="short" placeholder="Duration" min="100" max="999999">
<input type="number" id="speedInput" class="short" placeholder="Speed" step="0.1" min="0.1">
<div id="deleteRow" onclick="deleteRow(this)">
    <img src="./img/close.png"/>
</div>
`;

var pendingAnimations = [];
var animationMap;
var animating;
var rotating;
var rotateStart;
var rotateTime;
var rotateSpeed;

var buildingHeight = 0;

/**
* Adds a key listener to detect the event that opens the
* animation controls page.
*/
function setupDetection(map) {
animationMap = map;
document.addEventListener('keydown', function(event) {
    if (event.ctrlKey && event.key === '`') {
        openControls();
    }
});

document.addEventListener('keydown', function(event) {
    if (event.ctrlKey && event.key === 'ArrowDown') {
        if(buildingHeight > 0) buildingHeight -= 0.5;
        if(buildingHeight <= 0) {
            animationMap.setLayoutProperty("other_buildings", "visibility", "none");
        } 
    } else if (event.ctrlKey && event.key === 'ArrowUp') {
        buildingHeight += 0.5;
        if(buildingHeight > 0) {
            animationMap.setLayoutProperty("other_buildings", "visibility", "visible");
        }
    }

    animationMap.setPaintProperty(
        "other_buildings", 
        "fill-extrusion-height", 
        ["+", buildingHeight, ["%", ["id"], 5]]
       
    );
    animationMap.setFilter(
        "other_buildings",
        [">", ["-", buildingHeight, ["get", "offset"]], 0]
    )
});
}

/**
* Opens the animation controls page.
*/
function openControls() {
// Read the camera.html file and show it in a dialog
$.get("camera.html", function(data) {
    // Set content
    $("#animationDialog").html( data );

    // Open modal dialog
    $('#animationDialog').dialog({
        width: (window.innerWidth - 200),
        height: (window.innerHeight - 200),
        modal: true,
        resizable: false,
        zIndex: 10000,
        dialogClass: "noTitleStuff"
    });

    // Hide the title bar
    $(".ui-dialog-titlebar").hide();

    // Populate current settings
    document.getElementById("lngInput").value = animationMap.getCenter().lng.toFixed(7);
    document.getElementById("latInput").value = animationMap.getCenter().lat.toFixed(7);
    document.getElementById("pitchInput").value = animationMap.getPitch();
    document.getElementById("bearingInput").value = animationMap.getBearing();
    document.getElementById("zoomInput").value = animationMap.getZoom();

    // Setup listeners for starting conditions
    startingListeners();

    // Single starting row
    addRow();
});
}

function startingListeners() {
var container = document.getElementById("animationLeft");
var inputs = container.querySelectorAll("input");

inputs.forEach(input => {
    input.addEventListener('input', function (evt) {
        switch(input.id) {
            case "lngInput":
                animationMap.setCenter([input.value, animationMap.getCenter().lat]);
            break;
            case "latInput":
                animationMap.setCenter([animationMap.getCenter().lng, input.value]);
            break;
            case "pitchInput":
                animationMap.setPitch(input.value);
            break;
            case "bearingInput":
                animationMap.setBearing(input.value);
            break;
            case "zoomInput":
                animationMap.setZoom(input.value);
            break;

            case "overlayCheck":
                manager.hideAllControls(input.checked);
            break;
            case "fullscreenCheck":
                if(input.checked) {
                    document.documentElement.requestFullscreen();
                } else {
                    document.exitFullscreen();
                }
            break;
            case "greenscreenCheck":
                // TODO
                if(input.checked) {
                    let layers = animationMap.getStyle().layers;
                    let firstCMCLLayer;
                    for (const layer of layers) {
                        if (layer["metadata"] && layer["metadata"]["provider"] && layer["metadata"]["provider"] === "cmcl") {
                            firstCMCLLayer = layer.id;
                            break;
                        }
                    }
                    console.log("Behind layer " + firstCMCLLayer);

                    animationMap.addLayer({
                        "id": "greenscreen",
                        "type": "background",
                        "paint": {
                            "background-color": "#FF6600"
                        }
                    }, firstCMCLLayer);
                } else {
                    animationMap.removeLayer("greenscreen");
                }
            break;
        }
    });
});
}

function addRow() {
var rowContainer = document.getElementById("animationRowContainer");
var rows = rowContainer.querySelectorAll("#animationRow");

if(rows.length >= 1) {
    var newRow = rows[rows.length - 1].cloneNode(true);
    rowContainer.appendChild(newRow);
    newRow.querySelector("#deleteRow").style.display = "flex";
    typeChange(newRow.querySelector("#animationType"));

} else {
    rowContainer.innerHTML += ROW_TEMPLATE;
    typeChange(rowContainer.querySelector("#animationType"));
}
}

function deleteRow(source) {
var row = source.parentElement;
var rowContainer = document.getElementById("animationRowContainer");
rowContainer.removeChild(row);
}

function cancelAnimation() {
manager.hideAllControls(true);
if(document.getElementById("fullscreenCheck").checked) {
    document.exitFullscreen();
}
$("#animationDialog").closest('.ui-dialog-content').dialog('close'); 
if(animationMap.getLayer("greenscreen") != null) animationMap.removeLayer("greenscreen");
}

function typeChange(select) {
var row = select.parentElement.querySelector("#innerRow");

if(select.value === "fly") {
    row.innerHTML = FLY_CONTROLS;
    row.querySelector("#lngInput").value = animationMap.getCenter().lng.toFixed(7);
    row.querySelector("#latInput").value = animationMap.getCenter().lat.toFixed(7);
    row.querySelector("#pitchInput").value = animationMap.getPitch();
    row.querySelector("#bearingInput").value = animationMap.getBearing();
    row.querySelector("#zoomInput").value = animationMap.getZoom();
    row.querySelector("#delayInput").value = 1000;
    row.querySelector("#curveInput").value = 1.42;
    row.querySelector("#speedInput").value = 1.2;
    row.querySelector("#deleteRow").style.display = "none";
} else {
    row.innerHTML = ROTATE_CONTROLS;
    row.querySelector("#lngInput").value = animationMap.getCenter().lng.toFixed(7);
    row.querySelector("#latInput").value = animationMap.getCenter().lat.toFixed(7);
    row.querySelector("#pitchInput").value = animationMap.getPitch();
    row.querySelector("#zoomInput").value = animationMap.getZoom();
    row.querySelector("#delayInput").value = 1000;
    row.querySelector("#durationInput").value = 3000;
    row.querySelector("#speedInput").value = 1.2;
    row.querySelector("#deleteRow").style.display = "none";
}
}

function startAnimation() {
animating = true;

// Hide the dialog
$("#animationDialog").closest('.ui-dialog-content').dialog('close'); 

// Get all animation steps
var rowContainer = document.getElementById("animationRowContainer");
var rows = rowContainer.querySelectorAll("#innerRow");

// Add listener for animation end
animationMap.on('moveend', processNextAnimation);

// Parse and store animation events
rows.forEach(row => {
    var delay = row.querySelector("#delayInput").value;
    var type = row.parentElement.querySelector("#animationType").value;

    var options = {
        essential: true,
        center: [
            row.querySelector("#lngInput").value,
            row.querySelector("#latInput").value
        ],
        pitch: row.querySelector("#pitchInput").value,
        zoom: row.querySelector("#zoomInput").value,
        speed: row.querySelector("#speedInput").value,
        easing(t) {
            return t;
        }
    };

    if(type === "fly") {
        options["bearing"] = row.querySelector("#bearingInput").value;
        options["curve"] = row.querySelector("#curveInput").value;
    } else {
        options["duration"] = row.querySelector("#durationInput").value;
    }

    var animation = {};
    animation["options"] = options;
    animation["delay"] = delay;
    animation["type"] = type;
    pendingAnimations.push(animation);
});

// Append an overlay div to prevent all mouse events whilst animating
var overlay = document.createElement("div");
overlay.id = "overlay";
document.body.appendChild(overlay);
overlay.click(function(e) {
    e.stopPropagation();
    e.preventDefault();
    e.stopImmediatePropagation();
    return false;
});

// Start the animation chain
processNextAnimation();
}

function processNextAnimation() {
if(!animating || rotating) return;

if(pendingAnimations.length > 0) {
    var delay = pendingAnimations[0]["delay"];
    var options = pendingAnimations[0]["options"];
    var type = pendingAnimations[0]["type"];
    pendingAnimations.shift();
    processAnimation(delay, options, type);
} else {
    animating = false;
    animationMap.off('moveend', processNextAnimation);
    if(animationMap.getLayer("greenscreen") != null) animationMap.removeLayer("greenscreen");

    document.body.removeChild(document.getElementById("overlay"));
}
}

function processAnimation(delay, options, type) {
setTimeout(function() {
    if(type === "fly") {
        animationMap.flyTo(options);
    } else {
        rotating = true;

        rotateStart = animationMap.getBearing();
        if(rotateStart < 0) rotateStart = 360 + rotateStart;

        rotateTime = performance.now();
        rotateSpeed =  options["speed"];
        rotateCamera(rotateTime);

        setTimeout(function() {
            rotating = false;
            processNextAnimation();
        }, options["duration"]);
    }
}, delay);
}

function rotateCamera(timestamp) {
if(!rotating) return;
var speed = 100 / rotateSpeed;
var degrees = ((timestamp - rotateTime) / speed) % 360;
degrees += rotateStart;

animationMap.rotateTo(degrees, { duration: 0 });
requestAnimationFrame(rotateCamera);
}