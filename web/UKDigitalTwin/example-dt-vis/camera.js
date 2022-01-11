
const ROW_TEMPLATE = `
    <div id="animationRow">
        <input type="number" id="lngInput" class="long" placeholder="Longitude">
        <input type="number" id="latInput" class="long" placeholder="Latitude">
        <input type="number" id="pitchInput" class="short" placeholder="Pitch">
        <input type="number" id="bearingInput" class="short" placeholder="Bearing">
        <input type="number" id="zoomInput" class="short" placeholder="Zoom">
        <input type="number" id="curveInput" class="short" placeholder="Curve">
        <input type="number" id="speedInput" class="short" placeholder="Speed">
        <div id="deleteRow" onclick="deleteRow(this)">
            <img src="./img/close.png"/>
        </div>
    </div>
`;

var animationMap;

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

    } else {
        rowContainer.innerHTML += ROW_TEMPLATE;
        var row = document.getElementById("animationRowContainer");

        row.querySelector("#lngInput").value = animationMap.getCenter().lng.toFixed(7);
        row.querySelector("#latInput").value = animationMap.getCenter().lat.toFixed(7);
        row.querySelector("#pitchInput").value = animationMap.getPitch();
        row.querySelector("#bearingInput").value = animationMap.getBearing();
        row.querySelector("#zoomInput").value = animationMap.getZoom();
        row.querySelector("#curveInput").value = 1.42;
        row.querySelector("#speedInput").value = 1.2;
        row.querySelector("#deleteRow").style.display = "none";
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
}

function startAnimation() {
    // Hide the dialog
    $("#animationDialog").closest('.ui-dialog-content').dialog('close'); 

    // Get all animation steps
    var rowContainer = document.getElementById("animationRowContainer");
    var rows = rowContainer.querySelectorAll("#animationRow");

    // Chain animation events
    rows.forEach(row => {
        var options = {
            essential: true,
            center: [
                row.querySelector("#lngInput").value,
                row.querySelector("#latInput").value
            ],
            pitch: row.querySelector("#pitchInput").value,
            bearing: row.querySelector("#bearingInput").value,
            zoom: row.querySelector("#zoomInput").value,
            curve: row.querySelector("#curveInput").value,
            speed: row.querySelector("#speedInput").value
        };
        console.log(JSON.stringify(options));
        animationMap.flyTo(options);
    });
}