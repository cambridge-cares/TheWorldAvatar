PanelHandler.prototype.prepareMetaContainers = function(e, t) {
    var a = document.getElementById("metaTabs"),
        a = (null == a && this.appendContent("<div id='buttonTabs'></div> <br><div id='metaTabs'></div>"), document.getElementById("metaContainer"));
    null == a && this.appendContent("<div id='metaContainer'></div>");
    let n = document.getElementById("treeButton"),
        i = document.getElementById("timeButton");
    e && (null === n && (document.getElementById("metaTabs").innerHTML += `
                <button id="treeButton" class="tablinks" onclick="manager.openMetaTab(this.id, 'metaTreeContainer')">Metadata</button>
            `, n = document.getElementById("treeButton")), null === document.getElementById("metaTreeContainer")) && (document.getElementById("metaContainer").innerHTML += "<div id='metaTreeContainer' class='tabcontent'></div>"), t && (null === i && (document.getElementById("metaTabs").innerHTML += `
                <button id="timeButton" class="tablinks" onclick="manager.openMetaTab(this.id, 'metaTimeContainer')">Time Series</button>
            `, i = document.getElementById("timeButton")), null === document.getElementById("metaTimeContainer")) && (document.getElementById("metaContainer").innerHTML += "<div id='metaTimeContainer' style='display: none;' class='tabcontent'></div>"), null != n && (n.style.display = e ? "block" : "none"), null != i && (i.style.display = t ? "block" : "none"), e && !t ? (n.style.width = "100%", n.style.borderRadius = "10px", this.manager.openMetaTab("treeButton", "metaTreeContainer")) : !e && t ? (i.style.width = "100%", i.style.borderRadius = "10px", this.manager.openMetaTab("timeButton", "metaTimeContainer")) : e && t && (n.style.width = "50%", n.style.borderRadius = "10px 0 0 10px", this.manager.openMetaTab("treeButton", "metaTreeContainer"));
    document.getElementById("buttonTabs").innerHTML = "<button id='getClassScheduleButton'  class='custom-button2''>Class Schedule</button>";
    document.getElementById("buttonTabs").innerHTML += "<button id='getWeatherDataButton'  class='custom-button2''>Weather Data</button>";
    document.getElementById("buttonTabs").innerHTML += "<button id='getShowHeatmapButton'  class='custom-button2''>Power Heatmap</button>";
    a = document.getElementById("footerContainer");
    null !== a && (a.style.display = "none")


    document.getElementById('getClassScheduleButton').addEventListener('click', function () {
        alert('Getting class schedule data...');
        var iframe = document.createElement("iframe");
        // Set attributes for the iframe
        iframe.src = "http://localhost:3838/analytics/d-solo/a3e4e240-87cc-455e-888a-fd3af4e9e64d/ntu-dashboard?orgId=1&var-schedule=All&from=1672731037713&to=1672787129841&theme=light&panelId=1";
        //iframe.src = "http://128.199.151.149:3838/analytics/d-solo/b56f5515-ef78-4feb-b712-7e5cddb4ba25/ntu-classschedule?orgId=1&from=1672731037713&to=1672787129841&theme=light&panelId=1";
        iframe.width = "450";
        iframe.height = "800";
        iframe.frameBorder = "0"; // Note: It's frameBorder, not frameborder

        var metaTabsContainer = document.getElementById("metaTabs");
        metaTabsContainer.innerHTML = '';
        metaTabsContainer.appendChild(iframe);

        // Get the "metaContainer" div and clear its content
        var metaContainer = document.getElementById("metaContainer");
        metaContainer.innerHTML = '';
        /*
        // Get the container element
        var container = document.getElementById("metaTabs");
        // Append the iframe to the container
        container.replaceWith(iframe);
        container = document.getElementById("metaContainer");
        container.replaceWith(" ");
        */
    });

    document.getElementById('getWeatherDataButton').addEventListener('click', function () {
        alert('Getting weather data...');

        // Create first iframe and its header
        var iframe1 = document.createElement("iframe");
        var header1 = document.createElement("b");
        header1.textContent = "Weather dashboard";
        iframe1.src = "http://localhost:3838/analytics/d-solo/f9f0c568-ebe8-4971-95c0-b4d1729692a3/new-dashboard-2?orgId=1&from=1672531200000&to=1672588799000&theme=light&panelId=1";
        iframe1.width = "450";
        iframe1.height = "270";
        iframe1.frameBorder = "0";

        // Get the container and clear its content
        var container = document.getElementById("metaTabs");
        container.innerHTML = '';

        // Append the first iframe and its header to the container
        container.appendChild(header1);
        container.appendChild(iframe1);

        // Create second iframe and its header (assuming you want to append it to the same container)
        var iframe2 = document.createElement("iframe");
        var header2 = document.createElement("b");
        header2.textContent = "Weather forecast";

        iframe2.src = "http://localhost:3838/analytics/d-solo/c7376c69-1c16-4d12-89f6-450707cd8897/new-dashboard?orgId=1&from=1672531200000&to=1672588799000&theme=light&panelId=1";
        iframe2.width = "450";
        iframe2.height = "280";
        iframe2.frameBorder = "0";

        // Append the second iframe and its header to the container
        container.appendChild(header2);
        container.appendChild(iframe2);

        // Append the iframe to the container
        metaContainer = document.getElementById("metaContainer");
        metaContainer.innerHTML = '';

    });


    // for Heatmap button
    document.getElementById('getShowHeatmapButton').addEventListener('click', function () {

        console.log("viewer? ", MapHandler.MAP instanceof Cesium.Viewer);
        let bounds = {
        west: 103.6687938762,
        east: 103.6963979333,
        south: 1.3372667367,
        north: 1.3605829566,
        };
        let heatMap = CesiumHeatmap.create(
        MapHandler.MAP, // your cesium viewer
        bounds, // bounds for heatmap layer
        {
          // heatmap.js options go here
          //minOpacity: 0.2,
          maxOpacity: 0.4,
          //useEntitiesIfAvailable: false,
          radius: 200,
          blur: 0.9,
          gradient: {  // the gradient used if not given in the heatmap options object
        '.3': '#d9e7fc',
        '.65': '#2a7aed',
        '.8': '#fbd801',
        '.95': '#c91212'
        },
        },
        );
        let data = [
        { x: 103.6818572460, y: 1.3419334155, value: 100 },   // School of Physical and Mathematical Sciences
        { x: 103.6800064641, y: 1.3442181323, value: 100 },  // Nanyang Auditorium
        { x: 103.6791746564, y: 1.3450496383, value: 100 },   // School of Biological Sciences
        { x: 103.6800881613, y: 1.3454565079, value: 100 },     // Block N1.3
        { x: 103.6796595995, y: 1.3467408413, value: 100 },     // Block N2
        { x: 103.6800130775, y: 1.3473157834, value: 41 },     // Block N2.1
        { x: 103.6757490809, y: 1.3445919035, value: 75 },     // Experimental Medicine Building
        { x: 103.6854021333, y: 1.3442537009, value: 76 },     // Hall of Residence 4
        { x: 103.6883786891, y: 1.3458209704, value: 100 },    // Pioneer Hall
        { x: 103.6853405644, y: 1.3483400219, value: 80 },     // Canteen 2
        { x: 103.6877703643, y: 1.3494060864, value: 40 },      // The Wave
        { x: 103.6881162008, y: 1.3514382803, value: 21 },     // Nanyang Executive Centre
        { x: 103.6859452065, y: 1.3539692593, value: 30 },      // Hall 10
        { x: 103.6811869703, y: 1.3508321630, value: 30 }      // Hall 16
        ];
        let valueMin = 0;
        let valueMax = 50;
        heatMap.setWGS84Data(valueMin, valueMax, data);
        });
}


Manager.prototype.showFeature = function(e, t) {
    if (null == t && null != e.properties) {
        t = e.properties;
    } else if (null === t) {
        return void console.warn("Selected feature has no properties, cannot show any side panel content!");
    }
    let a = getName(t);
    if (null == a) {
        a = e.hasOwnProperty("id") && "object" != typeof e.id ? "Feature " + e.id : "NTU Digital Twin Visualisation";
    }

    //this.panelHandler.setTitle("<a href='http://128.199.151.149:3939/'><img src='twa-logo-with-title-blue-256.png'></a><br><h3>" + a + "</h3>");
    this.panelHandler.setTitle("<img src='twa-logo-with-title-blue-256.png'><br><h3>" + "KG-Driven NTU Digital Twin Visualisation" + "</h3>");
    document.getElementById("titleContainer").classList.add("clickable");

    let n = t.description;
    if (null === n && t.desc) {
        n = t.desc;
    }

    if (null !== n && void 0 !== n) {
        this.panelHandler.setContent("<div class='description'>" + n + "</div>");
    } else {
        this.panelHandler.setContent("");
    }

    this.panelHandler.addSupportingData(e, t);

    let tButton = document.getElementById("treeButton");
    let iButton = document.getElementById("timeButton");

    if (void 0 === n && null === tButton && null === iButton) {
        this.panelHandler.setContent("<div class='description'>No data is available for this location.</div>");
    }

    $("#sidePanelInner").tabs("option", "active", 0);
    document.getElementById("returnContainer").style.display = "table";

    window.currentFeature = e;
}


// Overwrite the addSupportingData function
PanelHandler.prototype.addSupportingData = function(e, a) {
// Your new implementation here
    a = filterNulls(a);
    let t = a.endpoint;
    let n = a.iri;

    this.prepareMetaContainers(true, true);
    document.getElementById("metaTreeContainer").innerHTML = "<i>Retrieving data...</i>";

    const keys = Object.keys(a);
    keys.forEach(key => {
        console.log(`Property: ${key}, Value: ${a[key]}`);
    });

    console.warn("a: ", a);
    console.warn("filtered a: ", filterNulls(a));
    console.warn(Object.keys(a)[0]);
    console.warn(a[Object.keys(a)[0]]);

    let i = this;
    let iriValue;

    if(Object.keys(a)[0] === "id"){
        console.warn("a.id: ", a.id);
        // for branch ids
        if (a.id === "12"){
            iriValue = "https://www.theworldavatar.com/kg/ontopowsys/NTU_Branch_0";
        }
        else if (a.id === "23"){
            iriValue = "https://www.theworldavatar.com/kg/ontopowsys/NTU_Branch_1";
        }
        else if (a.id === "34"){
            iriValue = "https://www.theworldavatar.com/kg/ontopowsys/NTU_Branch_2";
        }
        else if (a.id === "45"){
            iriValue = "https://www.theworldavatar.com/kg/ontopowsys/NTU_Branch_3";
        }
        else if (a.id === "29"){
            iriValue = "https://www.theworldavatar.com/kg/ontopowsys/NTU_Branch_4";
        }
        else if (a.id === "910"){
            iriValue = "https://www.theworldavatar.com/kg/ontopowsys/NTU_Branch_5";
        }
        else if (a.id === "26"){
            iriValue = "https://www.theworldavatar.com/kg/ontopowsys/NTU_Branch_6";
        }
        else if (a.id === "67"){
            iriValue = "https://www.theworldavatar.com/kg/ontopowsys/NTU_Branch_7";
        }
        else if (a.id === "68"){
            iriValue = "https://www.theworldavatar.com/kg/ontopowsys/NTU_Branch_8";
        }
        else if (a.id === "311"){
            iriValue = "https://www.theworldavatar.com/kg/ontopowsys/NTU_Branch_9";
        }
        else if (a.id === "1112"){
            iriValue = "https://www.theworldavatar.com/kg/ontopowsys/NTU_Branch_10";
        }
        else if (a.id === "1213"){
            iriValue = "https://www.theworldavatar.com/kg/ontopowsys/NTU_Branch_11";
        }
        else if (a.id === "414"){
            iriValue = "https://www.theworldavatar.com/kg/ontopowsys/NTU_Branch_12";
        }
        else if (a.id === "415"){
            iriValue = "https://www.theworldavatar.com/kg/ontopowsys/NTU_Branch_13";
        }

        // for bus ids
        else if (a.id === 120) {
            iriValue = "https://www.theworldavatar.com/kg/ontopowsys/NTU_Building_SBS";
        } else if (a.id === 40) {
            iriValue = "https://www.theworldavatar.com/kg/ontopowsys/NTU_Building_N1_3";
        } else if (a.id === 442) {
            iriValue = "https://www.theworldavatar.com/kg/ontopowsys/NTU_Building_NYA";
        } else if (a.id === 47) {
            iriValue = "https://www.theworldavatar.com/kg/ontopowsys/NTU_Building_N_2";
        } else if (a.id === 454) {
            iriValue = "https://www.theworldavatar.com/kg/ontopowsys/NTU_Building_N_2_1";
        } else if (a.id === 488) {
            iriValue = "https://www.theworldavatar.com/kg/ontopowsys/NTU_Building_SPMS";
        } else if (a.id === 17) {
            iriValue = "https://www.theworldavatar.com/kg/ontopowsys/NTU_Building_HALL_4";
        } else if (a.id === 387) {
            iriValue = "https://www.theworldavatar.com/kg/ontopowsys/NTU_Building_PIONEER_HALL";
        } else if (a.id === 277) {
            iriValue = "https://www.theworldavatar.com/kg/ontopowsys/NTU_Building_THE_WAVE";
        } else if (a.id === 190) {
            iriValue = "https://www.theworldavatar.com/kg/ontopowsys/NTU_Building_CANTEEN_2";
        } else if (a.id === 1) {
            iriValue = "https://www.theworldavatar.com/kg/ontopowsys/NTU_Building_EMB";
        } else if (a.id === 92) {
            iriValue = "https://www.theworldavatar.com/kg/ontopowsys/NTU_Building_RTP";
        } else if (a.id === 399) {
            iriValue = "https://www.theworldavatar.com/kg/ontopowsys/NTU_Building_NEC";
        }
        else {
            iriValue = a.id;
        }
    }else{
        iriValue = a.iri;
        //For showing the class schedule chart
        console.log("a.iri: ", a.iri);
        if(a.iri.includes("https://www.theworldavatar.com/kg/ontopowsys/NTU_Venue")){
                console.log("This is a venue");
                // Get the container element
                var container = document.getElementById("metaTabs");
                var iframe = document.createElement("iframe");
                var header = document.createElement("b");  // Using h2 as an example, you can adjust the heading level as needed
                header.textContent = "Class Schedule";
                // Set attributes for the iframe
                iframe.src = "http://localhost:3838/analytics/d-solo/a3e4e240-87cc-455e-888a-fd3af4e9e64d/ntu-dashboard?orgId=1&var-schedule=All&from=1672531200000&to=1672617599000&var-classroom=LT"+ a[Object.keys(a)[0]] +"&theme=light&panelId=2";
                iframe.width = "450";
                iframe.height = "100";

                iframe.frameBorder = "0"; // Note: It's frameBorder, not frameborder
                // Append the iframe to the container
                //container.appendChild(header);
                container.appendChild(iframe);

                var iframe = document.createElement("iframe");
                var header = document.createElement("b");  // Using h2 as an example, you can adjust the heading level as needed
                header.textContent = "Aircon planning";
                // Set attributes for the iframe
                iframe.src = "http://localhost:3838/analytics/d-solo/a3e4e240-87cc-455e-888a-fd3af4e9e64d/ntu-dashboard?orgId=1&from=1672531200000&to=1672617599000&var-classroom=LT"+ a[Object.keys(a)[0]] +"&theme=light&var-schedule=All&panelId=5";
                iframe.width = "450";
                iframe.height = "200";
                iframe.frameBorder = "0"; // Note: It's frameBorder, not frameborder
                // Append the iframe to the container
                //container.appendChild(header);
                container.appendChild(iframe);

                var iframe = document.createElement("iframe");
                var header = document.createElement("b");  // Using h2 as an example, you can adjust the heading level as needed
                header.textContent = "HVAC setting suggestions";
                // Set attributes for the iframe
                iframe.src = "http://localhost:3838/analytics/d-solo/a3e4e240-87cc-455e-888a-fd3af4e9e64d/ntu-dashboard?orgId=1&from=1672531200000&to=1672617599000&var-classroom=LT"+ a[Object.keys(a)[0]] +"&var-schedule=All&theme=light&panelId=4";
                iframe.width = "450";
                iframe.height = "200";
                iframe.frameBorder = "0"; // Note: It's frameBorder, not frameborder
                // Append the iframe to the container
                //container.appendChild(header);
                container.appendChild(iframe);
        }
        else if(a.iri.includes("https://www.theworldavatar.com/kg/ontopowsys/NTU_WaterStation")){
            console.log("This is a water station");
            // Get the container element
            var container = document.getElementById("metaTabs");

                var iframe = document.createElement("iframe");
                var header = document.createElement("b");  // Using h2 as an example, you can adjust the heading level as needed
                header.textContent = "Battery Sensor";
                // Set attributes for the iframe
                iframe.src = "http://localhost:3838/analytics/d-solo/d743aeba-8066-4f51-9cf3-c04b46049709/ntu-water-stations?orgId=1&from=1692748800000&to=1692835199000&var-station=" + a.station_id + "&theme=light&panelId=3";
                iframe.width = "130";
                iframe.height = "300";
                iframe.frameBorder = "0"; // Note: It's frameBorder, not frameborder
                // Append the iframe to the container
                container.appendChild(iframe);

                var iframe = document.createElement("iframe");
                var header = document.createElement("b");  // Using h2 as an example, you can adjust the heading level as needed
                header.textContent = "Water Station";
                // Set attributes for the iframe
                iframe.src = "http://localhost:3838/analytics/d-solo/d743aeba-8066-4f51-9cf3-c04b46049709/ntu-water-stations?orgId=1&from=1692748800000&to=1692835199000&var-station=" + a.station_id + "&theme=light&panelId=2";
                iframe.width = "300";
                iframe.height = "300";
                iframe.frameBorder = "0"; // Note: It's frameBorder, not frameborder
                // Append the iframe to the container
                container.appendChild(iframe);

                var iframe = document.createElement("iframe");
                var header = document.createElement("b");  // Using h2 as an example, you can adjust the heading level as needed
                header.textContent = "Water Station";
                // Set attributes for the iframe
                iframe.src = "http://localhost:3838/analytics/d-solo/d743aeba-8066-4f51-9cf3-c04b46049709/ntu-water-stations?orgId=1&from=1692748800000&to=1692835199000&var-station=" + a.station_id + "&theme=light&panelId=5";
                iframe.width = "450";
                iframe.height = "200";
                iframe.frameBorder = "0"; // Note: It's frameBorder, not frameborder
                // Append the iframe to the container
                container.appendChild(iframe);

                var iframe = document.createElement("iframe");
                var header = document.createElement("b");  // Using h2 as an example, you can adjust the heading level as needed
                header.textContent = "Water Station";
                // Set attributes for the iframe
                iframe.src = "http://localhost:3838/analytics/d-solo/d743aeba-8066-4f51-9cf3-c04b46049709/ntu-water-stations?orgId=1&from=1692748800000&to=1692835199000&var-station=" + a.station_id + "&theme=light&panelId=1";
                iframe.width = "450";
                iframe.height = "200";
                iframe.frameBorder = "0"; // Note: It's frameBorder, not frameborder
                // Append the iframe to the container
                container.appendChild(iframe);

                var iframe = document.createElement("iframe");
                var header = document.createElement("b");  // Using h2 as an example, you can adjust the heading level as needed
                header.textContent = "Water Station";
                // Set attributes for the iframe
                iframe.src = "http://localhost:3838/analytics/d-solo/d743aeba-8066-4f51-9cf3-c04b46049709/ntu-water-stations?orgId=1&from=1692748800000&to=1692835199000&var-station=" + a.station_id + "&theme=light&panelId=4";
                iframe.width = "450";
                iframe.height = "200";
                iframe.frameBorder = "0"; // Note: It's frameBorder, not frameborder
                // Append the iframe to the container
                container.appendChild(iframe);

            // Select the elements by their IDs
            var treeButton = document.getElementById("treeButton");
            var timeButton = document.getElementById("timeButton");

            // Hide the elements by setting their display style to 'none'
            treeButton.style.display = 'none';
            timeButton.style.display = 'none';

        }
    }

    console.warn("sent request: http://localhost:3838/feature-info-agent/get", iriValue);

    return $.getJSON("http://localhost:3838/feature-info-agent/get", {
    //return $.getJSON("http://128.199.151.149:3838/feature-info-agent/get", {
        iri: iriValue
    }, function(e) {
        let t;
        if (e === null || Array.isArray(e) && e.length === 0 || Object.keys(e).length === 0) {
            i.showBuiltInData(a);
        } else {
            t = e.meta;
            e = e.time;
            if (t !== null) {
                console.log("Got a meta object!");
            }
            if (e !== null) {
                console.log("Got a time object!");
            }

            document.getElementById("metaTreeContainer").innerHTML = "";
            if (t !== null) {
                t = JSONFormatter.formatJSON(t);
                if (document.getElementById("metaTreeContainer") === null) {
                    console.log("TREE CONTAINER IS NULL, WHAT?!");
                }

                if (Array.isArray(t) && t.length === 0 || typeof t === 'string' && t === '') {
                    this.showBuiltInData(a);
                } else {
                    t = JsonView.renderJSON(t, document.getElementById("metaTreeContainer"));
                    JsonView.expandChildren(t);
                    JsonView.selectiveCollapse(t);
                }
            } else {
                i.showBuiltInData(a);
            }

            document.getElementById("metaTimeContainer").innerHTML = "";
            if (e !== null) {
                i.timeseriesHandler.parseData(e);
                i.timeseriesHandler.showData("metaTimeContainer");
                document.getElementById("time-series-select").onchange(null);
            } else {
1            }
            i.prepareMetaContainers(true, e !== null);


        }
    }).fail(function() {
        console.warn("Could not get valid response from the agent, will show any in-model content instead...");
        i.showBuiltInData(a);
    });
};

MapHandler_Cesium.prototype.addLayer = function(e) {
console.log("running addLayer function")
var t = e.source.definition,
    a = t.uri;
switch (null == a && console.error("Cannot plot a data source that has no 'uri' parameter"), t.type.toLowerCase()) {
    case "wms":
    case "geoserver":
        this.addWMS(t, e);
        break;
    case "kml":
        this.addKMLFile(t, e);
        break;
    case "glb":
    case "gltf":
        this.addGLTFFile(t, e);
        break;
    case "tile":
    case "tiles":
        this.addTileset(t, e).then(function() {
            const academicBuildings = [441, 55, 434, 37, 438, 46, 232, 306, 443, 424, 415, 21, 329, 303, 302, 409, 218, 404, 51, 349, 314, 142, 166, 432, 429, 444, 156, 305, 430, 420, 411, 427, 437, 428, 52, 402, 101, 436, 318, 450, 179, 127, 83, 448, 304, 311, 414, 400, 97, 38, 483, 375, 439, 114, 435, 412, 343, 363, 384, 103, 239, 248, 370, 245, 237, 254, 246, 364, 247, 244, 243, 203, 235, 61, 120, 40, 47, 488, 59, 449, 1, 50, 382, 50, 41];
            const undergradHalls = [17, 401, 286, 58, 291, 292, 294, 95, 281, 296, 276, 406, 372, 272, 270, 342, 90, 265, 392, 152, 75, 362, 5, 3, 317, 295, 397, 178, 283, 187, 11, 126, 7, 128, 319, 53, 6, 242, 290, 22, 313, 13, 322, 301, 337, 345, 190, 15, 323, 325, 110, 354, 484, 282, 387, 327, 88, 186, 396, 212, 44, 348, 45, 347, 162, 330, 271, 280, 386, 299, 268, 30, 27, 379, 183, 352, 274, 398, 293, 355, 278, 309, 312, 324, 350, 445, 425, 332, 300, 344,334, 367, 316, 39, 481, 57, 395, 234, 431, 423, 338, 480, 479, 328, 335, 321, 91, 339, 393, 341, 189, 145, 394, 320, 48, 16, 14, 9, 8, 310, 28, 389, 10, 78, 23, 24, 26, 12, 29, 20, 308, 297, 356, 298, 369];
            const staffHalls = [273, 275, 336, 285, 289, 79, 315, 25, 326, 2, 353, 221, 351, 188, 287,284, 86, 288, 391, 346, 191, 368, 385, 18]
            const graduateHalls = [69, 397, 295, 317, 3, 5]
            const admin = [429, 314, 142, 166, 432, 178, 398, 352, 183, 454]
            const multipurpose = [399, 442, 333, 277, 360]

            let primitives = MapHandler.MAP.scene.primitives;
            console.log("adding color for primitives:" + primitives);
            // Loop through each primitive
            for (let i = 0; i < primitives.length; i++) {
                let primitive = primitives.get(i);
                // Check if the primitive is a 3D tileset
                if (primitive instanceof Cesium.Cesium3DTileset) {
                    primitive.tileVisible.addEventListener(function (tile) {
                        let content = tile.content;
                        let featuresLength = content.featuresLength;
                        for (let j = 0; j < featuresLength; j++) {
                            //console.log("adding color for each building")
                            let feature = content.getFeature(j);
                            let featureId = feature.getProperty('id'); // Replace 'id' with the correct property name for the ID
                            if (academicBuildings.includes(featureId)) {
                                feature.color = new Cesium.Color(76/255, 175/255, 80/255, 1.0);
                            }
                            if (undergradHalls.includes(featureId)) {
                                feature.color = new Cesium.Color(255/255, 183/255, 77/255, 1.0);
                            }
                            if (staffHalls.includes(featureId)) {
                                feature.color = new Cesium.Color(121/255, 85/255, 72/255, 1.0);
                            }
                            if (graduateHalls.includes(featureId)) {
                                feature.color = new Cesium.Color(240/255, 98/255, 146/255, 1.0);
                            }
                            if (admin.includes(featureId)) {
                                feature.color = new Cesium.Color(158/255, 158/255, 158/255, 1.0);
                            }
                            if (multipurpose.includes(featureId)) {
                                feature.color = new Cesium.Color(103/255, 58/255, 183/255, 1.0);
                            }
                        }
                    });
                }
            }
        });
        break;
    default:
        console.warn("Unknown type '" + t.type + "', skipping this data source.")
}

let coordinates = []; // Array to hold coordinates for the tube

function createRedPointWithLabel(name, longitude, latitude) {
const entity = MapHandler.MAP.entities.add({
  name: name,
  position: Cesium.Cartesian3.fromDegrees(longitude, latitude),
  point: {
    pixelSize: 5,
    color: Cesium.Color.RED,
    outlineColor: Cesium.Color.WHITE,
    outlineWidth: 2,
    disableDepthTestDistance: Number.POSITIVE_INFINITY, // Always render on top
  },
  label: {
    text: name,
    font: "14pt monospace",
    style: Cesium.LabelStyle.FILL_AND_OUTLINE,
    outlineWidth: 2,
    verticalOrigin: Cesium.VerticalOrigin.BOTTOM,
    pixelOffset: new Cesium.Cartesian2(0, -9),
    disableDepthTestDistance: Number.POSITIVE_INFINITY, // Always render on top
  },
});

// Add the coordinates to the array
coordinates.push(longitude);
coordinates.push(latitude);

return entity;
}

function createWaterStation(name, longitude, latitude) {
    var scene = MapHandler.MAP.scene;
    var billboards = new Cesium.BillboardCollection();

    var billboard = billboards.add({
        position: Cesium.Cartesian3.fromDegrees(longitude, latitude, 0), // Make sure to set the proper altitude if needed
        image: "waterstation_icon.png", // URL of the icon image
        width: 50, // these sizes are in pixels
        height: 50,
        id: name, // Assign a unique identifier
        heightReference: Cesium.HeightReference.RELATIVE_TO_GROUND, // Ensure it's above ground
        disableDepthTestDistance: Number.POSITIVE_INFINITY // Disable depth testing
    });

    // Add billboards to the scene
    scene.primitives.add(billboards);

    // For labels, you can still use entities or create a LabelCollection
    var labelEntity = MapHandler.MAP.entities.add({
        position: Cesium.Cartesian3.fromDegrees(longitude, latitude),
        label: {
            text: name,
            font: "14pt monospace",
            style: Cesium.LabelStyle.FILL_AND_OUTLINE,
            outlineWidth: 2,
            verticalOrigin: Cesium.VerticalOrigin.BOTTOM,
            pixelOffset: new Cesium.Cartesian2(0, -20), // Adjust for proper positioning
            disableDepthTestDistance: Number.POSITIVE_INFINITY,
        },
    });

    // Add click event listener to the billboard
    var handler = new Cesium.ScreenSpaceEventHandler(scene.canvas);
    handler.setInputAction(function(click) {
        var pickedObject = scene.pick(click.position);
        if (Cesium.defined(pickedObject) && pickedObject.primitive === billboard) {
            console.log('Water station clicked'); // This should now appear in the console
            // console.log('e is: ', manager_instance);
            // Here, you might want to construct a feature-like object that
            // represents your water station, or pass relevant data to showFeature
            var featureData = {
                // id: 277,
                iri: 'https://www.theworldavatar.com/kg/ontopowsys/NTU_WaterStation',
                station_id: Number(name.slice(-2))
            };
            manager.showFeature(e, featureData);
        }
    }, Cesium.ScreenSpaceEventType.LEFT_CLICK);

    return { billboard: billboard, label: labelEntity, handler: handler };

}

function computeCircle(radius) {
    const positions = [];
    for (let i = 0; i < 360; i++) {
    const radians = Cesium.Math.toRadians(i);
    positions.push(
        new Cesium.Cartesian2(
            radius * Math.cos(radians),
            radius * Math.sin(radians)
        )
    );
}
return positions;
}

// Create points with labels for building names
const SPMS = createRedPointWithLabel("SPMS", 103.6818313449, 1.3418622912);
const Gaia = createRedPointWithLabel("Gaia", 103.6832968323, 1.3420784334);
const EEE = createRedPointWithLabel("EEE", 103.6808374316, 1.3432265836);
const NYA = createRedPointWithLabel("NYA", 103.6802170967, 1.3444732784);
const SBS = createRedPointWithLabel("SBS", 103.6790837070, 1.3446677129);
const RSIS = createRedPointWithLabel("RSIS", 103.6828289455, 1.3445778104);
const SOH = createRedPointWithLabel("SOH", 103.6832219492, 1.3436959528);
const WKWSCI = createRedPointWithLabel("WKWSCI", 103.6798157027, 1.3422039360);
const SCSE = createRedPointWithLabel("SCSE", 103.6820219077, 1.3462758314);
const CCEB = createRedPointWithLabel("CCEB", 103.6814839745, 1.3423278539);
const ADM = createRedPointWithLabel("ADM", 103.6836713722, 1.3494452508);
const MSE = createRedPointWithLabel("MSE", 103.6828206330, 1.3466488196);
const MAE = createRedPointWithLabel("MAE", 103.6815714070, 1.3471301712);
const SRC = createRedPointWithLabel("SRC", 103.6877626788, 1.3493380482);
const Hall4 = createRedPointWithLabel("Hall4", 103.6859727443, 1.3443150373);
const Hall5 = createRedPointWithLabel("Hall5", 103.6871059725, 1.3443975857);
const Hall1 = createRedPointWithLabel("Hall1", 103.6874975524, 1.3454683228);
const CrescentHall = createRedPointWithLabel("Crescent Hall", 103.6882582783, 1.3461486472);
const Hall16 = createRedPointWithLabel("Hall16", 103.6811622680, 1.3503034094);
const Hall3 = createRedPointWithLabel("Hall3", 103.6820282620, 1.3506749592);
const Hall12 = createRedPointWithLabel("Hall12", 103.6807031375, 1.3511813697);
const Hall13 = createRedPointWithLabel("Hall13", 103.6813738353, 1.3517889450);
const Hall14 = createRedPointWithLabel("Hall14", 103.6820429886, 1.3523370583);
const NIE = createRedPointWithLabel("NIE", 103.6783990817, 1.3488637461);
const ASE = createRedPointWithLabel("ASE", 103.6793951143, 1.3467359942);
const CEE = createRedPointWithLabel("CEE", 103.6801307860, 1.3458486846);
const GH2 = createRedPointWithLabel("GH2", 103.6859232949, 1.3556889014);
const Hall11 = createRedPointWithLabel("Hall11", 103.6864649318, 1.3547248513);
const Hall10 = createRedPointWithLabel("Hall10", 103.6856292927, 1.3540245185);
const Hall9 = createRedPointWithLabel("Hall9", 103.6854104020, 1.3525671634);
const Hall8 = createRedPointWithLabel("Hall8", 103.6851604620, 1.3509547080);
const NYV = createRedPointWithLabel("Nanyang View", 103.6874021080, 1.3523853151);
const NYM = createRedPointWithLabel("Nanyang Meadows", 103.6891537361, 1.3513951034);
const NEC = createRedPointWithLabel("NEC", 103.6879722516, 1.3512641616);
const NYH = createRedPointWithLabel("NYH", 103.6867896898, 1.3498278509);
const Hall6 = createRedPointWithLabel("Hall6", 103.6870895070, 1.3478470168);
const Hall2 = createRedPointWithLabel("Hall2", 103.6861317072, 1.3478659302);
const TLSK = createRedPointWithLabel("Tan Lark Sye Walk", 103.6840865406, 1.3460192930);
const SSC = createRedPointWithLabel("SSC", 103.6833743723, 1.3454324190);
const UHSB = createRedPointWithLabel("UHSB", 103.6825848272, 1.3454364470);

// Create water stations
const waterStation14 = createWaterStation("Water Station 14", 103.688964, 1.355134);
const waterStation13 = createWaterStation("Water Station 13", 103.687987, 1.355906);
const waterStation17 = createWaterStation("Water Station 17", 103.690186, 1.350232);
const waterStation16 = createWaterStation("Water Station 16", 103.681179, 1.353062);
const waterStation11 = createWaterStation("Water Station 11", 103.680077, 1.348109);
const waterStation15 = createWaterStation("Water Station 15", 103.687457, 1.346455);
const waterStation12 = createWaterStation("Water Station 12", 103.684028, 1.342373);

}