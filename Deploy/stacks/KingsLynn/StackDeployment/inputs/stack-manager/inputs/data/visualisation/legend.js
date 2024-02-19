
// Static HTML
let staticHTML = `
    <div class="legend-section">
        <div class="legend-upper">
            <h3>Measurement Stations</h3>
            <i class="fas fa-angle-up fa-lg" onclick="toggleLegend(this, 'stations-lower')"></i>
        </div>
        <div class="legend-lower" id="stations-lower">
            <h4>Data Providers:</h4>
            <div class="legend-entry">
                <img src="data/icons/metoffice.png">
                <p>Met Office Weather Stations</p>
            </div>
            <div class="legend-entry">
                <img src="data/icons/floodmonitoring.png">
                <p>Environment Agency Flood-monitoring</p>
            </div>
            <div class="legend-entry">
                <img src="data/icons/ukair.png">
                <p>UK-AIR Sensor Observation Service</p>
            </div>
            <h4>Station Types:</h4>
            <div class="legend-entry">
                <img src="data/icons/weather-obs.png">
                <p>Weather Observation</p>
            </div>
            <div class="legend-entry">
                <img src="data/icons/weather-for.png">
                <p>Weather Forecast</p>
            </div>
            <div class="legend-entry">
                <img src="data/icons/weather.png">
                <p>Weather (unspecified)</p>
            </div>        
            <br/>        
            <div class="legend-entry">
                <img src="data/icons/rainfall.png">
                <p>Rainfall</p>
            </div>
            <div class="legend-entry">
                <img src="data/icons/water-level.png">
                <p>Water Level</p>
            </div>
            <div class="legend-entry">
                <img src="data/icons/flow.png">
                <p>Flow</p>
            </div>
            <div class="legend-entry">
                <img src="data/icons/temperature.png">
                <p>Temperature</p>
            </div>
            <div class="legend-entry">
                <img src="data/icons/wind.png">
                <p>Wind</p>
            </div>        
            <br/>        
            <div class="legend-entry">
                <img src="data/icons/airquality.png">
                <p>Air Quality</p>
            </div>
        </div>
    </div>
    <br/>
    <div class="legend-section">
        <div class="legend-upper">
            <h3>Flood Alert and Warning Types</h3>
            <i class="fas fa-angle-up fa-lg" onclick="toggleLegend(this, 'warnings-lower')"></i>
        </div>
        <div class="legend-lower" id="warnings-lower">
            <br/>
            <img src="data/icons/flood_warning_types.png" height="130px">
        </div>
    </div>
    <br/>
    <div class="legend-section">
        <div class="legend-upper">
            <h3>Property Usages</h3>
            <i class="fas fa-angle-up fa-lg" onclick="toggleLegend(this, 'usages-lower')"></i>
        </div>
        <div class="legend-lower" id="usages-lower">
            <img src="data/icons/usage_colorbar.png" height="220px">
        </div>
    </div>
    <br/>
    <div class="legend-section">
        <div class="legend-upper">
            <h3>Property Market Valuation Estimates</h3>
            <i class="fas fa-angle-up fa-lg" onclick="toggleLegend(this, 'values-lower')"></i>
        </div>
        <div class="legend-lower" id="values-lower">
            <img src="data/icons/value_colorbar.png" height="400px">
        </div>
    </div>
    <br/>
    <div class="legend-section">
        <div class="legend-upper">
            <h3>Network Infrastructure</h3>
            <i class="fas fa-angle-up fa-lg" onclick="toggleLegend(this, 'network-lower')"></i>
        </div>
        <div class="legend-lower" id="network-lower">
            <h4>Water Network:</h4>
            <div class="legend-entry">
                <img src="data/icons/water-clean-legend.png"/>
                <p>Water Sites</p>
            </div>
            <div class="legend-entry">
                <img src="data/icons/water-sewage-legend.png"/>
                <p>Sewage Sites</p>
            </div>
            <div class="legend-entry">
                <img src="data/icons/water-sludge-legend.png"/>
                <p>Sludge Sites</p>
            </div>
            <h4>Power Network:</h4>
            <div class="legend-entry">
                <img src="data/icons/power-primary-legend.png"/>
                <p>Primary Substations</p>
            </div>
            <div class="legend-entry">
                <img src="data/icons/power-secondary-legend.png"/>
                <p>Secondary Substations</p>
            </div>
            <h4>Status Indicators:</h4>
            <div class="legend-entry">
                <img src="data/icons/low-primary-failure.png"/>
                <p>Direct failure</p>
            </div>
            <div class="legend-entry">
                <img src="data/icons/low-secondary-failure.png" />
                <p>Indirect failure</p>
            </div>
            <div class="legend-entry">
                <img src="data/icons/low-tertiary-failure.png" />
                <p>Secondary failure</p>
            </div>
            <div class="legend-entry">
                <img src="data/icons/connection-failed.png" />
                <p>Unsupplied connection</p>
            </div>
        </div>
    </div>
`;

function buildLegend() {
    let container = document.getElementById("sidePanelLegend");
    container.innerHTML += staticHTML;
}

function toggleLegend(control, elementName) {
    let element = document.getElementById(elementName);

    if(element.style.display !== "none") {
        element.style.display = "none";
        control.className = "fas fa-angle-down fa-lg";
    } else {
        element.style.display = "block";
        control.className = "fas fa-angle-up fa-lg";
    }
}