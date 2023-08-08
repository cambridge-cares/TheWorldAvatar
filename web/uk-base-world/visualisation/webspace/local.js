/**
 * Gets the legend content for this example visualisation. In this case it's loaded
 * from an external HTML file, but a HTML string could be dynamically generated
 * if desired.
 * 
 * @returns HTML string for legend content.
 */
function buildLegend() {
    $.get("./legend.html", function(contents) {
        manager.getPanelHandler().setLegend(contents);
    }); 
}

/**
 * Collapses a section in the legend.
 * 
 * @param {String} sectionName name of legend section div
 */
function toggleLegend(sectionName) {
    let section = document.getElementById(sectionName);

    if(section != null) {
        let icon = section.querySelector("#icon")
        let contents = section.querySelector("#contents");

        if(contents.style.display === "block") {
            contents.style.display = "none";
            icon.setAttribute("class", "fas fa-angle-down fa-lg");
        } else {
            contents.style.display = "block";
            icon.setAttribute("class", "fas fa-angle-up fa-lg");
        }
    }
}

/**
 * Adds a custom, advanced filtering control panel. 
 */
function addAdvancedFilters() {
    // Add the filter controls button
    let button = document.createElement("div");
    button.id="filterButton";
    button.classList.add("controlBlock");
    button.classList.add("expanded");
    button.innerHTML = `
        <div class="tooltip">
            <i class="fas fa-search fa-lg" style="color: white;"></i>
            <span class="tooltiptext left">
                Show/Hide Filter Tools
            </span>
        </div>
    `;
    document.body.appendChild(button);

    // Add the container for filter controls
    let searchContainer = document.createElement("div");
    searchContainer.id="filterContainer";
    searchContainer.classList.add("controlBlock");
    searchContainer.classList.add("expanded");
    document.body.appendChild(searchContainer);

    // Click functionality for filter button
    button.addEventListener("click", function() {
        let container = document.getElementById("filterContainer")
        if(container.style.display === "block") {
            container.style.display = "none";
        } else {
            container.style.display = "block";
        }
    });
}
