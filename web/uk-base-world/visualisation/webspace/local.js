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