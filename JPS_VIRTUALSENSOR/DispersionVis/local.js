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