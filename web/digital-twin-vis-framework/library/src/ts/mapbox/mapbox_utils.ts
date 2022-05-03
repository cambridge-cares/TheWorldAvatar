/**
 * Returns true if the input feature is contained within a layer
 * created by CMCL (rather than an existing one from MapBox).
 * 
 * @param feature 
 * 
 * @returns true if from CMCL layer
 */
function isCMCLLayer(feature: Object) {
    try {
        let layer = feature["layer"]["id"];
        if(MapHandler.MAP.getLayoutProperty(layer, "visibility") === "none") return false;

        if(!layer["matadata"]) {
            return false;
        } else {
            if(!layer["metadata"]["attribution"] || layer["metadata"]["attribution"] !== "CMCL Innovations") return false;
            if(!layer["metadata"]["clickable"]) return false;
        }

        return true;

    } catch(error) {
        return false;
    }
}