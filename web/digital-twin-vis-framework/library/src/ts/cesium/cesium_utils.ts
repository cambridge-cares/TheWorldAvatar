 /**
 * Utilities specific to MapBox implementations
 */
class CesiumUtils {
    
    /**
     * Returns the visibility state of the layer with the input ID.
     * 
     * @param layerID ID of layer
     * @returns visibility
     */
    public static isVisible(layerID: string): boolean {
        let dataSources = MapHandler_Cesium.DATA_SOURCES[layerID];
        if(dataSources === null || dataSources === undefined) return false;

        for(let i = 0; i < dataSources.length; i++) {
            let dataSource = dataSources[i];
            if(dataSource.show === false) {
                return false;
            }
        }
        return true;
    }

    /**
	 * Show or hide a single (MapBox) layer on the map.
	 * 
	 * @param {String} layerID MapBox layer name.
	 * @param {boolean} visible desired visibility.
	 */
    public static toggleLayer(layerID, visible) {
        let dataSources = MapHandler_Cesium.DATA_SOURCES[layerID];
        if(dataSources === null || dataSources === undefined) return;

        for(let i = 0; i < dataSources.length; i++) {
            let dataSource = dataSources[i];
            dataSource.show = visible;
        }
        MapHandler.MAP.scene.requestRender();

        // if(MapHandler.MAP.getLayer(layerID) === undefined) return;
        // if(layerID.endsWith("_cluster")) return;

        // MapHandler.MAP.setLayoutProperty(
        //     layerID,
        //     "visibility",
        //     (visible ? "visible" : "none")
        // );

        // // Is there a corresponding _clickable layer?
        // if(MapHandler.MAP.getLayer(layerID + "_clickable") != null) {
        //     MapHandler.MAP.setLayoutProperty(
        //         layerID + "_clickable",
        //         "visibility",
        //         (visible ? "visible" : "none")
        //     );
        // }

        // // Is there a corresponding _cluster layer?
        // if(MapHandler.MAP.getLayer(layerID + "_cluster") != null) {
        //     MapHandler.MAP.setLayoutProperty(
        //         layerID + "_cluster",
        //         "visibility",
        //         (visible ? "visible" : "none")
        //     );
        // }

        // // Is there a corresponding _arrows layer?
        // if(MapHandler.MAP.getLayer(layerID + "_arrows") != null) {
        //     MapHandler.MAP.setLayoutProperty(
        //         layerID + "_arrows",
        //         "visibility",
        //         (visible ? "visible" : "none")
        //     );
        // }

        // // Is there a corresponding -highlight layer?
        // if(MapHandler.MAP.getLayer(layerID + "-highlight") != null) {
        //     MapHandler.MAP.setLayoutProperty(
        //         layerID + "-highlight",
        //         "visibility",
        //         (visible ? "visible" : "none")
        //     );
        // }
    }

}