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
    }

    /**
     * Change the underlying MapBox style.
     * 
     * @param {String} mode {"light", "dark", "satellite", "satellite-streets"}
     */
    public static changeTerrain(mode) {
        // Find existing base layer
        let baseLayer = null;
        for(let i = 0; i < MapHandler.MAP.imageryLayers.length; i++) {

            let layer = MapHandler.MAP.imageryLayers.get(i);
            if(layer.isBaseLayer()) {
                baseLayer = layer;
                break;
            }
        }

        // Bug out if not found
        if(baseLayer === null || baseLayer === undefined) {
            console.error("Could not identify base layer!");
            return;
        }

        // Remove base layer
        MapHandler.MAP.imageryLayers.remove(baseLayer, true);
        console.log("removed?");

        // Start of URL for imagery tiles
        let tileURL = "https://api.mapbox.com/styles/v1/" + MapHandler.MAP_USER + "/";

        switch(mode.toLowerCase()) {
            default:
            case "light":
                tileURL += "cl6p66equ000314pj8v0p60ub";
            break;
            case "dark":
                tileURL += "cl6owj7v2000415q1oj9aq5zq";
            break;
            case "outdoors":
                tileURL += "cl6p69s56000t14p47gpfhg1o";
            break;
            case "satellite":
                tileURL += "cl6p6j16m000415tj1s4wtfi8";
            break;
        }

        // Finish tile URL
        tileURL += "/tiles/256/{z}/{x}/{y}?access_token=" + MapHandler.MAP_API;

        // Add our own imagery provider
        // @ts-ignore
        let imageryProvider = new Cesium.UrlTemplateImageryProvider({
            url: tileURL,
            credit: "mapbox"
        });
        MapHandler.MAP.scene.imageryLayers.addImageryProvider(imageryProvider, 0);
        MapHandler.MAP.scene.requestRender();
    }

    /**
     * Reset the camera to a default position.
     * 
     * @param {String} mode {"bird", "pitch"}
     */
    public static changeCamera(mode) {
        let mapOptions = MapHandler.MAP_OPTIONS;

        if(mode === "bird") {
            MapHandler.MAP.camera.flyTo({
                // @ts-ignore
                destination : Cesium.Cartesian3.fromDegrees(mapOptions["target"][0], mapOptions["target"][1], mapOptions["target"][2]),
                orientation: {
                    // @ts-ignore
                    heading: Cesium.Math.toRadians(0),
                    // @ts-ignore
                    pitch: Cesium.Math.toRadians(-90),
                    // @ts-ignore
                    roll: Cesium.Math.toRadians(0)
                }
            });

        } else if(mode === "pitch") {
            MapHandler.MAP.camera.flyTo({
                // @ts-ignore
                destination : Cesium.Cartesian3.fromDegrees(mapOptions["target"][0], mapOptions["target"][1], mapOptions["target"][2]),
                orientation: {
                    // @ts-ignore
                    heading: Cesium.Math.toRadians(30),
                    // @ts-ignore
                    pitch: Cesium.Math.toRadians(-45),
                    // @ts-ignore
                    roll: Cesium.Math.toRadians(0)
                }
            });
        } 
    }

    // public static addTerrainClipping() {
    //     let mapOptions = MapHandler.MAP_OPTIONS;
    //     let globe = MapHandler.MAP.scene.globe;
    //     let distance = 1000.0;

    //     // @ts-ignore
    //     const position = Cesium.Cartographic.toCartesian(
    //         // @ts-ignore
    //         new Cesium.Cartographic.fromDegrees(103.77398, 1.30411, 0)
    //     );

    //     // @ts-ignore
    //     const boundingSphere = new Cesium.BoundingSphere(position, distance);

    //     // @ts-ignore
    //     globe.clippingPlanes = new Cesium.ClippingPlaneCollection({
    //         // @ts-ignore
    //         modelMatrix: Cesium.Transforms.eastNorthUpToFixedFrame(position),
    //         unionClippingRegions: true,
    //         edgeWidth: 1.0,
    //         // @ts-ignore
    //         edgeColor: Cesium.Color.WHITE,
    //         enabled: true,
    //         planes: [
    //             // @ts-ignore
    //             new Cesium.ClippingPlane(new Cesium.Cartesian3(1.0, 0.0, 0.0), distance),
    //             // @ts-ignore
    //             new Cesium.ClippingPlane(new Cesium.Cartesian3(-1.0, 0.0, 0.0), distance),
    //             // @ts-ignore
    //             new Cesium.ClippingPlane(new Cesium.Cartesian3(0.0, 1.0, 0.0), distance),
    //             // @ts-ignore
    //             new Cesium.ClippingPlane(new Cesium.Cartesian3(0.0, -1.0, 0.0), distance)
    //         ]
    //       });
    //       globe.backFaceCulling = false;
    //       globe.showSkirts = false;
        
    //       MapHandler.MAP.camera.viewBoundingSphere(
    //         boundingSphere,
    //         // @ts-ignore
    //         new Cesium.HeadingPitchRange(0.5, -0.5, boundingSphere.radius * 5.0)
    //       );
    //       // @ts-ignore
    //       MapHandler.MAP.camera.lookAtTransform(Cesium.Matrix4.IDENTITY);

    //       // Turn off Sky Atmosphere
    //       MapHandler.MAP.scene.skyAtmosphere.show = false;
    // }

}