export {};

import * as Cesium from "cesium";

declare global {
    var Cesium;
    var turf;
    var JsonView;
    interface Window {
        terrain: string;
        selectFeatures: Object;
        manager: Manager;
        currentFeature: Object;
    }
}
