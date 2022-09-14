export {};

import * as Cesium from "cesium";

declare global {
    var Cesium;
    interface Window {
        terrain: string;
        selectFeatures: Object;
        manager: Manager;
        currentFeature: Object;
    }
}
