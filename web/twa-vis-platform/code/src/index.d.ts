import { Map } from "mapbox-gl";

export {};

declare global {
    interface Window {

        // Map object from provider library
        map: Map;

        // "mapbox" or "cesium"
        type: string;
    }
}