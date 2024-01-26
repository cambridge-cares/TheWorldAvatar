import { Map } from "mapbox-gl";

export {};

declare global {
    interface Window {
        /**
         * Map object from the map provider library (Mapbox or Cesium).
         * This global object allows for easy access to the map instance throughout the application.
         */
        map: Map;

        /**
         * The type of map being used, either "mapbox" or "cesium".
         * This property helps in making decisions based on the map provider being used.
         */
        type: 'mapbox' | 'cesium';
    }
}