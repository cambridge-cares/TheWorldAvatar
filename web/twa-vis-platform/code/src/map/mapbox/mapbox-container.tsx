/* eslint-disable @typescript-eslint/no-explicit-any */
/* eslint-disable react/prop-types */

/**
 * Question(s) here are:
 * 
 * - How the hell can I create a map instance that can be interacted with from other classes, or other
 * UI components in a totally different place in the UI hierarchy?
 *     - Quick solution is to use the global "window" object to store the map object globally.
 * 
 * - Can I create a map instance that won't re-initialise (and re-load ALL the data) anytime something
 * else in the UI changes?
 */
"use client";

import 'mapbox-gl/dist/mapbox-gl.css';

import mapboxgl from 'mapbox-gl';
import React, { useRef, useEffect } from 'react';
import { getDefaultCameraPosition } from './mapbox-camera-utils';
import { MapSettings } from '../../types/map-settings';

// Type definition of incoming properties
interface MapProperties {
    settings: MapSettings
}

/**
 * Dynamically load and render content from an optional metadata file
 * based on the incoming URL route.
 * 
 * @param params incoming route parameters.
 * 
 * @returns React component for display. 
 */
export default function MapboxMapComponent(props: MapProperties) {
    const settings = props.settings;
    const mapContainer = useRef(null);
    const map = useRef(null);

    // Run when component loaded
    useEffect(() => {
        initialiseMap();
    }, []);

    // Initialise the map object
    const initialiseMap = async () => {
        if(map.current) return;

        mapboxgl.accessToken = settings["credentials"]["key"];

        // Get default camera position
        const defaultPosition = getDefaultCameraPosition(settings);

        map.current = new mapboxgl.Map({
            container: mapContainer.current,
            style: "mapbox://styles/mapbox/standard",
            center: defaultPosition["center"],
            zoom: defaultPosition["zoom"],
            bearing: defaultPosition["bearing"],
            pitch: defaultPosition["pitch"]
        });

        // Store map object globally.
        // Note that setting a globally accessible variable for the map probably isn't wise. However,
        // we know this shouldn't be re-initialised, and the alternative is to pass the map object into
        // dozens of other UI components as a prop. This method should also allow client-side JS scripts
        // to access the map too. Would recommend revisiting this choice later though.
        window.map = map.current;
        console.info("Initialised a new Mapbox map object.");
    }

    return (
        <div id="mapContainer" ref={mapContainer} className="mapContainer">
            {/* Map will be generated here. */}
        </div>
    );
}