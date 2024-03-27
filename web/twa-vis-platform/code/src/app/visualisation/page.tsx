/**
 * Question(s) here are:
 * 
 * - How the hell can I create a map instance that can be interacted with from other classes, or other
 * UI components in a totally different place in the UI hierarchy?
 * 
 * - Can I create a map instance that won't re-initialise (and re-load ALL the data) anytime something
 * else in the UI changes?
 */
"use client";

import 'mapbox-gl/dist/mapbox-gl.css';
import styles from './visualisation.module.css';

import React, { useEffect, useRef } from 'react';
import { useDispatch, useSelector } from 'react-redux';

import Ribbon from './ribbon/ribbon';
import { addItem, selectItem } from 'state/context-menu-slice';
import { ContextItemDefinition } from 'ui/context-menu/context-item';
import FloatingPanelContainer from 'ui/tree/floating-panel';
import MapboxMapComponent from 'map/mapbox/mapbox-container';
import { getAndParseDataSettings, getMapSettings } from 'utils/client-utils';


// Definition of context menu item used to toggle map ribbon.
const ribbonContextItem: ContextItemDefinition = {
    name: "Show Controls Ribbon",
    description: "Toggle map controls ribbon.",
    toggled: true
};

/**
 * Dynamically load and render content from an optional metadata file
 * based on the incoming URL route.
 * 
 * @param params incoming route parameters.
 * 
 * @returns React component for display. 
 */
export default function MapContainer() {

    // State for map configuration settings
    const dispatch = useDispatch()
    const ribbonState = useSelector(selectItem("Show Controls Ribbon"));

    const mapSettings = useRef(null);
    const dataStore = useRef(null);

    // Run when component loaded
    useEffect(() => {
        dispatch(addItem(ribbonContextItem));   // Add context menu item

        const settingsPromise = getMapSettings();
        settingsPromise.then((settingsObj) => {
            mapSettings.current = settingsObj;
            window.type = mapSettings?.current?.["type"];

            const dataPromise = getAndParseDataSettings();
            dataPromise.then((dataObj) => {
                dataStore.current = dataObj;
            });
        });
    }, []);

    return (
        <>
            {/* Mapbox map */}
            {mapSettings?.current?.["type"] === "mapbox" &&
                <MapboxMapComponent
                    settings={mapSettings.current}
                    dataStore={dataStore.current}
                />
            }

            {/* Cesium map */}
            {mapSettings?.current?.["type"] === "cesium" &&
                <div></div>
            }

            {/* Container elements */}
            <div className={styles.componentContainer}>

                {/* Map controls ribbon */}
                {ribbonState?.toggled != null && ribbonState.toggled &&
                    <Ribbon startingIndex={0} />
                }

                {/* Containers for upcoming components (layer tree, metadata, time series charts etc.) */}
                <div className={styles.upperContainer}>
                    <FloatingPanelContainer dataStore={dataStore.current} />
                </div>
                <div className={styles.lowerContainer} />
            </div>
        </>
    )
}