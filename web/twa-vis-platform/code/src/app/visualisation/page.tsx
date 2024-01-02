"use client";

import 'mapbox-gl/dist/mapbox-gl.css';
import styles from "./visualisation.module.css";

import React from 'react';
import Map from "react-map-gl";
import { useEffect } from 'react';

import Ribbon from './ribbon/ribbon';
import { useDispatch, useSelector } from 'react-redux';
import { addItem, selectItem } from '../../state/context-menu-slice';
import { ContextItemDefinition } from '../../ui/context-menu/context-item';

// Definition of context menu item used to toggle map ribbon.
const ribbonContextItem: ContextItemDefinition = {
    name: "Show Controls Ribbon",
    description: "Toggle map controls ribbon.",
    toggled: true
};

/**
 * Queries the server to get the map configuration
 */
// function getMapStart() {

// }



/**a=
 * Dynamically load and render content from an optional metadata file
 * based on the incoming URL route.
 * 
 * @param params incoming route parameters.
 * 
 * @returns React component for display. 
 */
export default function MapContainer() {

    const dispatch = useDispatch()
    const ribbonState = useSelector(selectItem("Show Controls Ribbon"));

    // Add ribbon context item when loaded
    useEffect(() => {
        // Add context menu item
        dispatch(addItem(ribbonContextItem));
    });

    return (
        <>
            <div id="mapContainer" className={styles.mapContainer}>
                <Map
                    mapboxAccessToken="pk.eyJ1IjoiY21jbGlubm92YXRpb25zIiwiYSI6ImNrbGdqa3RoNDFnanIyem1nZXR3YzVhcmwifQ.hVk983r6YYlmFE8kSMbzhA"
                    mapStyle="mapbox://styles/mapbox/streets-v11"
                />
            </div>
            
            <div className={styles.componentContainer}>

                {ribbonState?.toggled != null && ribbonState.toggled &&
                    <Ribbon startingIndex={0}/>
                }

                <div className={styles.upperContainer}>
                    <div className={styles.leftUpperContainer}/>
                    <div className={styles.middleUpperContainer}/>
                    <div className={styles.rightUpperContainer}/>
                </div>

                <div className={styles.lowerContainer}/>
            </div>
        </>
    )
}