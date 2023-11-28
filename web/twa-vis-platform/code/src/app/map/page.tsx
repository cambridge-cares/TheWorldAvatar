"use client";

import styles from "./map.module.css";
import Map from "react-map-gl";

/**
 * Dynamically load and render content from an optional metadata file
 * based on the incoming URL route.
 * 
 * @param params incoming route parameters.
 * 
 * @returns React component for display. 
 */
export default async function MapContainer() {
    return (
        <>
            <div className={styles.mapContainer}>
                <Map
                    mapboxAccessToken="pk.eyJ1IjoiY21jbGlubm92YXRpb25zIiwiYSI6ImNrbGdqa3RoNDFnanIyem1nZXR3YzVhcmwifQ.hVk983r6YYlmFE8kSMbzhA"
                    mapStyle="mapbox://styles/mapbox/streets-v11"
                />
            </div>
            
            <div className={styles.componentContainer}>

                <div className={styles.upperContainer}>

                    <div className={styles.leftUpperContainer}/>
                    <div className={styles.middleUpperContainer}/>
                    <div className={styles.rightUpperContainer}/>
                    
                </div>

                <div className={styles.lowerContainer}>
                    
                </div>
            </div> 
        </>
    )
}