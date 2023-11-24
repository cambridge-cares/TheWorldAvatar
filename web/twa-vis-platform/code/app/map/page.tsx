import Toolbar from "@/components/toolbar/toolbar";
import styles from "./map.module.css";
import UISettings from "@/utils/settings/ui-settings";

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
            <div className={styles.mapContainer}/>

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