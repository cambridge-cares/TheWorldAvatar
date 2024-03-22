import styles from './analytics.module.css';
import React from 'react';

/**
 * Dynamically load and render content from an optional metadata file
 * based on the incoming URL route.
 * 
 * @param params incoming route parameters.
 * 
 * @returns React component for display. 
 */
export default async function DashContainer() {
    return (
        <div className={styles.dashContainer}>

            <h1>Grafana dashboard will be embedded here.</h1>

        </div>
    )
}