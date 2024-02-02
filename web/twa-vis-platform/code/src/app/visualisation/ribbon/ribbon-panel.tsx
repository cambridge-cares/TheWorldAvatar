import styles from "./ribbon-panel.module.css"

import React from "react";

// Properties for RibbonPanel
type RibbonPanelProps = {
    children?: React.ReactNode
}

/**
 * Single content panel for the visualisation ribbon.
 */
export default class RibbonPanel extends React.Component<RibbonPanelProps> {

    // Return renderable element
    public render() {
        return (
            <div className={styles.ribbonPanel}>
                {this.props.children}
            </div>
        );
    }
}