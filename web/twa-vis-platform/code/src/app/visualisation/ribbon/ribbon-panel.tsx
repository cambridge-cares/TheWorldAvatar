import styles from "./ribbon-panel.module.css"

import React from "react";

// Properties for RibbonPanel
type RibbonPanelProps = {
    children?: React.ReactNode,
    index: number,
    activeIndex: number
}

/**
 * Single content panel for the visualisation ribbon.
 */
export default class RibbonPanel extends React.Component<RibbonPanelProps> {

    // Return renderable element
    public render() {
        return (
            <div
                className={styles.ribbonPanel}
                style={{
                    display: (this.props.activeIndex === this.props.index) ? "block" : "none"
                }}>

                {this.props.activeIndex === this.props.index && (
                    <>
                        {this.props.children}
                    </>
                )}
            </div>
        );
    }
}