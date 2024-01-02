import styles from "./ribbon.module.css"
import React from "react";
import { Box, Tabs, Tab } from "@mui/material";
import RibbonPanel from "./ribbon-panel";
import RibbonComponent from "./ribbon-component";

// Type definition for Ribbon parameters
export type RibbonProps = {
    startingIndex: number
}

// Type definition for Ribbon state.
type RibbonState = {
    activeIndex: number
}

/**
 * Ribbon containing visualisation controls.
 */
export default class Ribbon extends React.Component<RibbonProps, RibbonState> {

    // Initialise a new state
    state: RibbonState = {
        activeIndex: this.props.startingIndex
    }

    handleChange = (event: React.SyntheticEvent, newValue: number) => {
        this.setState({
            activeIndex: newValue
        });
    };

    getClass = (isActive: boolean) => {
        return isActive ? styles.ribbonTabActive : styles.ribbonTab
    }

    // Return renderable element
    public render() {

        return (
            <div className={styles.ribbonContainer}>

                <Box className={styles.ribbon}>
                    <Box>
                        <Tabs
                            className={styles.ribbonTabs}
                            value={this.state.activeIndex}
                            onChange={this.handleChange}
                            TabIndicatorProps={{
                                style: {
                                    background: "transparent"
                                }
                            }}>

                            <Tab
                                className={this.getClass(this.state.activeIndex === 0)}
                                label="View" />
                            <Tab
                                className={this.getClass(this.state.activeIndex === 1)}
                                label="Filter" />
                            <Tab
                                className={this.getClass(this.state.activeIndex === 2)}
                                label="Search" />

                        </Tabs>
                    </Box>

                    <RibbonPanel activeIndex={this.state.activeIndex} index={0}>
                        <RibbonComponent
                            icon="/img/icons/imagery.svg"
                            text="Imagery"
                            tooltip="Change map imagery"
                        />
                        <RibbonComponent
                            icon="/img/icons/camera.svg"
                            text="Reset Camera"
                            tooltip="Reset camera to default position."
                        />
                        <RibbonComponent
                            icon="/img/icons/terrain.svg"
                            text="3D Terrain"
                            tooltip="Toggle 3D terrain."
                        />
                        <RibbonComponent
                            icon="/img/icons/maximise.svg"
                            text="Maximise"
                            tooltip="Toggle fullscreen mode."
                        />
                    </RibbonPanel>
                    <RibbonPanel activeIndex={this.state.activeIndex} index={1}>
                        Item Two
                    </RibbonPanel>
                    <RibbonPanel activeIndex={this.state.activeIndex} index={2}>
                        Item Three
                    </RibbonPanel>
                </Box>

            </div>
        )
    }
}
