import styles from "./ribbon.module.css"
import React, { useRef } from "react";
import { Box, Tabs, Tab } from "@mui/material";
import RibbonPanel from "./ribbon-panel";
import RibbonComponentClick from "./components/ribbon-component-click";
import RibbonComponentToggle from "./components/ribbon-component-toggle";
import { closeFullscreen, getMapSettings, openFullscreen } from "utils/client-utils";
import { set3DTerrain, resetCamera } from "map/mapbox/mapbox-camera-utils";
import RibbonComponentCombo from "./components/ribbon-component-combo";
import MapSettingsStore from "../../../io/config/map-settings";
import { CameraPosition } from "../../../types/map-settings";

// Type definition for Ribbon parameters
export type RibbonProps = {
    startingIndex: number
}

// Type definition for Ribbon state.
type RibbonState = {
    activeIndex: number,
    cameraNames: string[],
    cameraDefault: string
}

async function getCameraPositions() {
    const settings = await getMapSettings();
    const camera = settings.camera;
    return camera.positions.map((position: CameraPosition) => position.name);
}

async function getDefaultCameraPosition() {
    const settings = await getMapSettings();
    return settings.camera.default;
}

/**
 * Ribbon containing visualisation controls.
 */
export default class Ribbon extends React.Component<RibbonProps, RibbonState> {

    // Initialise a new state
    state: RibbonState = {
        activeIndex: this.props.startingIndex,
        cameraNames: [""],
        cameraDefault: ""
    }


    handleChange = (event: React.SyntheticEvent, newValue: number) => {
        this.setState({
            activeIndex: newValue
        });
    };

    getClass = (isActive: boolean) => {
        return isActive ? styles.ribbonTabActive : styles.ribbonTab
    }

    async componentDidMount() {
        this.setState({
            cameraNames: await getCameraPositions(),
            cameraDefault: await getDefaultCameraPosition()
        });
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
                        <RibbonComponentClick
                            icon="/img/icons/imagery.svg"
                            text="Imagery"
                            tooltip="Change map imagery"
                        />
                        <RibbonComponentCombo
                            icon="/img/icons/camera.svg"
                            text="Reset Camera"
                            tooltip="Reset camera to default position."
                            options={this.state.cameraNames}
                            initialOption={this.state.cameraDefault}
                            action={() => {
                                resetCamera();
                                console.log("CAMERA HAS BEEN RESET?");
                            }}
                        />
                        <RibbonComponentToggle
                            icon="/img/icons/terrain.svg"
                            text="3D Terrain"
                            tooltip="Toggle 3D terrain."
                            initialState={false}
                            action={state => {
                               set3DTerrain(state);
                            }}
                        />
                        <RibbonComponentToggle
                            icon="/img/icons/maximise.svg"
                            text="Full Screen"
                            tooltip="Toggle fullscreen mode."
                            initialState={false}
                            action={state => {
                                if(state) {
                                    openFullscreen();
                                } else {
                                    closeFullscreen();
                                }
                            }}
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
