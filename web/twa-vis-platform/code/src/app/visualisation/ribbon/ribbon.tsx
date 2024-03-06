import styles from './ribbon.module.css';
import React, { useRef } from 'react';
import { Box, Tabs, Tab } from '@mui/material';
import RibbonPanel from './ribbon-panel';

import RibbonComponentClick from './components/ribbon-component-click';
import RibbonComponentToggle from './components/ribbon-component-toggle';
import RibbonComponentCombo from './components/ribbon-component-combo';
import { set3DTerrain, resetCamera, locateUser } from 'map/mapbox/mapbox-camera-utils';
import { setImagery, togglePlacenames } from 'map/mapbox/mapbox-imagery-utils';
import { CameraPosition, ImageryOption, MapSettings } from 'types/map-settings';
import { closeFullscreen, getMapSettings, openFullscreen } from 'utils/client-utils';

// Type definition for Ribbon parameters
export type RibbonProps = {
    startingIndex: number
}

// Type definition for Ribbon state.
type RibbonState = {
    activeIndex: number,
    cameraNames: string[],
    cameraDefault: string,
    imageryNames: string[],
    imageryDefault: string
}

async function getMapSettingsObject(): Promise<MapSettings> {
    return await getMapSettings();
}

function getCameraPositions(settings: MapSettings) {
    const camera = settings.camera;
    return camera.positions.map((position: CameraPosition) => position.name);
}

function getDefaultCameraPosition(settings: MapSettings) {
    return settings.camera.default;
}

function getImageryOptions(settings: MapSettings) {
    const imagery = settings.imagery;
    return imagery.options.map((option: ImageryOption) => option.name);
}

function getDefaultImageryOption(settings: MapSettings) {
    // Return the default style name
    if(settings.imagery.default.toLowerCase() == "auto") {
        // Auto detect browser theme
        if (window?.matchMedia && window?.matchMedia('(prefers-color-scheme: dark)').matches) {
            return "3D (Night)"
        } else {
            return "3D (Day)"
        }
    } else {
        return settings.imagery.default;
    }
}

/**
 * Ribbon containing visualisation controls.
 */
export default class Ribbon extends React.Component<RibbonProps, RibbonState> {

    // Initialise a new state
    state: RibbonState = {
        activeIndex: this.props.startingIndex,
        cameraNames: [""],
        cameraDefault: "",
        imageryNames: [""],
        imageryDefault: ""
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
        const settings = await getMapSettingsObject();

        this.setState({
            cameraNames: getCameraPositions(settings),
            cameraDefault: getDefaultCameraPosition(settings),
            imageryNames: getImageryOptions(settings),
            imageryDefault: getDefaultImageryOption(settings)
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

                    {this.state.activeIndex == 0 &&
                        <RibbonPanel>
                            <RibbonComponentCombo
                                icon="/img/icons/imagery.svg"
                                text="Imagery"
                                tooltip="Change map imagery"
                                options={this.state.imageryNames}
                                initialOption={this.state.imageryDefault}
                                iconClickable={false}
                                action={() => {
                                    setImagery();
                                }}
                            />
                            <RibbonComponentCombo
                                icon="/img/icons/camera.svg"
                                text="Reset Camera"
                                tooltip="Reset camera to default position."
                                options={this.state.cameraNames}
                                initialOption={this.state.cameraDefault}
                                action={() => {
                                    resetCamera();
                                }}
                            />
                            <RibbonComponentToggle
                                icon="glyphs"
                                text="Hide Labels"
                                tooltip="Toggle display of place names."
                                initialState={false}
                                action={state => {
                                    togglePlacenames();
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
                    }

                    {this.state.activeIndex == 1 &&
                        <RibbonPanel>
                            Item Two
                        </RibbonPanel>
                    }

                    {this.state.activeIndex == 2 &&
                        <RibbonPanel>
                            <RibbonComponentClick
                                icon="my_location"
                                text="Your Location"
                                tooltip="Move the map to your location."
                                action={() => {
                                    locateUser();
                                }}
                            />
                        </RibbonPanel>
                    }
                </Box>

            </div>
        )
    }
}
