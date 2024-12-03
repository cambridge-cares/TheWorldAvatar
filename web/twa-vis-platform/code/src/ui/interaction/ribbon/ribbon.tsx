"use client"

import { Divider } from '@mui/material';
import { Map } from 'mapbox-gl';
import React, { useEffect, useState } from 'react';
import { useDispatch, useSelector } from 'react-redux';

import styles from './ribbon.module.css';

import {
  getCameraPositions,
  getDefaultImageryOption,
  getImageryOptions,
  locateUser,
  resetCamera,
  set3DTerrain,
  setImagery,
  togglePlacenames
} from 'ui/map/map-helper';
import { addItem, selectItem } from 'state/context-menu-slice';
import { getScenarioName, getScenarioType } from 'state/map-feature-slice';
import { ImageryOption, MapSettings } from 'types/settings';
import { ContextItemDefinition } from 'ui/interaction/context-menu/context-item';
import IconComponent from 'ui/graphic/icon/icon';
import { closeFullscreen, openFullscreen } from 'utils/client-utils';
import RibbonComponentClick from './components/ribbon-component-click';
import RibbonComponentOptions from './components/ribbon-component-options';
import RibbonComponentToggle from './components/ribbon-component-toggle';
import { scenarioTypeIcon } from '../modal/scenario';

// Type definition for Ribbon parameters
export interface RibbonProps {
  map: Map,
  startingIndex: number,
  mapSettings: MapSettings,
  hasScenario: boolean,
  toggleScenarioSelection: React.Dispatch<React.SetStateAction<boolean>>;
}

// Definition of context menu item used to toggle map ribbon.
const ribbonContextItem: ContextItemDefinition = {
  name: "Show Controls Ribbon",
  description: "Toggle map controls ribbon.",
  toggled: true,
};

/**
 * Ribbon containing visualisation controls.
 */
export default function Ribbon(props: Readonly<RibbonProps>) {
  const cameraDefault: string = props.mapSettings.camera.default;
  const ribbonState: ContextItemDefinition = useSelector(selectItem("Show Controls Ribbon"));
  const [isRibbonToggled, setIsRibbonToggled] = useState<boolean>(ribbonState?.toggled);
  const cameraNames: string[] = getCameraPositions(props.mapSettings.camera);
  const imageryNames: string[] = getImageryOptions(props.mapSettings.imagery);
  const currentImagery: ImageryOption = getDefaultImageryOption(props.mapSettings.imagery);

  useEffect(() => {
    setIsRibbonToggled(ribbonState?.toggled);
  }, [isRibbonToggled, ribbonState?.toggled])


  const currentScenarioName = useSelector(getScenarioName);
  const currentScenarioType = useSelector(getScenarioType);

  // State for map configuration settings
  const dispatch = useDispatch();
  useEffect(() => {
    dispatch(addItem(ribbonContextItem));   // Add context menu item
  }, [])

  if (isRibbonToggled) {
    return (
      <div className={styles.ribbon}>
        <RibbonComponentClick
          key="location" id="location"
          icon="my_location"
          tooltip="Move the map to your location."
          action={() => {
            locateUser(props.map);
          }}
        />
        <RibbonComponentOptions
          key="map-style" id="map-style"
          icon="palette"
          tooltip="Change base map style"
          options={imageryNames}
          initialOption={currentImagery?.name}
          iconClickable={false}
          action={() => {
            setImagery(props.mapSettings.imagery, props.map);
          }}
        />
        <RibbonComponentOptions
          key="reset" id="reset"
          icon="reset_focus"
          tooltip="Reset camera to default position."
          options={cameraNames}
          initialOption={cameraDefault}
          action={() => {
            resetCamera(props.mapSettings.camera, props.map);
          }}
        />
        <RibbonComponentToggle
          key="placenames" id="placenames"
          icon="glyphs"
          tooltip="Show / hide place names."
          initialState={false}
          action={() => {
            togglePlacenames(props.mapSettings.imagery, props.map);
          }}
        />
        <RibbonComponentToggle
          key="terrain" id="terrain"
          icon="landscape_2"
          tooltip="Toggle 3D terrain."
          initialState={false}
          action={state => {
            set3DTerrain(state, props.map);
          }}
        />
        <RibbonComponentToggle
          key="fullscreen" id="fullscreen"
          icon="open_in_full"
          tooltip="Toggle fullscreen mode."
          initialState={false}
          action={state => {
            if (state) {
              openFullscreen();
            } else {
              closeFullscreen();
            }
          }}
        />
        <Divider orientation="vertical" flexItem />
        {currentScenarioName &&
          <RibbonComponentToggle
            key="scenario" id="scenario"
            tooltip="Select another scenario."
            initialState={false}
            action={props.toggleScenarioSelection}
          >
            <div className={styles.selectedScenarioDisplay}>
              <span className={styles.scenarioDisplayText}>{currentScenarioName}</span>
              <IconComponent icon={scenarioTypeIcon(currentScenarioType)} classes={styles.navbarScenarioIcon} />
            </div>
          </RibbonComponentToggle>
        }
      </div>)
  }
}
