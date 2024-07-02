"use client"

import { Divider, Tab, Tabs } from '@mui/material';
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
} from 'map/map-helper';
import { addItem, selectItem } from 'state/context-menu-slice';
import { ImageryOption, MapSettings } from 'types/settings';
import { ContextItemDefinition } from 'ui/interaction/context-menu/context-item';
import { closeFullscreen, openFullscreen } from 'utils/client-utils';
import RibbonComponentClick from './components/ribbon-component-click';
import RibbonComponentOptions from './components/ribbon-component-options';
import RibbonComponentToggle from './components/ribbon-component-toggle';
import RibbonPanel from './ribbon-panel';

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
  const [activeIndex, SetActiveIndex] = useState(props.startingIndex);
  const ribbonState: ContextItemDefinition = useSelector(selectItem("Show Controls Ribbon"));
  const [isRibbonToggled, setIsRibbonToggled] = useState<boolean>(ribbonState?.toggled);
  const cameraNames: string[] = getCameraPositions(props.mapSettings.camera);
  const imageryNames: string[] = getImageryOptions(props.mapSettings.imagery);
  const currentImagery: ImageryOption = getDefaultImageryOption(props.mapSettings.imagery);

  useEffect(() => {
    setIsRibbonToggled(ribbonState?.toggled);
  }, [isRibbonToggled, ribbonState?.toggled])


  // State for map configuration settings
  const dispatch = useDispatch();
  dispatch(addItem(ribbonContextItem));   // Add context menu item

  const handleChange = (event: React.SyntheticEvent, newValue: number) => {
    SetActiveIndex(newValue);
  };

  const getRibbonTabClass = (isActive: boolean) => {
    return isActive ? styles.ribbonTabActive : styles.ribbonTab;
  }

  // Return renderable element
  if (isRibbonToggled) {
    return (
      <div className={styles.ribbonContainer}>
        <Tabs
          className={styles.ribbonTabs}
          value={activeIndex}
          onChange={handleChange}
          TabIndicatorProps={{
            style: {
              background: "transparent"
            }
          }}>

          <Tab
            className={getRibbonTabClass(activeIndex === 0)}
            label="View" />
          {/*
              <Tab
                className={getRibbonTabClass(activeIndex === 1)}
                label="Filter" />
              */}
          <Tab
            className={getRibbonTabClass(activeIndex === 1)}
            label="Search" />

        </Tabs>

        {activeIndex == 0 &&
          <RibbonPanel>
            <RibbonComponentOptions
              key="imagery" id="imagery"
              icon="palette"
              tooltip="Change map imagery"
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
              tooltip="Toggle display of place names."
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
            {props.hasScenario &&
              <RibbonComponentToggle
                key="scenario" id="scenario"
                icon="huboutlined"
                tooltip="Select another scenario."
                initialState={false}
                action={props.toggleScenarioSelection}
              />
            }
          </RibbonPanel>
        }

        {activeIndex == 1 &&
          <RibbonPanel>
            <RibbonComponentClick
              key = "location" id="location"
              icon="my_location"
              text="Your Location"
              tooltip="Move the map to your location."
              action={() => {
                locateUser(props.map);
              }}
            />
          </RibbonPanel>
        }
      </div>)
  }
}
