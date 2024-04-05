import styles from './ribbon.module.css';
import React, { useState } from 'react';
import { useDispatch, useSelector } from 'react-redux';
import { Box, Tabs, Tab } from '@mui/material';

import RibbonPanel from './ribbon-panel';
import RibbonComponentClick from './components/ribbon-component-click';
import RibbonComponentToggle from './components/ribbon-component-toggle';
import RibbonComponentCombo from './components/ribbon-component-combo';
import { addItem, selectItem } from 'state/context-menu-slice';
import { ImageryOption, MapSettings } from 'types/settings';
import { ContextItemDefinition } from 'ui/context-menu/context-item';
import { closeFullscreen, openFullscreen } from 'utils/client-utils';
import {
  getCameraPositions,
  getImageryOptions,
  getDefaultImageryOption,
  setImagery, 
  resetCamera, 
  togglePlacenames, 
  locateUser, 
  set3DTerrain
} from 'map/map-helper';

// Type definition for Ribbon parameters
export type RibbonProps = {
  startingIndex: number,
  mapSettings: MapSettings,
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
export default function Ribbon(props: RibbonProps) {
  const cameraDefault: string = props.mapSettings.camera.default;
  const [activeIndex, SetActiveIndex] = useState(props.startingIndex);
  const ribbonState: ContextItemDefinition = useSelector(selectItem("Show Controls Ribbon"));
  const cameraNames: string[] = getCameraPositions(props.mapSettings.camera);
  const imageryNames: string[] = getImageryOptions(props.mapSettings.imagery);
  const currentImagery:ImageryOption = getDefaultImageryOption(props.mapSettings.imagery);

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
  if (ribbonState?.toggled != null && ribbonState.toggled) {
    return (
      <div className={styles.ribbonContainer}>

        <Box className={styles.ribbon}>
          <Box>
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
              <Tab
                className={getRibbonTabClass(activeIndex === 1)}
                label="Filter" />
              <Tab
                className={getRibbonTabClass(activeIndex === 2)}
                label="Search" />

            </Tabs>
          </Box>

          {activeIndex == 0 &&
            <RibbonPanel>
              <RibbonComponentCombo
                icon="/images/defaults/icons/imagery.svg"
                text="Imagery"
                tooltip="Change map imagery"
                options={imageryNames}
                initialOption={currentImagery?.name}
                iconClickable={false}
                action={() => {
                  setImagery(props.mapSettings.imagery);
                }}
              />
              <RibbonComponentCombo
                icon="/images/defaults/icons/camera.svg"
                text="Reset Camera"
                tooltip="Reset camera to default position."
                options={cameraNames}
                initialOption={cameraDefault}
                action={() => {
                  resetCamera(props.mapSettings.camera);
                }}
              />
              <RibbonComponentToggle
                icon="glyphs"
                text="Hide Labels"
                tooltip="Toggle display of place names."
                initialState={false}
                action={() => {
                  togglePlacenames(props.mapSettings.imagery);
                }}
              />
              <RibbonComponentToggle
                icon="/images/defaults/icons/terrain.svg"
                text="3D Terrain"
                tooltip="Toggle 3D terrain."
                initialState={false}
                action={state => {
                  set3DTerrain(state);
                }}
              />
              <RibbonComponentToggle
                icon="/images/defaults/icons/maximise.svg"
                text="Full Screen"
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
            </RibbonPanel>
          }

          {activeIndex == 1 &&
            <RibbonPanel>
              Item Two
            </RibbonPanel>
          }

          {activeIndex == 2 &&
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
      </div>)
  }
}
