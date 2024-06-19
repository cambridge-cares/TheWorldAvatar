"use client";

import styles from './scenario.module.css';

import { Button } from "@mui/material";
import Dialog from '@mui/material/Dialog';
import ToggleButton from '@mui/material/ToggleButton';
import ToggleButtonGroup from '@mui/material/ToggleButtonGroup';
import React from 'react';
import { useDispatch, useSelector } from 'react-redux';

import { getScenarioDefinitions, setScenario } from 'state/map-feature-slice';
import { ScenarioDefinition } from 'types/scenario';
import IconComponent from 'ui/graphic/icon/icon';
import { getScenarios } from '../../../utils/getScenarios';
import { setScenarioDefinitions } from 'state/map-feature-slice';

interface ScenarioModalProperties {
  scenarioURL: string,
  scenarios: ScenarioDefinition[],
  show: boolean,
  setShowState: React.Dispatch<React.SetStateAction<boolean>>;
}

/**
 * A modal component for users to select their scenario
 * 
 * @returns JSX for landing page.
 */
export default function ScenarioModal(props: Readonly<ScenarioModalProperties>) {
  const [view, setView] = React.useState(null);
  const scenarioDefinitions = useSelector(getScenarioDefinitions);


  const scenarioUrl = JSON.parse(props.scenarioURL).resources.scenario.url;
  const dispatch = useDispatch();

  const handleChange = (event: React.MouseEvent<HTMLElement>, nextView: string) => {
    dispatch(setScenario(nextView));
    props.setShowState(false);
    setView(nextView);
  };

  const onClick = async () => {
    const data = await getScenarios(scenarioUrl)
    dispatch(setScenarioDefinitions(data)); // can't do this in getsScenarios code bc server

    console.log("Refreshed")
  };

  return (
    <Dialog
      sx={{ '& .MuiDialog-paper': { width: "80vw", maxWidth: "80vw", height: "70vh", maxHeight: "70vh" } }}
      open={props.show}
    >

      <div className={styles.header}><h1>Select a scenario:</h1>
        <Button style={{ marginLeft: 'auto' }} onClick={onClick}>Refresh</Button>
      </div>
      <ToggleButtonGroup
        value={view}
        exclusive
        orientation="vertical"
        onChange={handleChange}
      >
        {(scenarioDefinitions.length > 0 ? scenarioDefinitions : props.scenarios).map((scenario, index) => (
          <ToggleButton
            value={scenario.id}
            aria-label={scenario.name}
            color="standard"
            sx={{ textAlign: "left", textTransform: "none" }}
            key={scenario.name}
          >
            <div className={styles["option-container"]}>
              <div className={styles["icon-container"]}>
                <IconComponent icon="images/defaults/icons/about.svg" classes={styles.icon} />
              </div>
              <div className={styles.content}>
                <span className={styles.title}><b>({index + 1}) {scenario.name}</b></span>
                <span className={styles.description}>{scenario.description}</span>
              </div>
            </div>
          </ToggleButton>
        ))}
      </ToggleButtonGroup>
    </Dialog>
  )
}