"use client";

import styles from './scenario.module.css';

import { Button } from "@mui/material";
import Dialog from '@mui/material/Dialog';
import React from 'react';
import { useDispatch, useSelector } from 'react-redux';

import { getScenarioDefinitions, setScenario, setScenarioDefinitions } from 'state/map-feature-slice';
import { ScenarioDefinition } from 'types/scenario';
import IconComponent from 'ui/graphic/icon/icon';
import { getScenarios } from '../../../utils/getScenarios';

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
  const scenarioDefinitions = useSelector(getScenarioDefinitions);
  const scenarioUrl = JSON.parse(props.scenarioURL).resources.scenario.url;
  const dispatch = useDispatch();

  const handleChange = (event: React.MouseEvent<HTMLButtonElement>) => {
    dispatch(setScenario(event.currentTarget.value));
    props.setShowState(false);
  };

  const onClick = async () => {
    const data = await getScenarios(scenarioUrl)
    dispatch(setScenarioDefinitions(data)); // can't do this in getsScenarios code bc server

    console.log("Refreshed")
  };

  function scenarioTypeIcon(scenario: ScenarioDefinition) {
    return scenario.type ? `images/credo-misc/${scenario.type}.svg` : "images/defaults/icons/about.svg";
  }

  return (
    <Dialog
      sx={{ '& .MuiDialog-paper': { maxWidth: "80vw", maxHeight: "70vh" } }}
      open={props.show}
    >

      <div className={styles.globalContainer}>
        <div className={styles.header}><h1>Select a scenario:</h1>
          <Button style={{ marginLeft: 'auto', textTransform: 'none' }} className={styles.refreshButton} onClick={onClick}>Refresh</Button>
        </div>
        {(scenarioDefinitions.length > 0 ? scenarioDefinitions : props.scenarios).map((scenario, index) => (
          <button key={scenario.name + index} value={scenario.id} className={styles["option-container"]} onClick={handleChange}>
            <div className={styles["icon-container"]}>
              <IconComponent icon={scenarioTypeIcon(scenario)} classes={styles.icon} />
            </div>
            <div className={styles.content}>
              <span className={styles.title}><b>{scenario.name}</b></span>
              <span className={styles.description}>{scenario.description}</span>
            </div>
          </button>
        ))}
      </div>

    </Dialog>
  )
}