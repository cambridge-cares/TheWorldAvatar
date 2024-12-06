"use client";

import styles from './scenario.module.css';

import { Button } from "@mui/material";
import Dialog from '@mui/material/Dialog';
import React from 'react';
import { useDispatch, useSelector } from 'react-redux';

import { getScenarioDefinitions, setScenarioID, setScenarioName, setScenarioType, setScenarioDefinitions } from 'state/map-feature-slice';
import { ScenarioDefinition } from 'types/scenario';
import IconComponent from 'ui/graphic/icon/icon';
import { getScenarios } from '../../../utils/getScenarios';

interface ScenarioModalProperties {
  scenarioURL: string,
  scenarios: ScenarioDefinition[],
  show: boolean,
  setShowState: React.Dispatch<React.SetStateAction<boolean>>;
}

export function scenarioTypeIcon(scenarioType: string) {
  return scenarioType ? `/images/credo-misc/${scenarioType}.svg` : "images/defaults/icons/about.svg";
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
    const scenarioID = event.currentTarget.value;
    const selectedScenario: ScenarioDefinition = (scenarioDefinitions.length > 0 ? scenarioDefinitions : props.scenarios).find(scenario => scenario.id === scenarioID);
    dispatch(setScenarioID(scenarioID));
    dispatch(setScenarioName(selectedScenario.name))
    dispatch(setScenarioType(selectedScenario.type))
    props.setShowState(false);
  };

  const onClick = async () => {
    const data = await getScenarios(scenarioUrl)
    dispatch(setScenarioDefinitions(data)); // can't do this in getsScenarios code bc server
  };


  return (
    <Dialog
      sx={{ '& .MuiDialog-paper': { maxWidth: "80vw", maxHeight: "70vh" } }}
      open={props.show}
    >

      <div className={styles.globalContainer}>
        <div className={styles.headerContainer}>
          <div className={styles.header}>
            <h1>Select a scenario:</h1>
            <Button style={{ marginLeft: 'auto', textTransform: 'none' }} className={styles.refreshButton} onClick={onClick}>Refresh</Button>
          </div>
        </div>
        <div className={styles.contentContainer}>
          {(scenarioDefinitions.length > 0 ? scenarioDefinitions : props.scenarios).map((scenario, index) => (
            <button key={scenario.name + index} value={scenario.id} className={styles.optionContainer} onClick={handleChange}>
              <div className={styles["icon-container"]}>
                <IconComponent icon={scenarioTypeIcon(scenario.type)} classes={styles.icon} />
              </div>
              <div className={styles.content}>
                <span className={styles.title}><b>{scenario.name}</b></span>
                <span className={styles.description}>{scenario.description}</span>
              </div>
            </button>
          ))}
        </div>
      </div>

    </Dialog>
  )
}