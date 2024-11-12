"use client";

import styles from './table.ribbon.module.css';
import fieldStyles from 'ui/interaction/form/field/field.module.css';

import React, { useState } from 'react';
import { useProtectedRole } from 'hooks/useProtectedRole';
import { useRouter } from 'next/navigation';

import MaterialIconButton from 'ui/graphic/icon/icon-button';
import { sendPostRequest } from 'utils/server-actions';
import { DownloadButton } from 'ui/interaction/download/download';

interface TableRibbonProps {
  entityType: string;
  registryAgentApi: string;
  schedulerAgentApi: string;
  setSubmitScheduling: React.Dispatch<React.SetStateAction<boolean>>;
}

/**
 * Renders a ribbon for the view page
 * 
 * @param {string} entityType The type of entity.
 * @param {string} registryAgentApi The target endpoint for default registry agents.
 * @param {string} schedulerAgentApi The target endpoint for scheduler specific functionality.
 * @param setSubmitScheduling Set the submit scheduling state for submitting the scheduler.
 */
export default function TableRibbon(props: Readonly<TableRibbonProps>) {
  const router = useRouter();

  const isKeycloakEnabled = process.env.KEYCLOAK === 'true';
  
  const authorised = useProtectedRole().authorised;

  const scheduleId: string = "schedule date";
  // Users can only either add or schedule at one time; schedule is expected to add new instances but with restrictions
  const buttonIcon: string = props.schedulerAgentApi ? "schedule_send" : "add";
  const buttonText: string = props.schedulerAgentApi ? "schedule" : "add " + props.entityType;

  // Start off with today's date
  const [selectedDate, setSelectedDate] = useState<string>(new Date().toISOString().split("T")[0]);

  // Handle change event for the date input
  const handleDateChange = (event: React.ChangeEvent<HTMLInputElement>) => {
    setSelectedDate(event.target.value);
  };

  const openAddModal: React.MouseEventHandler<HTMLDivElement> = () => {
    router.push(`../add/${props.entityType}`);
  };

  const sendScheduleRequest: React.MouseEventHandler<HTMLDivElement> = () => {
    const jsonBody: string = JSON.stringify({
      date: selectedDate,
    });
    sendPostRequest(`${props.schedulerAgentApi}/schedule`, jsonBody);
    props.setSubmitScheduling(true);
  };
  const buttonEvent: React.MouseEventHandler<HTMLDivElement> = props.schedulerAgentApi ? sendScheduleRequest : openAddModal;

  return (
    <div className={styles.menu}>
      <div className={styles["ribbon-button-container"]}>
        {(authorised || !isKeycloakEnabled) && props.schedulerAgentApi && <div>
          <label className={fieldStyles["form-input-label"]} htmlFor={scheduleId}>
            Date:
          </label>
          <input
            id={scheduleId}
            className={fieldStyles["dtpicker"]}
            style={{ width: "5.5rem" }}
            type={"date"}
            defaultValue={selectedDate}
            aria-label={scheduleId}
            onChange={handleDateChange}
          />
        </div>
        }
        {(authorised || !isKeycloakEnabled) &&
          <MaterialIconButton
            iconName={buttonIcon}
            className={styles["ribbon-button"] + " " + styles["ribbon-button-layout"]}
            text={{
              styles: [styles["button-text"]],
              content: buttonText
            }}
            onClick={buttonEvent}
          />
        }
        <DownloadButton
          agentApi={`${props.registryAgentApi}/csv/${props.entityType}`}
          className={styles["ribbon-button"] + " " + styles["ribbon-button-layout"]}
        />
      </div>
    </div>
  );
}
