"use client";

import styles from './table.ribbon.module.css';

import { useProtectedRole } from 'hooks/useProtectedRole';
import { useRouter } from 'next/navigation';
import React from 'react';

import MaterialIconButton from 'ui/graphic/icon/icon-button';
import { DownloadButton } from 'ui/interaction/download/download';
import { sendGetRequest } from 'utils/server-actions';

interface TableRibbonProps {
  entityType: string;
  registryAgentApi: string;
  schedulerAgentApi: string;
}

/**
 * Renders a ribbon for the view page
 * 
 * @param {string} entityType The type of entity.
 * @param {string} registryAgentApi The target endpoint for default registry agents.
 * @param {string} schedulerAgentApi The target endpoint for scheduler specific functionality.
 */
export default function TableRibbon(props: Readonly<TableRibbonProps>) {
  const router = useRouter();
  const { authorised } = useProtectedRole();

  // Users can only either add or schedule at one time; schedule is expected to add new instances but with restrictions
  const buttonIcon: string = props.schedulerAgentApi ? "schedule_send" : "add";
  const buttonText: string = props.schedulerAgentApi ? "schedule today" : "add " + props.entityType;

  const openAddModal: React.MouseEventHandler<HTMLDivElement> = () => {
    router.push(`../add/${props.entityType}`);
  };

  const sendScheduleRequest: React.MouseEventHandler<HTMLDivElement> = () => {
    sendGetRequest(`${props.schedulerAgentApi}schedule`);
  };
  const buttonEvent: React.MouseEventHandler<HTMLDivElement> = props.schedulerAgentApi ? sendScheduleRequest : openAddModal;

  return (
    <div className={styles.menu}>
      <div className={styles["ribbon-button-container"]}>
        {authorised && (
          <MaterialIconButton
            iconName={buttonIcon}
            className={styles["ribbon-button"] + " " + styles["ribbon-button-layout"]}
            text={{
              styles: [styles["button-text"]],
              content: buttonText
            }}
            onClick={buttonEvent}
          />)}
        <DownloadButton
          agentApi={`${props.registryAgentApi}/csv/${props.entityType}`}
          className={styles["ribbon-button"] + " " + styles["ribbon-button-layout"]}
        />
      </div>
    </div>
  );
}
