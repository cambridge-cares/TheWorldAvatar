"use client";

import styles from './table.ribbon.module.css';
import fieldStyles from 'ui/interaction/form/field/field.module.css';
import actionStyles from 'ui/interaction/action/action.module.css';

import React from 'react';
import { useProtectedRole } from 'hooks/useProtectedRole';
import { useRouter } from 'next/navigation';

import { Routes } from 'io/config/routes';
import { DownloadButton } from 'ui/interaction/action/download/download';
import RedirectButton from 'ui/interaction/action/redirect/redirect-button';
import ActionButton from 'ui/interaction/action/action';

interface TableRibbonProps {
  entityType: string;
  registryAgentApi: string;
  lifecycleStage: string;
  selectedDate: string;
  isTaskPage: boolean;
  setSelectedDate: React.Dispatch<React.SetStateAction<string>>;
  setIsTaskPage: React.Dispatch<React.SetStateAction<boolean>>;
}

/**
 * Renders a ribbon for the view page
 * 
 * @param {string} entityType The type of entity.
 * @param {string} registryAgentApi The target endpoint for default registry agents.
 * @param {string} lifecycleStage The current stage of a contract lifecycle to display.
 */
export default function TableRibbon(props: Readonly<TableRibbonProps>) {
  const router = useRouter();

  const isKeycloakEnabled = process.env.KEYCLOAK === 'true';

  const authorised = useProtectedRole().authorised;

  const taskId: string = "task date";

  // Handle change event for the date input
  const handleDateChange = (event: React.ChangeEvent<HTMLInputElement>) => {
    props.setSelectedDate(event.target.value);
  };

  const openAddModal: React.MouseEventHandler<HTMLButtonElement> = () => {
    router.push(`${Routes.REGISTRY_ADD}/${props.entityType}`);
  };

  const switchTaskPage: React.MouseEventHandler<HTMLButtonElement> = () => {
    props.setIsTaskPage(!props.isTaskPage);
  };

  return (
    <div className={styles.menu}>
      <div className={styles["ribbon-button-container"]}>
        <RedirectButton
          icon="pending"
          url={`${Routes.REGISTRY_PENDING}/${props.entityType}`}
          isActive={props.lifecycleStage == Routes.REGISTRY_PENDING}
          title="Pending"
        />
        <RedirectButton
          icon="schedule"
          url={`${Routes.REGISTRY_ACTIVE}/${props.entityType}`}
          isActive={props.lifecycleStage == Routes.REGISTRY_ACTIVE}
          title="Active"
        />
        <RedirectButton
          icon="archive"
          url={`${Routes.REGISTRY_ARCHIVE}/${props.entityType}`}
          isActive={props.lifecycleStage == Routes.REGISTRY_ARCHIVE}
          title="Archive"
        />
      </div>
      <div className={styles["ribbon-button-container"]}>
        {(authorised || !isKeycloakEnabled) && props.lifecycleStage == Routes.REGISTRY_PENDING &&
          <ActionButton
            icon={"add"}
            title={"add " + props.entityType}
            onClick={openAddModal}
          />
        }
        {props.lifecycleStage == Routes.REGISTRY_ACTIVE && <ActionButton
          icon={"task"}
          title={"view tasks"}
          onClick={switchTaskPage}
          className={props.isTaskPage ? actionStyles["active"] : ""}
        />}
        <DownloadButton
          agentApi={`${props.registryAgentApi}/csv/${props.entityType}`}
        />
        {(authorised || !isKeycloakEnabled) && props.lifecycleStage == Routes.REGISTRY_ACTIVE && props.isTaskPage && <div>
          <label className={fieldStyles["form-input-label"]} htmlFor={taskId}>
            Date:
          </label>
          <input
            id={taskId}
            className={fieldStyles["dtpicker"]}
            style={{ width: "5.5rem" }}
            type={"date"}
            defaultValue={props.selectedDate}
            aria-label={taskId}
            onChange={handleDateChange}
          />
        </div>
        }
      </div>
    </div>
  );
}
