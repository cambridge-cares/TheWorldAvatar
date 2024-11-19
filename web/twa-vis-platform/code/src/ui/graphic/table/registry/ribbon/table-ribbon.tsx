"use client";

import styles from './table.ribbon.module.css';
import fieldStyles from 'ui/interaction/form/field/field.module.css';

import React, { useState } from 'react';
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

  const scheduleId: string = "schedule date";

  // Start off with today's date
  const [selectedDate, setSelectedDate] = useState<string>(new Date().toISOString().split("T")[0]);

  // Handle change event for the date input
  const handleDateChange = (event: React.ChangeEvent<HTMLInputElement>) => {
    setSelectedDate(event.target.value);
  };

  const openAddModal: React.MouseEventHandler<HTMLButtonElement> = () => {
    router.push(`../add/${props.entityType}`);
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
        <DownloadButton
          agentApi={`${props.registryAgentApi}/csv/${props.entityType}`}
        />
        {(authorised || !isKeycloakEnabled) && props.lifecycleStage == Routes.REGISTRY_ACTIVE && <div>
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
      </div>
    </div>
  );
}
