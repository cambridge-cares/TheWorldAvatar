"use client"

import styles from './registry.table.module.css';

import React, { useEffect, useState } from 'react';
import { FieldValues } from 'react-hook-form';
import { useSelector } from 'react-redux';
import { useRouter } from 'next/navigation';

import { getIsOpenState } from 'state/modal-slice';
import { parseWordsForLabels } from 'utils/client-utils';
import { getData } from 'utils/server-actions';
import RegistryTable from './registry-table';
import TableRibbon from './table-ribbon';
import { PathNames } from 'io/config/routes';

interface RegistryTableComponentProps {
  entityType: string;
  agentApi: string;
}

/**
 * This component renders a registry table for the specified entity.
 * 
 * @param {string} entityType Type of entity for rendering.
 * @param {string} agentApi The target stack endpoint for contacting the backend agents.
 */
export default function RegistryTableComponent(props: Readonly<RegistryTableComponentProps>) {
  const router = useRouter();

  const isModalOpen: boolean = useSelector(getIsOpenState);
  const [currentInstances, setCurrentInstances] = useState<FieldValues[]>([]);

  const handleClickView = (index: number): void => {
    // Move to the view modal page for the specific entity associated with the row
    router.push(`./${props.entityType}/${currentInstances[index].id}`);
  };

  const handleClickEdit = (index: number): void => {
    // Move to the edit modal page for the specific entity associated with the row
    router.push(`../edit/${props.entityType}/${currentInstances[index].id}`);
  };

  const handleClickDelete = (index: number): void => {
    // Move to the delete modal page for the specific entity associated with the row
    router.push(`../delete/${props.entityType}/${currentInstances[index].id}`);
  };

  // A hook that refetches all data when the dialogs are closed
  useEffect(() => {
    const fetchData = async (): Promise<void> => {
      try {
        const instances: FieldValues[] = await getData(props.agentApi + PathNames.OPS_AGENT, props.entityType);
        setCurrentInstances(instances);
      } catch (error) {
        console.error('Error fetching instances', error);
      }
    };

    if (!isModalOpen) {
      fetchData();
    }
  }, [isModalOpen]);

  return (
    <div className={styles["container"]}>
      <div className={styles["contents-container"]}>
        <h1 className={styles["title"]}>{parseWordsForLabels(props.entityType)}</h1>
        <TableRibbon
          entityType={props.entityType}
        />
        <div className={styles["table-contents"]}>
          <RegistryTable
            fields={currentInstances}
            clickEventHandlers={{
              "view": handleClickView,
              "edit": handleClickEdit,
              "delete": handleClickDelete,
            }}
            limit={3}
          />
        </div>
      </div>
    </div>
  );
}