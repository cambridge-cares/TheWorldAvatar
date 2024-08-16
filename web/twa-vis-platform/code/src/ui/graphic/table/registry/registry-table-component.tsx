"use client"

import styles from './registry.table.module.css';

import React, { useEffect, useState } from 'react';
import { FieldValues } from 'react-hook-form';
import { useSelector } from 'react-redux';

import { getIsOpenState } from 'state/modal-slice';
import { parseWordsForLabels } from 'utils/client-utils';
import { getData } from 'utils/server-actions';
import RegistryTable from './registry-table';

interface RegistryTableComponentProps {
  entityType: string;
  agentApi: string;
}

/**
 * This component renders a registry table for the specified entity.
 * 
 * @param {string} entityType Type of entity for rendering.
 * @param {string} agentApi The target endpoint for contacting the backend agent.
 */
export default function RegistryTableComponent(props: Readonly<RegistryTableComponentProps>) {
  const isModalOpen: boolean = useSelector(getIsOpenState);
  const [currentInstances, setCurrentInstances] = useState<FieldValues[]>([]);

  // A hook that refetches all data when the dialogs are closed
  useEffect(() => {
    const fetchData = async (): Promise<void> => {
      try {
        const instances: FieldValues[] = await getData(props.agentApi, props.entityType);
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
        <div className={styles["table-contents"]}>
          <RegistryTable
            fields={currentInstances}
            limit={3}
          />
        </div>
      </div>
    </div>
  );
}