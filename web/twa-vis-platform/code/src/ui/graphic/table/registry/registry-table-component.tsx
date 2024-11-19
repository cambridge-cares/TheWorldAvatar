"use client"

import styles from './registry.table.module.css';

import React, { useEffect, useState } from 'react';
import { useSelector } from 'react-redux';

import { getIsOpenState } from 'state/modal-slice';
import { RegistryFieldValues } from 'types/form';
import { parseWordsForLabels } from 'utils/client-utils';
import { getLifecycleData } from 'utils/server-actions';
import LoadingSpinner from 'ui/graphic/loader/spinner';
import RegistryTable from './registry-table';
import TableRibbon from './ribbon/table-ribbon';

interface RegistryTableComponentProps {
  entityType: string;
  lifecycleStage: string;
  registryAgentApi: string;
}

/**
 * This component renders a registry table for the specified entity.
 * 
 * @param {string} entityType Type of entity for rendering.
 * @param {string} lifecycleStage The current stage of a contract lifecycle to display.
 * @param {string} registryAgentApi The target endpoint for default registry agents.
 */
export default function RegistryTableComponent(props: Readonly<RegistryTableComponentProps>) {
  const isModalOpen: boolean = useSelector(getIsOpenState);
  const [currentInstances, setCurrentInstances] = useState<RegistryFieldValues[]>([]);
  const [isLoading, setIsLoading] = useState<boolean>(true);

  // A hook that refetches all data when the dialogs are closed
  useEffect(() => {
    const fetchData = async (): Promise<void> => {
      setIsLoading(true);
      try {
        const instances: RegistryFieldValues[] = await getLifecycleData(props.registryAgentApi, props.lifecycleStage, props.entityType);
        setCurrentInstances(instances);
        setIsLoading(false);
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
          registryAgentApi={props.registryAgentApi}
          lifecycleStage={props.lifecycleStage}
        />
        <div className={styles["table-contents"]}>
          {isLoading ? <LoadingSpinner isSmall={false} /> : <RegistryTable
            recordType={props.entityType}
            instances={currentInstances}
            limit={3}
          />}
        </div>
      </div>
    </div>
  );
}