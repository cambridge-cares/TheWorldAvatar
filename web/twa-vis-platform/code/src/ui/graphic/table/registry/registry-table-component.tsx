"use client"

import styles from './registry.table.module.css';

import React, { useEffect, useState } from 'react';
import { useSelector } from 'react-redux';

import { getIsOpenState } from 'state/modal-slice';
import { RegistryFieldValues } from 'types/form';
import { parseWordsForLabels } from 'utils/client-utils';
import { getLifecycleData, getServiceTasks } from 'utils/server-actions';
import LoadingSpinner from 'ui/graphic/loader/spinner';
import RegistryTable from './registry-table';
import TableRibbon from './ribbon/table-ribbon';
import useRefresh from 'hooks/useRefresh';
import TaskModal from 'ui/interaction/modal/task/task-modal';

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
  const [refreshFlag, triggerRefresh] = useRefresh();
  const isModalOpen: boolean = useSelector(getIsOpenState);
  const [currentInstances, setCurrentInstances] = useState<RegistryFieldValues[]>([]);
  const [taskId, setTaskId] = useState<string>(null);
  const [taskStatus, setTaskStatus] = useState<string>(null);
  const [isTaskPage, setIsTaskPage] = useState<boolean>(false);
  const [isTaskModalOpen, setIsTaskModalOpen] = useState<boolean>(false);
  const [isLoading, setIsLoading] = useState<boolean>(true);
  const [selectedDate, setSelectedDate] = useState<string>(new Date().toISOString().split("T")[0]);

  // A hook that refetches all data when the dialogs are closed
  useEffect(() => {
    const fetchData = async (): Promise<void> => {
      setIsLoading(true);
      try {
        let instances: RegistryFieldValues[] = [];
        if (isTaskPage) {
          // Create a Date object from the YYYY-MM-DD string
          const date = new Date(selectedDate);
          // Convert to Unix timestamp in seconds (divide milliseconds by 1000)
          const unixTimestamp: number = Math.floor(date.getTime() / 1000);
          instances = await getServiceTasks(props.registryAgentApi, unixTimestamp);
        } else {
          instances = await getLifecycleData(props.registryAgentApi, props.lifecycleStage, props.entityType);
        }
        setCurrentInstances(instances);
        setIsLoading(false);
      } catch (error) {
        console.error('Error fetching instances', error);
      }
    };

    if (!isModalOpen) {
      fetchData();
    }
  }, [isModalOpen, isTaskPage, selectedDate, refreshFlag]);

  useEffect(() => {
    if (taskId) {
      setIsTaskModalOpen(true);
    }
  }, [taskId])

  return (
    <div className={styles["container"]}>
      <div className={styles["contents-container"]}>
        <h1 className={styles["title"]}>{parseWordsForLabels(props.entityType)}</h1>
        <TableRibbon
          entityType={props.entityType}
          registryAgentApi={props.registryAgentApi}
          lifecycleStage={props.lifecycleStage}
          selectedDate={selectedDate}
          isTaskPage={isTaskPage}
          setSelectedDate={setSelectedDate}
          setIsTaskPage={setIsTaskPage}
          triggerRefresh={triggerRefresh}
        />
        <div className={styles["table-contents"]}>
          {refreshFlag || isLoading ? <LoadingSpinner isSmall={false} /> : <RegistryTable
            recordType={props.entityType}
            isTaskPage={isTaskPage}
            setTaskId={setTaskId}
            setTaskStatus={setTaskStatus}
            instances={currentInstances}
            limit={3}
          />}
        </div>
      </div>
      {taskId && <TaskModal
        id={taskId}
        taskStatus={taskStatus}
        date={selectedDate}
        registryAgentApi={props.registryAgentApi}
        isOpen={isTaskModalOpen}
        setIsOpen={setIsTaskModalOpen}
        setTaskId={setTaskId}
        setTaskStatus={setTaskStatus}
      />}
    </div>
  );
}