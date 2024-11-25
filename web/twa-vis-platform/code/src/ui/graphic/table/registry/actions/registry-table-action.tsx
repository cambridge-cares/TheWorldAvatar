import styles from '../registry.table.module.css';
import iconStyles from 'ui/graphic/icon/icon-button.module.css';

import React from 'react';
import { useRouter } from 'next/navigation';

import MaterialIconButton from 'ui/graphic/icon/icon-button';
import { Routes } from 'io/config/routes';
import { GridRowModel } from '@mui/x-data-grid';
import { getAfterDelimiter, isValidIRI } from 'utils/client-utils';

interface RegistryRowActionsProps {
  recordType: string;
  lifecycleStage: string;
  isTaskPage: boolean;
  row: GridRowModel;
  setTaskId: React.Dispatch<React.SetStateAction<string>>;
  setTaskStatus: React.Dispatch<React.SetStateAction<string>>;
}

/**
 * Renders the possible row actions for each row in the registry.
 * 
 * @param {string} recordType The type of the record.
 * @param {string} lifecycleStage The current stage of a contract lifecycle to display.
 * @param {boolean} isTaskPage Indicator if the table is currently on the task view.
 * @param {GridRowModel} row Row values.
 * @param setTaskId A dispatch method to set task id when required.
 * @param setTaskStatus A dispatch method to set task status when required.
 */
export default function RegistryRowActions(props: Readonly<RegistryRowActionsProps>) {
  const router = useRouter();

  const recordId: string = props.row.id ?
    isValidIRI(props.row.id) ?
      getAfterDelimiter(props.row.id, "/") : props.row.id
    : props.row.iri;

  const handleClickEdit = (): void => {
    // Move to the edit modal page for the specific record
    router.push(`${Routes.REGISTRY_EDIT}/${props.recordType}/${recordId}`);
  };

  const handleClickView = (): void => {
    if (props.isTaskPage) {
      props.setTaskId(recordId);
      props.setTaskStatus(props.row.status);
    } else {
      // Move to the view modal page for the specific record
      router.push(`${Routes.REGISTRY}/${props.recordType}/${recordId}`);
    }
  };

  const handleClickDelete = (): void => {
    // Move to the delete modal page for the specific record
    router.push(`${Routes.REGISTRY_DELETE}/${props.recordType}/${recordId}`);
  };

  return (
    <div className={styles["table-icon-cell"]}>
      {/* Action buttons or icons */}
      {props.lifecycleStage == Routes.REGISTRY_PENDING && <MaterialIconButton
        iconName="edit"
        iconStyles={[iconStyles["small-icon"], styles["expand-icon"]]}
        onClick={handleClickEdit}
      />}
      <MaterialIconButton
        iconName="expand_circle_right"
        iconStyles={[iconStyles["small-icon"], styles["expand-icon"]]}
        onClick={handleClickView}
      />
      {props.lifecycleStage == Routes.REGISTRY_PENDING && <MaterialIconButton
        iconName="delete"
        iconStyles={[iconStyles["small-icon"], styles["expand-icon"]]}
        onClick={handleClickDelete}
      />}
    </div>
  );
}