import styles from '../registry.table.module.css';
import iconStyles from 'ui/graphic/icon/icon-button.module.css';

import React from 'react';
import { useRouter } from 'next/navigation';

import MaterialIconButton from 'ui/graphic/icon/icon-button';
import { Routes } from 'io/config/routes';

interface RegistryRowActionsProps {
  recordId: string;
  recordType: string;
  lifecycleStage: string;
}

/**
 * Renders the possible row actions for each row in the registry.
 * 
 * @param {string} recordId The identifier of the record.
 * @param {string} recordType The type of the record.
 * @param {string} lifecycleStage The current stage of a contract lifecycle to display.
 */
export default function RegistryRowActions(props: Readonly<RegistryRowActionsProps>) {
  const router = useRouter();
  const handleClickEdit = (): void => {
    // Move to the edit modal page for the specific record
    router.push(`${Routes.REGISTRY_EDIT}/${props.recordType}/${props.recordId}`);
  };

  const handleClickView = (): void => {
    // Move to the view modal page for the specific record
    router.push(`${Routes.REGISTRY}/${props.recordType}/${props.recordId}`);
  };

  const handleClickDelete = (): void => {
    // Move to the delete modal page for the specific record
    router.push(`${Routes.REGISTRY_DELETE}/${props.recordType}/${props.recordId}`);
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