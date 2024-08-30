"use client";

import styles from './table.ribbon.module.css';

import React from 'react';
import { usePathname, useRouter } from 'next/navigation';

import MaterialIconButton from 'ui/graphic/icon/icon-button';
import { getAfterDelimiter } from 'utils/client-utils';
import { sendGetRequest } from 'utils/server-actions';
import { DownloadButton } from 'ui/interaction/download/download';
import { PathNames } from 'io/config/routes';

interface TableRibbonProps {
  entityType: string;
  agentApi: string;
}

/**
 * Renders a ribbon for the view page
 * 
 * @param {string} entityType The type of entity.
 * @param {string} agentApi The target stack endpoint.

 */
export default function TableRibbon(props: Readonly<TableRibbonProps>) {
  const router = useRouter();
  const item: string = getAfterDelimiter(usePathname(), "/");

  const openAddModal = () => {
    router.push(`../add/${props.entityType}`);
  };

  const sendScheduleRequest = () => {
    sendGetRequest(`${props.agentApi}${PathNames.SCHEDULE_AGENT}schedule`);
  };

  return (
    <div className={styles.menu}>
      <div className={styles["ribbon-button-container"]}>
        {item == "service" ? <MaterialIconButton
          iconName={"schedule_send"}
          className={styles["ribbon-button"] + " " + styles["ribbon-button-layout"]}
          text={{
            styles: [styles["button-text"]],
            content: "schedule today"
          }}
          onClick={sendScheduleRequest}
        /> : <MaterialIconButton
          iconName={"add"}
          className={styles["ribbon-button"] + " " + styles["ribbon-button-layout"]}
          text={{
            styles: [styles["button-text"]],
            content: "add " + props.entityType
          }}
          onClick={openAddModal}
        />
        }
        <DownloadButton
          agentApi={`${props.agentApi}${PathNames.OPS_AGENT}csv/${item}`}
          className={styles["ribbon-button"] + " " + styles["ribbon-button-layout"]}
        />
      </div>
    </div>
  );
}
