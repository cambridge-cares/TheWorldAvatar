"use client";

import styles from './table.ribbon.module.css';

import React from 'react';
import { useRouter } from 'next/navigation';

import MaterialIconButton from 'ui/graphic/icon/icon-button';

interface TableRibbonProps {
  entityType: string;
}

/**
 * Renders a ribbon for the view page
 * 
 * @param {string} entityType The type of entity. Valid inputs include bin.
 */
export default function TableRibbon(props: Readonly<TableRibbonProps>) {
  const router = useRouter();

  const openAddModal = () => {
    router.push(`../add/${props.entityType}`);
  };

  return (
    <div className={styles.menu}>
      <div className={styles["ribbon-button-container"]}>
        <MaterialIconButton
          iconName={"add"}
          className={styles["ribbon-button"] + " " + styles["ribbon-button-layout"]}
          text={{
            styles: [styles["button-text"]],
            content: "add " + props.entityType
          }}
          onClick={openAddModal}
        />
      </div>
    </div>
  );
}
