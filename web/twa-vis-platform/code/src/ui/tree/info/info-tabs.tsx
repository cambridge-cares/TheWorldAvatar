import styles from './info-tree.module.css';

import React from 'react';

import { JsonObject } from "types/json";
import { MaterialIconButtonWithIndex } from 'ui/buttons/icon-button';

type InfoTabsProps = {
  data: JsonObject;
  activeTab: {
    index: number;
    setActiveTab: React.Dispatch<React.SetStateAction<number>>;
  }
};

type InfoTabProps = {
  iconName: string;
  activeTab: {
    index: number; // the index assigned to this tab
    state: number; // the current state of index
    setActiveTab: React.Dispatch<React.SetStateAction<number>>;
  };
};

/**
 * This component renders individual tabs depending on their availability.
 * 
 * @param {JsonObject} data The queried data that will be processed for display.
 * @param {number} activeTab.index The React state storing the current active index.
 * @param {React.Dispatch<React.SetStateAction<number>>} activeTab.setActiveTab A React function to set the current active index.
 */
export default function InfoTabs(props: InfoTabsProps) {
  return (
    <div className={styles["tab-container"]}>
      {props.data?.meta && (
        <InfoTab
          iconName="listAlt"
          activeTab={{
            index: 0,
            state: props.activeTab.index,
            setActiveTab: props.activeTab.setActiveTab
          }}
        />
      )}
      {props.data?.time && (
        <InfoTab
          iconName="timeline"
          activeTab={{
            index: 1,
            state: props.activeTab.index,
            setActiveTab: props.activeTab.setActiveTab,
          }}
        />
      )}
    </div>
  );
};

/**
 * This component renders a tab for the parent component.
 * 
 * @param {string} iconName The Material UI icon name.
 * @param {number} activeTab.index The index of this tab that will stay static.
 * @param {number} activeTab.state The React state storing the current active index.
 * @param {React.Dispatch<React.SetStateAction<number>>} activeTab.setActiveTab A React function to set the current active index.
 */
function InfoTab(props: InfoTabProps) {
  return (
    <MaterialIconButtonWithIndex
      index={props.activeTab.index}
      iconName={props.iconName}
      iconStyles={[styles["tab-icon"]]}
      onButtonClick={props.activeTab.setActiveTab}
      className={
        props.activeTab.state === props.activeTab.index
          ? `${styles.active} ${styles.tab}`
          : styles.tab
      }
    />
  );
};