import styles from './info-tree.module.css';

import React from 'react';

import { MaterialIconButtonWithIndex } from 'ui/graphic/icon/icon-button';

interface InfoTabsProps {
  tabs: {
    hasAttributes: boolean;
    hasTimeSeries: boolean;
  };
  activeTab: {
    index: number;
    setActiveTab: React.Dispatch<React.SetStateAction<number>>;
  }
}

interface InfoTabProps {
  iconName: string;
  activeTab: {
    index: number; // the index assigned to this tab
    state: number; // the current state of index
    setActiveTab: React.Dispatch<React.SetStateAction<number>>;
  };
}

/**
 * This component renders individual tabs depending on their availability.
 * 
 * @param {boolean} tabs.hasAttributes Indicates if the tabs should have a panel for displaying attributes.
 * @param {boolean} tabs.hasTimeSeries Indicates if the tabs should have a panel for displaying timeseries.
 * @param {number} activeTab.index The React state storing the current active index.
 * @param {React.Dispatch<React.SetStateAction<number>>} activeTab.setActiveTab A React function to set the current active index.
 */
export default function InfoTabs(props: Readonly<InfoTabsProps>) {
  return (
    <div className={styles["tab-container"]}>
      {props.tabs?.hasAttributes && (
        <InfoTab
          iconName="listAlt"
          activeTab={{
            index: 0,
            state: props.activeTab.index,
            setActiveTab: props.activeTab.setActiveTab,
          }}
        />
      )}
      {props.tabs?.hasTimeSeries && (
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
}

/**
 * This component renders a tab for the parent component.
 * 
 * @param {string} iconName The Material UI icon name.
 * @param {number} activeTab.index The index of this tab that will stay static.
 * @param {number} activeTab.state The React state storing the current active index.
 * @param {React.Dispatch<React.SetStateAction<number>>} activeTab.setActiveTab A React function to set the current active index.
 */
function InfoTab(props: Readonly<InfoTabProps>) {
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
}