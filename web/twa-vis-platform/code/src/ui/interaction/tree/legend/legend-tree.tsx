import styles from './legend-tree.module.css';
import parentStyles from '../floating-panel.module.css';

import React, { useState } from 'react';

import { LegendSettings, LegendGroup } from 'types/settings';
import HeaderField from 'ui/text/header';
import IconComponent from 'ui/graphic/icon/icon';
import DecagonIconComponent from 'ui/graphic/icon/decagon';

// Incoming parameters for component.
interface LegendTreeProps {
  settings: LegendSettings;
}

interface LegendTreeNodeProps {
  group: LegendGroup;
  groupName: string;
}

/**
 * Displays a legend component based on the user's input for legend settings.
 */
export default function LegendTree(props: Readonly<LegendTreeProps>) {
  return (
    <div className={styles.legendContainer}>
      <h2>Legend</h2>
      {Object.entries(props.settings).map(([groupName, group]) => {
        return <LegendTreeNode key={groupName} groupName={groupName} group={group} />
      })}
    </div>
  );
}

function LegendTreeNode(props: Readonly<LegendTreeNodeProps>) {
  const [isCollapsed, setIsCollapsed] = useState<boolean>(false);
  const collapsedIcon: string = isCollapsed ? "keyboard_arrow_right" : "keyboard_arrow_down";
  const toggleExpansion = () => {
    setIsCollapsed(!isCollapsed);
  };

  return (
    <div key={props.groupName} className={styles.legendGroup}>
      <HeaderField
        name={props.groupName}
        icon={collapsedIcon}
        containerStyle={parentStyles.treeHeader}
        headerNameStyle={parentStyles.treeHeaderName}
        isLoading = {false}
        spacing="0"
        toggleExpansion={toggleExpansion}
      />
      {Object.entries(props.group).map(([item, legendSettings]) => {
        if (!isCollapsed) {
          return (
            <div key={props.groupName + item} className={styles.legendEntry}>
              {legendSettings.type === "symbol" &&
                <IconComponent
                  icon={legendSettings.icon}
                  classes={styles.legendIcon}
                />}
              {legendSettings.type === "fill" &&
                <DecagonIconComponent
                  color={legendSettings.fill}
                  classes={styles.legendIcon}
                />}
              {item}
            </div>
          )
        }
      })}
    </div>
  );
}