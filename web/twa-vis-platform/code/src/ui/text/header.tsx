import styles from './header.module.css';
import iconStyles from 'ui/graphic/icon/icon-button.module.css';

import React from 'react';

import MaterialIconButton from 'ui/graphic/icon/icon-button';

type HeaderFieldProps = {
  readonly name: string;
  readonly spacing?: string;
  readonly isCollapsed: boolean;
  readonly toggleExpansion: () => void;
};

/**
 * This component renders a dropdown field populated with the options from the data.
 * 
 * @param {DropdownFieldOption[]} options The list of options to render.
 * @param {number} selectedIndex The currently selected index.
 * @param {React.Dispatch<React.SetStateAction<number>>} setSelectedIndex The method to update the selected index based on the event.
 */
export default function HeaderField(props: HeaderFieldProps) {
  const collapsedIcon: string = props.isCollapsed ? "keyboard_arrow_down" : "keyboard_arrow_up";

  return (
    <div style={{ paddingLeft: props.spacing }} className={styles.header} onClick={props.toggleExpansion}>
      {/* Header Name */}
      <div className={styles.name}>
        {props.name}
      </div>

      {/* Expand/collapse icon */}
      <MaterialIconButton
        iconName={collapsedIcon}
        className={iconStyles["push-right"]}
      />
    </div>
  );
}