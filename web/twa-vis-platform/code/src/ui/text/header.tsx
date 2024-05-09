import styles from './header.module.css';
import iconStyles from 'ui/graphic/icon/icon-button.module.css';

import React from 'react';

import MaterialIconButton from 'ui/graphic/icon/icon-button';

interface HeaderFieldProps {
  name: string;
  spacing?: string;
  isCollapsed: boolean;
  toggleExpansion: () => void;
}

/**
 * This component renders a header field.
 * 
 * @param {string} name Header name displayed.
 * @param {string} spacing Optional spacing value.
 * @param {boolean} isCollapsed Indicates if the header's fields are collapsed.
 * @param {Function} toggleExpansion Function to toggle expansion on click.
 */
export default function HeaderField(props: Readonly<HeaderFieldProps>) {
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