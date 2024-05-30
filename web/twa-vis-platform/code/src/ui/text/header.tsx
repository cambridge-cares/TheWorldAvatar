import iconStyles from 'ui/graphic/icon/icon-button.module.css';

import React from 'react';

import MaterialIconButton from 'ui/graphic/icon/icon-button';

interface HeaderFieldProps {
  name: string;
  icon: string;
  containerStyle: string;
  headerNameStyle: string;
  spacing?: string;
  toggleExpansion: () => void;
}

/**
 * This component renders a header field.
 * 
 * @param {string} name Header name displayed.
 * @param {string} icon The icon to display in this header.
 * @param {string} containerStyle Styling for the container.
 * @param {string} headerNameStyle Styling for the header name.
 * @param {string} spacing Optional spacing value.
 * @param {Function} toggleExpansion Function to toggle expansion on click.
 */
export default function HeaderField(props: Readonly<HeaderFieldProps>) {
  return (
    <div style={{ paddingLeft: props.spacing }} className={props.containerStyle} onClick={props.toggleExpansion}>
      {/* Header Name */}
      <div className={props.headerNameStyle}>
        {props.name}
      </div>

      {/* Expand/collapse icon */}
      <MaterialIconButton
        iconName={props.icon}
        className={iconStyles["push-right"]}
      />
    </div>
  );
}