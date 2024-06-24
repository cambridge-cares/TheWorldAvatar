import React from 'react';

import IconComponent from 'ui/graphic/icon/icon';
import LoadingSpinner from 'ui/graphic/loader/spinner';

interface HeaderFieldProps {
  name: string;
  icon: string;
  containerStyle: string;
  headerNameStyle: string;
  isLoading: boolean;
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
 * @param {boolean} isLoading  Indicates if a loading indicator is required.
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

      {/* Renders a loading indicator when required, or else, shows the required icon */}
      {props.isLoading ?
        <LoadingSpinner isSmall={true} /> :
        <IconComponent
          icon={props.icon}
        />
      }
    </div>
  );
}