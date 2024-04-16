"use client";

import React from 'react';

import Icon from '@mui/material/Icon';
import styles from './icon-button.module.css';

// Interface for properties
interface ButtonProps extends React.HTMLAttributes<HTMLDivElement> {
    iconName: string,
    iconStyles?: string[],
    text?: {
        styles?: string[],
        content: string,
    }
    callback?: () => void,
}

interface IndexedButtonProps extends ButtonProps {
    index?: number,
    onButtonClick: (index: number) => void;
}

/**
 * A material icon button with custom styling and icons.
 * 
 * @param {string} iconName The icon name from the material UI library.
 * @param {string[]} iconStyles An optional array of CSS class names for the icon.
 * @param {string} text.content An optional text content if required.
 * @param {string[]} text.styles An optional array of CSS class names for the text content.
 * @param {Function} callback An optional callback function if required.
 */
export default function MaterialIconButton({ iconName, iconStyles, text, callback, ...rest }: ButtonProps) {
    // CSS classes
    const containerClassNames = `${rest.className || ''} ${styles["icon-button-container"]}`.trim();
    const iconClassNames = ["material-symbols-outlined", styles["icon-button"]].concat(iconStyles).join(" ");
    const textClassNames = text?.styles?.join(" ");

    return (
        <div {...rest} className={containerClassNames}>
            <Icon className={iconClassNames} onClick={callback}>
                {iconName}
            </Icon>
            {
                text && (
                    <span className={textClassNames}>
                        {text?.content}
                    </span>
                )
            }
        </div>
    );
}

/**
 * A material icon button that can interact with click events based on the their index.
 * 
 * @param {number} index An optional index for this component. Defaults to 0 if excluded.
 * @param {Function} onButtonClick A function called on the index when clicking the button.
 */
export function MaterialIconButtonWithIndex({ index, onButtonClick, ...rest }: IndexedButtonProps) {
    const [position] = React.useState(index ? index : 0);

    const handleClick = () => {
        onButtonClick(position);
    };

    return (
        <MaterialIconButton {...rest} onClick={handleClick} />
    );
}