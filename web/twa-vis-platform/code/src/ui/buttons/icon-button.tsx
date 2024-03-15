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
};

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
    const textClassNames = text?.styles.join(" ");

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