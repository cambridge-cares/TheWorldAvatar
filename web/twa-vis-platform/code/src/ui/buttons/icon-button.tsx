"use client";

import React from 'react';

import Icon from '@mui/material/Icon';
import styles from './icon-button.module.css';

// Interface for properties
interface Props {
    iconName: string,
    classStyles?: {
        container?: string[],
        icon?: string[],
    },
    text?: {
        styles?: string[],
        content: string,
    }
    callback?: () => void
}

/**
 * A material icon button with custom styling and icons.
 * 
 * @param {string} iconName The icon name from the material UI library.
 * @param {string[]} classStyles.container An optional array of CSS class names for the container.
 * @param {string[]} classStyles.icon An optional array of CSS class names for the icon.
 * @param {string} text.content An optional text content if required.
 * @param {string[]} text.styles An optional array of CSS class names for the text content.
 * @param {Function} callback An optional callback function if required.
 */
export default function MaterialIconButton({ iconName, classStyles, text, callback }: Props) {
    // CSS classes
    const containerClassNames = [styles["icon-button-container"]].concat(classStyles?.container).join(" ");
    const iconClassNames = ["material-symbols-outlined", styles["icon-button"]].concat(classStyles?.icon).join(" ");
    const textClassNames = text?.styles.join(" ");

    return (
        <div className={containerClassNames}>
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