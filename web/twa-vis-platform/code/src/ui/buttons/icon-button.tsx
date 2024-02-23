"use client";

import React from 'react';

import Icon from '@mui/material/Icon';
import styles from "./icon-button.module.css";

// Interface for properties
interface Props {
    iconName: string,
    callback?: () => void
}

/**
 * 
 * @param param0 
 */
export default function MaterialIconButton({ iconName, callback }: Props) {
    // CSS classes for icons
    const classNames = ["material-symbols-outlined", styles.iconButton].join(" ");

    return (
        <div className={styles.iconButtonContainer}>
            <Icon className={classNames} onClick={callback}>
                {iconName}
            </Icon>
        </div>
    );
}