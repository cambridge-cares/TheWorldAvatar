"use client";

import styles from './context-item.module.css';

import React from 'react';
import Icon from '@mui/material/Icon';
import { Tooltip } from '@mui/material';

export type ContextItemDefinition = {
    name: string,
    description?: string,
    toggled?: boolean,
    callback?: (name: string) => void
}

/**
 * 
 */
export default function ContextItem(props: ContextItemDefinition) {
    
    // CSS class
    const iconClass = ["material-symbols-outlined", styles.icon].join(" ");

    // Update state and fire callback
    const handleClick = () => {
        if(props.callback != null) props.callback(props.name);
    }

    // Return item for rendering
    return (
        <Tooltip
            onClick={handleClick}
            title={props.description}
            enterDelay={1000}
            leaveDelay={100}
            placement="bottom-start">

            <div className={styles.menuItem}>
                <span className={styles.text}>{props.name}</span>
                {props.toggled != null &&
                    <Icon className={iconClass}>
                        {props.toggled ? "check_box" : "check_box_outline_blank"}
                    </Icon>
                }
            </div>
        </Tooltip>
    );
}