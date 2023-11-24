"use client";

import React from "react";
import Icon from '@mui/material/Icon';
import { useState } from 'react';
import { Link, Tooltip } from "@mui/material";

import styles from "./toolbar-component.module.css";

// Type definition for incoming parameters
export type ToolbarComponentProps = {
    name: string,
    tooltip: string, 
    icon: string, 
    url: string
}

// Type definition for state
export type ToolbarComponentState = {
    active: boolean
}

/**
 * This class represents an abstract toolbar button.
 */
export class ToolbarComponent extends React.Component<ToolbarComponentProps, ToolbarComponentState> {

    constructor(props: ToolbarComponentProps) {
        super(props);
        this.state = {
            active: false
        };
    }

    /**
     * Returns JSX to render the component.
     * 
     * @returns JSX for component.
     */
    render() {
        let classNames = ["material-symbols-outlined", styles.image].join(" ");
        if(this.state.active) {
            classNames += " active";
        }

        let callback = () => this.setState({
            active: true
        });

        return (
            <Link id="toolbarComponent" className={styles.toolbarButton} href={this.props.url}>
                <Tooltip title={this.props.tooltip} enterDelay={1000} leaveDelay={100} placement="bottom-start">
                    <Icon className={classNames}>
                        {this.props.icon}
                    </Icon>
                </Tooltip>
            </Link>
        );
    }
}
