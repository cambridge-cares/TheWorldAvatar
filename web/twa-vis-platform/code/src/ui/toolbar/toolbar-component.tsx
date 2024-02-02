"use client";

import styles from "./toolbar-component.module.css";

import React from "react";

import Icon from '@mui/material/Icon';
import { Link, Tooltip } from "@mui/material";


// Type definition for incoming parameters
export type ToolbarComponentProps = {
    name: string,
    tooltip: string,
    icon: string, 
    url: string,
    active?: boolean,
    callback?: (name: string) => void
}

/**
 * This class represents an abstract toolbar button.
 */
export default class ToolbarComponent extends React.Component<ToolbarComponentProps> {

    // Callback to bubble up to Toolbar
    bubbleUp = () => {
        if(this.props.callback != null) {
            this.props.callback(this.props.name);
        }
    }

    /**
     * Returns JSX to render the component.
     * 
     * @returns JSX for component.
     */
    render() {

        return (
            <Link
                className={styles.toolbarButton}
                onClick={this.bubbleUp}
                href={this.props.url}>

                <Tooltip
                    title={this.props.tooltip}
                    enterDelay={1000}
                    leaveDelay={100}
                    placement="bottom-start">

                    <Icon
                        className={`
                            material-symbols-outlined
                            ${styles.image}
                            ${this.props.active ? styles.active : null}`
                        }>

                        {this.props.icon}
                    </Icon>
                </Tooltip>
               
            </Link>
        );
    }
}
