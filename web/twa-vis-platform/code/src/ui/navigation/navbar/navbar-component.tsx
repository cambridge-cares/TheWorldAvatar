"use client";

import styles from './navbar-component.module.css';

import React from 'react';
import Icon from '@mui/material/Icon';
import { Tooltip } from '@mui/material';

import AppLink from 'ui/navigation/link/link';


// Type definition for incoming parameters
export type NavbarComponentProps = {
    name: string,
    tooltip: string,
    icon: string, 
    url: string,
    active?: boolean,
    callback?: (name: string) => void
}

/**
 * This class represents an abstract navigation bar button.
 */
export default class NavbarComponent extends React.Component<NavbarComponentProps> {

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
            <AppLink
                className={styles.navbarButton}
                onClick={this.bubbleUp}
                url={this.props.url}>

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
            </AppLink>
        );
    }
}
