"use client";

import styles from './ribbon-component.module.css';

import React from 'react';
import SVG from 'react-inlinesvg';
import { Icon, Tooltip } from '@mui/material';


type Props = {
    icon: string,
    text: string,
    tooltip: string,
    action: () => void
}

export default class RibbonComponentClick extends React.Component<Props> {

    public render() {

        let iconElement;
        if(this.props.icon.endsWith(".svg")) {
            // Image file
            iconElement = (
                <SVG src={this.props.icon}/>
            );
        } else {
            // Name of Google material icon
            iconElement = (
                <Icon className="material-symbols-outlined">
                    {this.props.icon}
                </Icon>
            );
        }

        const content = (
            <>
                <div className={styles.ribbonComponentInner}>
                    <div className={styles.ribbonComponentIcon}>
                        {iconElement}
                    </div>
                    <div className={styles.ribbonComponentText}>
                        {this.props.text}
                    </div>
                </div>
                <div style={{height: "10px"}}/>
            </>
        );

        return (
            <div className={styles.ribbonComponent} onClick={this.props.action}>
                <Tooltip
                    title={this.props.tooltip}
                    enterDelay={1000}
                    leaveDelay={100}
                    placement="bottom-start">

                    {content}
                </Tooltip>
            </div>
        );
    }
}