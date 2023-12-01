"use client";

import styles from "./ribbon-component.module.css";

import React from "react";
import SVG from 'react-inlinesvg';
import Icon from '@mui/material/Icon';
import { Tooltip } from "@mui/material";

type RibbonComponentProps = {
    icon: string,
    text: string,
    tooltip?: string,
    options?: string[]
}

export default class RibbonComponent extends React.Component<RibbonComponentProps> {

    public render() {

        const content = (
            <>
                <div className={styles.ribbonComponentInner}>
                    <div className={styles.ribbonComponentIcon}>
                        <SVG
                            src={this.props.icon}
                        />
                    </div>
                    <div className={styles.ribbonComponentText}>
                        {this.props.text}
                    </div>
                </div>
                <Icon className={`material-symbols-outlined ${styles.ribbonComponentArrow}`}>
                    keyboard_arrow_down
                </Icon>    
            </>
        );

        return (
            <div className={styles.ribbonComponent}>
                {this.props.tooltip &&
                    <Tooltip
                        title={this.props.tooltip}
                        enterDelay={1000}
                        leaveDelay={100}
                        placement="bottom-start">

                        {content}
                    </Tooltip>
                }

                {!this.props.tooltip &&
                    content
                }
            </div>
        );
    }
}