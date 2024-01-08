"use client";

import styles from "./ribbon-component.module.css";

import React from "react";
import SVG from 'react-inlinesvg';
import { Tooltip } from "@mui/material";


type Props = {
    icon: string,
    text: string,
    tooltip?: string
}

export default class RibbonComponentClick extends React.Component<Props> {

    public render() {

        const content = (
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