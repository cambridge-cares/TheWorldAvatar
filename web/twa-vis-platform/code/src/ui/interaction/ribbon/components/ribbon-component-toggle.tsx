"use client";

import styles from './ribbon-component.module.css';

import React from 'react';
import SVG from 'react-inlinesvg';
import { Icon, Tooltip } from '@mui/material';
import { useDispatch, useSelector } from 'react-redux';
import { getOption, setOption } from 'state/ribbon-component-slice';
import { formatAppUrl } from 'utils/client-utils';

type Props = {
    icon: string,
    text: string,
    tooltip: string,
    initialState: boolean,
    action: (state: boolean) => void
}

export default function RibbonComponentToggle(props: Props) {
    const toggled = useSelector(getOption(props.text));
    const dispatch = useDispatch();

    const classNames = [styles.ribbonComponentInner];
    if(toggled?.selection != null && toggled.selection === true) {
        classNames.push(styles.toggled);
    } else if(toggled?.selection == null && props.initialState) {
        classNames.push(styles.toggled);
    }

    const clickAction = () => {
        const selected = (toggled?.selection == null) ? !props.initialState : !toggled.selection;
        dispatch(setOption({
            name: props.text,
            selection: selected
        }));
        props.action(selected);
    }

    let iconElement;
    if(props.icon.endsWith(".svg")) {
        // Image file
        iconElement = (
            <SVG src={formatAppUrl(props.icon)}/>
        );
    } else {
        // Name of Google material icon
        iconElement = (
            <Icon className="material-symbols-outlined">
                {props.icon}
            </Icon>
        );
    }

    return (
        <div className={styles.ribbonComponent} onClick={clickAction}>
            <Tooltip
                title={props.tooltip}
                enterDelay={1000}
                leaveDelay={100}
                placement="bottom-start">

                <>
                    <div className={classNames.join(" ")}>
                        <div className={styles.ribbonComponentIcon}>
                            {iconElement}
                        </div>
                        <div className={styles.ribbonComponentText}>
                            {props.text}
                        </div>
                    </div>
                    <div style={{height: "10px"}}/>
                </>
            </Tooltip>
        </div>
    );
}