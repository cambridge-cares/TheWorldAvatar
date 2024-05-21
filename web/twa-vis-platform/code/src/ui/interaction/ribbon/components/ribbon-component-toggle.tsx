"use client";

import styles from './ribbon-component.module.css';

import React from 'react';
import { Tooltip } from '@mui/material';
import { useDispatch, useSelector } from 'react-redux';
import { getOption, setOption } from 'state/ribbon-component-slice';
import IconComponent from 'ui/graphic/icon/icon';

interface RibbonComponentToggleProps {
    icon: string,
    text: string,
    tooltip: string,
    initialState: boolean,
    action: (state: boolean) => void
}

export default function RibbonComponentToggle(props: Readonly<RibbonComponentToggleProps>) {
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
                            <IconComponent icon={props.icon} />
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