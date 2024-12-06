"use client";

import styles from './ribbon-component.module.css';

import React, { useEffect, useState } from 'react';
import { Icon, Tooltip } from '@mui/material';
import { useDispatch, useSelector } from 'react-redux';
import { getOption, setOption } from 'state/ribbon-component-slice';
import IconComponent from 'ui/graphic/icon/icon';

interface RibbonComponentOptionsProps {
    id: string,
    icon: string,
    text?: string,
    tooltip: string,
    initialOption: string,
    options: string[],
    action: () => void,
    iconClickable?: boolean
}

export default function RibbonComponentOptions(props: Readonly<RibbonComponentOptionsProps>) {
    const [expanded, setExpanded] = useState(false);
    const option = useSelector(getOption(props.id));
    const dispatch = useDispatch();

    // Triggered on dropdown option selection
    const selectAction = (selectedOption: string) => {
        dispatch(setOption({
            id: props.id,
            selection: selectedOption
        }));
        setExpanded(false);
        props.action();
    }

    // Triggered on click on dropdown arrow
    const toggleAction = () => {
        setExpanded(!expanded);
    }

    const closeAction = (event: MouseEvent) => {
        const target = event.target as HTMLElement;
        if (!target.classList.contains("material-icons")) {
            setExpanded(false);
        }
    }

    // Create dropdown options
    const dropdown = createOptions(
        props.options,
        (option == null) ? props.initialOption : option.selection,
        selectAction
    )

    // On mount, add LMB listener
    useEffect(() => {
        document.addEventListener("click", closeAction);
    }, []);
    // On unmount, remove LMB listener
    useEffect(() => () => {
        document.removeEventListener("click", closeAction);
    }, []);

    let innerClass = styles.ribbonComponentInner;
    if (props.iconClickable != null && !props.iconClickable) {
        innerClass = styles.ribbonComponentInnerDisabled;
    }

    return (
        <div className={styles.ribbonComponent}>
            <Tooltip
                title={props.tooltip}
                enterDelay={1000}
                leaveDelay={100}
                placement="bottom-start">

                <div>
                    <div className={innerClass} onClick={props.action}>
                        <div className={styles.ribbonComponentIcon}>
                            <IconComponent icon={props.icon} />
                        </div>
                        {props.text &&
                            <div className={styles.ribbonComponentText}>
                                {props.text}
                            </div>}
                    </div>
                    <div className={styles.dropdownArrowIconContainer}>
                        <Icon
                            className={`material-symbols-outlined ${styles.ribbonComponentArrow}`}
                            onClick={toggleAction}>
                            keyboard_arrow_down
                        </Icon>
                    </div>
                </div>
            </Tooltip>
            {expanded && dropdown}
        </div>
    );
}

/**
 * Creates elements for the component drop down.
 * 
 * @param options possible dropdown options.
 * @param selectedOption selected dropdown option.
 * 
 * @returns react elements.
 */
function createOptions(
    options: string[],
    selectedOption: string,
    selectAction: (_selection: string) => void) {

    return (
        <div id="ribbonDropdown" className={styles.ribbonDropdown}>
            {options.map((option) => {

                const classNames = [styles.ribbonOption];
                if (selectedOption === option) classNames.push(styles.active);
                return (
                    <div
                        className={classNames.join(" ")}
                        key={option}
                        onClick={() => selectAction(option)}>

                        <p>{option}</p>
                    </div>
                );
            })}
        </div>
    )
}