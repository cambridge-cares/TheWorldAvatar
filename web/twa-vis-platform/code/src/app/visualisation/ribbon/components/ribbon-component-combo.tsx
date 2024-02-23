"use client";

import styles from "./ribbon-component.module.css";

import React, { useEffect, useState } from "react";
import SVG from 'react-inlinesvg';
import { Icon, Tooltip } from "@mui/material";
import { useDispatch, useSelector } from "react-redux";
import { getOption, setOption } from "../../../../state/ribbon-component-slice";


type Props = {
    icon: string,
    text: string,
    tooltip: string,
    initialOption: string,
    options: string[],
    action: () => void,
    iconClickable?: boolean
}

export default function RibbonComponentCombo(props: Props) {
    const [expanded, setExpanded] = useState(false);
    const option = useSelector(getOption(props.text));
    const dispatch = useDispatch();

    // Triggered on dropdown option selection
    const selectAction = (selectedOption: string) => {
        dispatch(setOption({
            name: props.text,
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
        if(!target.classList.contains("material-icons")) {
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

    let iconElement;
    if(props.icon.endsWith(".svg")) {
        // Image file
        iconElement = (
            <SVG src={props.icon}/>
        );
    } else {
        // Name of Google material icon
        iconElement = (
            <Icon className="material-symbols-outlined">
                {props.icon}
            </Icon>
        );
    }

    let innerClass = styles.ribbonComponentInner;
    if(props.iconClickable != null && !props.iconClickable) {
        innerClass = styles.ribbonComponentInnerDisabled;
    }

    return (
        <div className={styles.ribbonComponent}>
            <Tooltip
                title={props.tooltip}
                enterDelay={1000}
                leaveDelay={100}
                placement="bottom-start">
                
                <>
                    <div className={innerClass} onClick={props.action}>
                        <div className={styles.ribbonComponentIcon}>
                            {iconElement}
                        </div>
                        <div className={styles.ribbonComponentText}>
                            {props.text}
                        </div>
                    </div>
                    <Icon 
                        className={`material-symbols-outlined ${styles.ribbonComponentArrow}`}
                        onClick={toggleAction}>
                        keyboard_arrow_down
                    </Icon>
                    {expanded && dropdown}
                </>
            </Tooltip>
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
    selectAction: (selection: string) => void) {

    return (
        <div id="ribbonDropdown" className={styles.ribbonDropdown}>
            {options.map((option) => {
            
                const classNames = [styles.ribbonOption];
                if(selectedOption === option) classNames.push(styles.active);
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