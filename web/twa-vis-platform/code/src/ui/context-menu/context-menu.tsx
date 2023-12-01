"use client";

import { useLocation } from 'react-router-dom'

import Icon from "@mui/material/Icon";
import styles from "./context-menu.module.css";

import React from "react";

export type ContextMenuItem = {
    id: string,
    name: string, 
    toggleEnabled: boolean,
    toggleDefault?: boolean,
    callback?: (id: string, toggled: boolean) => void
}

type ContextMenuProps = {
    items: ContextMenuItem[]
}

type ContextMenuState = {
    xPos: string,
    yPos: string,
    show: boolean
}

/**
 * 
 */
export default class ContextMenu extends React.Component<ContextMenuProps, ContextMenuState> {
    state = {
        xPos: "0px",
        yPos: "0px",
        show: false
    }

    componentDidMount() {
        document.addEventListener("click", this.handleClick);
        document.addEventListener("contextmenu", this.handleContextMenu);
    }

    componentWillUnmount() {
        document.removeEventListener("click", this.handleClick);
        document.removeEventListener("contextmenu", this.handleContextMenu);
    }

    // eslint-disable-next-line @typescript-eslint/no-unused-vars
    handleClick = (e: MouseEvent) => {

        const location = useLocation();
        console.log("RIGHT-CLICKED");
        console.log(location.pathname);

        if (this.state.show) this.setState({ show: false });
    }

    handleContextMenu = (e: MouseEvent) => {
        e.preventDefault();
        this.setState({
            xPos: `${e.pageX}px`,
            yPos: `${e.pageY}px`,
            show: true
        });
        console.log("X = " + e.pageX);
        console.log("Y = " + e.pageY);
    }

    render() {
        const { show, xPos, yPos } = this.state;
        if(!show) return null;

        const iconClass = ["material-symbols-outlined", styles.icon].join(" ");
        const items = this.props.items;

        return (
            <div
                className={styles.menu}
                style={{
                    position: "absolute",
                    top: yPos,
                    left: xPos
                }}>

                {items.map((item) => (
                    <div key={item.id} className={styles.menuItem}>
                        <span className={styles.text}>{item.name}</span>

                        {item.toggleEnabled &&
                            <Icon className={iconClass}>
                                {item.toggleDefault ? "check_box" : "check_box_outline_blank"}
                            </Icon>
                        }
                    </div>
                ))}
            </div>
        );
    }
}