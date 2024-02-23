"use client";

import styles from "./toolbar.module.css";

import React from "react";
import SVG from 'react-inlinesvg';
import { Link } from "@mui/material";

import ToolbarComponent from "./toolbar-component";
import { useSelector } from "react-redux";
import { selectItem } from "../../state/context-menu-slice";

// Type definition for toolbar properties
type ToolbarProps = {
    showLanding?: boolean,
    showMap?: boolean,
    showDash?: boolean,
    showHelp?: boolean,
    toolbarLogo?: string
}

// Default values for toolbar properties
const defaultProps: ToolbarProps = {
    showLanding: true,
    showMap: true,
    showDash: true,
    showHelp: true,
    toolbarLogo: null
};

/**
 * Represents the top level toolbar, that loads a number of 
 * custom toolbar components.
 */
export default function Toolbar(props: ToolbarProps) {

    // Visibility state of toolbar
    const toolbarState = useSelector(selectItem("Show Toolbar"));

    // Apply defaults to any missing props
    props = {...defaultProps, ...props};
    
    // Do not show if state exists and is disabled
    if(toolbarState?.toggled != null && !toolbarState.toggled) {
        return null;
    }

    return (
        <div id="toolbar" className={styles.toolbar}>
            
            {/* Render toolbar logo if set */}
            {props.toolbarLogo != null &&
                <Link href="/">
                    <div className="toolbarLogo">
                        <SVG
                            src={props.toolbarLogo}
                        />
                    </div>
                </Link>
            }

            {/* Fill horizontal space */}
            <div className={styles.spacer}/>

            {/* Render each component as required */}
            {props.showLanding &&
                <ToolbarComponent
                    name = "LANDING"
                    tooltip = "Return to landing page."
                    icon = "home"
                    url = "/" />
            }
            {props.showMap &&
                <ToolbarComponent
                    name = "MAP"
                    tooltip = "Geospatial view."
                    icon = "public"
                    url = "/visualisation" />
            }
            {props.showDash &&
                <ToolbarComponent
                    name = "DASH"
                    tooltip = "Analytics view."
                    icon = "monitoring"
                    url = "/analytics" />
            }
            {props.showHelp &&
                <ToolbarComponent
                    name = "HELP"
                    tooltip = "Open help page."
                    icon = "help"
                    url = "/help" />
            }
        </div>
    );
}