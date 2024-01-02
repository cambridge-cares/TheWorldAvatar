"use client";

import React from "react";
import Konami from 'react-konami-code';
import {  Provider } from "react-redux";

import Trex from "utils/trex";
import Toolbar from "./toolbar/toolbar";
import ContextMenu from "./context-menu/context-menu";
import { reduxStore } from "../app/store";
import { SettingsInterface } from "../io/config/ui-settings";

// Incoming properties for global container
type GlobalContainerProps = {
    children?: React.ReactNode,
    settings: SettingsInterface
}

// Internal state for global container
type GlobalContainerState = {
    popup: boolean
}

/**
 * Component representing a common global page container for all content.
 */
export default class GlobalContainer extends React.Component<GlobalContainerProps, GlobalContainerState> {

    // Initial state
    state = {
        popup: false
    }

    // Update popup state
    setPopup = () => {
        this.setState(prevState => ({
            popup: !prevState.popup
        }));
    }

    // Return displayable element
    render() {
        const { modules, branding } = this.props.settings;

        return (
            <Provider store={reduxStore}>
                <div
                    id="globalContainer"
                    onContextMenu={(e) => {
                        e.preventDefault()
                    }}> 

                    {/* Right click menu */}
                    <ContextMenu />

                    {/* Slim toolbar component */}
                    <Toolbar
                        showLanding={modules.landing}
                        toolbarLogo={branding.toolbarLogo.toString()}
                    />

                    {/* Main content container */}
                    <div id="contentContainer">
                        {this.props.children}
                    </div>
                    
                    <Konami action={this.setPopup} timeout={6000} resetDelay={1000}/>
                    {this.state.popup &&
                        <Trex callback={this.setPopup}/>
                    }
                </div>
            </Provider>
        );
    }

}