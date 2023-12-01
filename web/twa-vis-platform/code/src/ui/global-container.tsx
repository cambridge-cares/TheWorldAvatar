"use client";

import React from "react";

import Toolbar from "./toolbar/toolbar";
import { SettingsInterface } from "../io/config/ui-settings";
import ContextMenu from "./context-menu/context-menu";
import Raptor from "../utils/raptor";

type GlobalContainerProps = {
    children?: React.ReactNode,
    settings: SettingsInterface
}

export default class GlobalContainer extends React.Component<GlobalContainerProps> {

    menuItems = [
        {
            id: "show-toolbar",
            name: "Show Toolbar",
            toggleEnabled: true,
            toggleDefault: true
        },
        {
            id: "show-ribbon",
            name: "Show Ribbon",
            toggleEnabled: true,
            toggleDefault: true
        }
    ];

    // Return displayable element
    render() {
        const { modules, branding } = this.props.settings;

        return (
            <div
                id="globalContainer"
                onContextMenu={(e) => {
                    e.preventDefault()
                }}>

                <ContextMenu items={this.menuItems} />

                {/* Slim toolbar component */}
                <Toolbar
                    landing={modules.landing}
                    help={modules.help}
                    dashboard={modules.dashboard}
                    toolbarLogo={branding.toolbarLogo.toString()}
                />

                {/* Main content container */}
                <div id="contentContainer">
                    {this.props.children}
                </div>

                {/* <Raptor/> */}
            </div>
        );
    }

}