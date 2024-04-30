"use client";

import React from 'react';
import Konami from 'react-konami-code';
import { Provider } from 'react-redux';

import Trex from 'utils/trex';
import ContextMenu from './interaction/context-menu/context-menu';
import Navbar from './navigation/navbar/navbar';
import { reduxStore } from 'app/store';
import { DefaultSettings } from 'types/settings';
import Footer from './text/footer';

// Incoming properties for global container
type GlobalContainerProps = {
  children?: React.ReactNode;
  settings: DefaultSettings;
};

// Internal state for global container
type GlobalContainerState = {
  popup: boolean;
  contextMenuVisible: boolean;
  contextMenuPosition: { x: number; y: number };
};

/**
 * Component representing a common global page container for all content.
 */
export default class GlobalContainer extends React.Component<
  GlobalContainerProps,
  GlobalContainerState
> {
  state = {
    popup: false,
    contextMenuVisible: false,
    contextMenuPosition: { x: 0, y: 0 },
  };

  setPopup = () => {
    this.setState((prevState) => ({
      popup: !prevState.popup,
    }));
  };

  // New method to handle right-click and show the context menu
  handleContextMenu = (e: React.MouseEvent) => {
    e.preventDefault();
    this.setState({
      contextMenuVisible: true,
      contextMenuPosition: { x: e.pageX, y: e.pageY },
    });
  };

  // New method to close the context menu when it is no longer needed
  closeContextMenu = () => {
    this.setState({
      contextMenuVisible: false,
    });
  };

  render() {
    const { modules, branding } = this.props.settings;

    return (
      <Provider store={reduxStore}>
        <div
          id="globalContainer"
          onContextMenu={this.handleContextMenu}
          onClick={this.closeContextMenu} // Close context menu when clicking elsewhere
        >
          {/* Conditionally render the ContextMenu component based on contextMenuVisible */}
          {this.state.contextMenuVisible && (
            <ContextMenu
              x={this.state.contextMenuPosition.x}
              y={this.state.contextMenuPosition.y}
              showContextMenu={this.state.contextMenuVisible}
            />
          )}

          <Navbar
            showLanding={modules.landing}
            showMap={modules.map}
            showDash={modules.dashboard}
            showHelp={modules.help}
            navbarLogo={branding.navbarLogo.toString()}
          />

          <div id="contentContainer">{this.props.children}</div>

          <Konami action={this.setPopup} timeout={6000} resetDelay={1000} />
          {this.state.popup && <Trex callback={this.setPopup} />}
          <Footer />
        </div>
      </Provider>
    );
  }
}
