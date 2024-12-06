"use client";

import React, { useState } from 'react';
import Konami from 'react-konami-code';
import { Provider } from 'react-redux';

import Trex from 'utils/trex';
import ContextMenu from './interaction/context-menu/context-menu';
import Navbar from './navigation/navbar/navbar';
import { reduxStore } from 'app/store';
import { UISettings } from 'types/settings';
import Footer from './text/footer';

// Incoming properties for global container
interface GlobalContainerProps {
  children?: React.ReactNode;
  settings: UISettings;
}

/**
 * Component representing a common global page container for all content.
 */
export default function GlobalContainer(
  props: Readonly<GlobalContainerProps>
) {
  const [popup, setPopup] = useState<boolean>(false);
  const [contextMenuVisible, setContextMenuVisible] = useState<boolean>(false);
  const [contextMenuPosition, setContextMenuPosition] = useState<{ x: number; y: number }>({ x: 0, y: 0 });

  const togglePopup = () => {
    setPopup(!popup);
  };

  // Method to handle right-click and show the context menu
  const handleContextMenu = (e: React.MouseEvent) => {
    e.preventDefault();
    setContextMenuVisible(true);
    setContextMenuPosition({ x: e.pageX, y: e.pageY });
  };

  // Method to close the context menu when it is no longer needed
  const closeContextMenu = () => {
    setContextMenuVisible(false);
  };


  return (
    <Provider store={reduxStore}>
      <div
        id="globalContainer"
        onContextMenu={handleContextMenu}
        onClick={closeContextMenu} // Close context menu when clicking elsewhere
      >
        {/* Conditionally render the ContextMenu component based on contextMenuVisible */}
        {contextMenuVisible && (
          <ContextMenu
            x={contextMenuPosition.x}
            y={contextMenuPosition.y}
            showContextMenu={contextMenuVisible}
          />
        )}

        <Navbar
          settings={props.settings}
        />

        <div id="contentContainer">{props.children}</div>

        <Konami action={togglePopup} timeout={6000} resetDelay={1000} />
        {popup && <Trex callback={togglePopup} />}
        <Footer />
      </div>
    </Provider>
  );
}
