"use client";

import styles from './context-menu.module.css';

import React from 'react';
import { connect } from 'react-redux';

import { ReduxState } from 'app/store';
import { addItem, toggleItem } from 'state/context-menu-slice';
import ContextItem, { ContextItemDefinition } from './context-item';

// Incoming properties type
type ContextMenuProps = {
    items?: ContextItemDefinition[],
    addItem?: (item: ContextItemDefinition) => void,
    toggleItem?: (name: string) => void
}

// Internal state
type ContextMenuState = {
    xPos: string, 
    yPos: string,
    show: boolean
}

const toolbarItem: ContextItemDefinition = {
    name: "Show Toolbar",
    description: "Toggle visibility of global toolbar.",
    toggled: true
}

// Time the RMB was pressed down
let rmbDownTime: number;

/**
 * Represents a component for a custom right-click menu, containing instances of the
 * ContextItem class to representing individual items. 
 * 
 * The definition (and current toggled state) of each ContextItem is stored within
 * the global Redux state so it that it persists across the application lifecycle.
 */
class ContextMenu extends React.Component<ContextMenuProps, ContextMenuState> {

    // Default state
    state = {
        xPos: "0px",
        yPos: "0px",
        show: false
    }

    // On render
    componentDidMount() {
        document.addEventListener("click", this.handleLeftClick);
        this.props.addItem(toolbarItem);

        document.onmousedown = (event) => {
            if(event.button === 2) rmbDownTime = Date.now();
        }

        document.onmouseup = (event) => {
            if(event.button === 2) {
                const duration = Date.now() - rmbDownTime;
                if(duration < 500) {
                    this.handleRightClick(event);
                }
            }
        }
    }

    // On dispose
    componentWillUnmount() {
        document.removeEventListener("click", this.handleLeftClick);
    }

    // On left-click
    handleLeftClick = () => {
        if (this.state.show) this.setState({ show: false });
    }

    // On right-click
    handleRightClick = (e: MouseEvent) => {
        e.preventDefault();
        this.setState({
            xPos: `${e.pageX}px`,
            yPos: `${e.pageY}px`,
            show: true
        });
    }

    handleItemClick = (name: string) => {
        this.props.toggleItem(name);
    }

    // Return element(s) for display
    render() {
        const { show, xPos, yPos } = this.state;

        if(!show || this.props.items == null || this.props.items.length === 0){
            return null;
        }

        return (
            <div
                className={styles.menu}
                style={{
                    position: "absolute",
                    top: yPos,
                    left: xPos
                }}>

                {this.props.items.map((item) => (
                    <ContextItem
                        key={item.name}
                        name={item.name}
                        description={item.description ?? ""}
                        toggled={item.toggled}
                        callback={(name: string) => {
                            this.handleItemClick(name);
                        }}
                    />
                ))}
            </div>
        );
    }
}

// Convert redux state to incoming props
const mapStateToProps = (state: ReduxState) => ({
    items: state.contextMenu.items
});
// eslint-disable-next-line @typescript-eslint/no-explicit-any
const mapDispatchToProps = (dispatch: any) => ({
    addItem: (item: ContextItemDefinition) => dispatch(addItem(item)),
    toggleItem: (name: string) => dispatch(toggleItem(name))
});
export default connect(mapStateToProps, mapDispatchToProps)(ContextMenu);