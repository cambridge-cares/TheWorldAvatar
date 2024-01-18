
import { Icon, Tooltip } from "@mui/material";
import styles from "./floating-panel.module.css";

import React from "react";
import { getIndex, setIndex } from "../../state/floating-panel-slice";
import { useDispatch, useSelector } from "react-redux";
import { DataStore } from "../../io/data/data-store";
import LayerTree from "./layer-tree";

// Incoming parameters for component.
type FloatingPanelContainerProps = {
    dataStore: DataStore,
    hideLegend?: boolean,
    hideInfo?: boolean
}

/**
 * Floating panel that contains the layer tree and legend components.
 */
export default function FloatingPanelContainer(props: FloatingPanelContainerProps) {
    const showLegend = props.hideLegend == null || !props.hideLegend;
    const showInfo = props.hideInfo == null || !props.hideInfo;

    const activeIndex = useSelector(getIndex);
    const dispatch = useDispatch();

    const buttonClass = styles.headButton;
    const buttonClassActive = [styles.headButton, styles.active].join(" ");

    const clickAction = (index: number) => {
        dispatch(setIndex({
            index: index
        }));
    }

    return (
        <div className={styles.floatingPanelContainer}>
            
            <div className={styles.floatingPanelHead}>

                {/* Layer tree button */}
                <button
                    className={(activeIndex == 0) ? buttonClassActive : buttonClass}
                    onClick={() => clickAction(0)}>

                    <Tooltip
                        title="Layer Selection"
                        enterDelay={1000}
                        leaveDelay={100}
                        placement="bottom-start">    

                        <Icon className="material-symbols-outlined">
                            stacks
                        </Icon>
                    </Tooltip>
                </button>

                {/* Legend button */}
                {showLegend &&
                     <button
                        className={(activeIndex == 1) ? buttonClassActive : buttonClass}
                        onClick={() => clickAction(1)}>

                        <Tooltip
                            title="Key/Legend"
                            enterDelay={1000}
                            leaveDelay={100}
                            placement="bottom-start">    
                                
                            <Icon className="material-symbols-outlined">
                                key_vertical
                            </Icon>
                        </Tooltip>
                    </button>
                }

                {/* Info button */}
                {showInfo &&
                    <button
                        className={(activeIndex == 2) ? buttonClassActive : buttonClass}
                        onClick={() => clickAction(2)}>

                        <Tooltip
                            title="Information"
                            enterDelay={1000}
                            leaveDelay={100}
                            placement="bottom-start">    
                                
                            <Icon className="material-symbols-outlined">
                                info
                            </Icon>
                        </Tooltip>
                    </button>
                }
            </div>

            <div className={styles.floatingPanelBody}>
                <LayerTree dataStore={props.dataStore}/>
            </div>
        </div>
    )
}