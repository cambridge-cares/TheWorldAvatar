

import styles from './layer-tree.module.css';
import iconStyles from 'ui/buttons/icon-button.module.css';

import SVG from 'react-inlinesvg';
import React, { useEffect, useState } from 'react';
import { useSelector } from 'react-redux';

import { getIsStyleLoaded } from 'state/floating-panel-slice';
import { MapLayerGroup, MapLayer } from 'types/map-layer';
import MaterialIconButton from 'ui/buttons/icon-button';

// type definition for incoming properties
type LayerTreeHeaderProps = {
  group: MapLayerGroup;
  depth: number;
  parentShowChildren: boolean;
};

type LayerTreeEntryProps = {
  layer: MapLayer;
  depth: number;
  showChildren: boolean;
  isStyleLoaded: boolean;
  handleLayerVisibility: (layerIds: string, isVisible: boolean) => void;
};

/**
 * This component renders the header based on the input map layers and their groups.
 *
 * @param {MapLayerGroup} group The map layer group to render.
 * @param {number} depth The current depth to this tree.
 * @param {boolean} parentShowChildren An indicator based on the parent node that shows children or not.
*/
export default function LayerTreeHeader(props: LayerTreeHeaderProps) {
  // Size of left hand indentation
  const spacing: string = props.depth * 0.8 + "rem";
  const group: MapLayerGroup = props.group;
  const initialState: boolean = props.parentShowChildren ? group.showChildren : false;
  const [isFirstRender, setIsFirstRender] = useState<boolean>(true);
  const [isExpanded, setIsExpanded] = useState<boolean>(initialState);
  const [showChildren, setShowChildren] = useState<boolean>(initialState);
  const isStyleLoaded: boolean = useSelector(getIsStyleLoaded);

  // Whenever the parent component's show children property is updated, 
  // the layer visibility should be toggled accordingly
  useEffect(() => {
    // Toggle should only occur after first render
    if (isFirstRender) {
      setIsFirstRender(false);
    } else {
      toggleChildrenDisplay(group);
    }
  }, [props.parentShowChildren]);

  // A function to hide or expand the current group's content
  const toggleExpansion = () => {
    setIsExpanded(!isExpanded);
  };

  /** This method toggles the children display indicator in two kind of events:
   * 1) Clicking on the specific header's icon 
   * 2) Whenever the parent header's showChildren property is updated
   */
  const toggleChildrenDisplay = (group: MapLayerGroup) => {
    // Ensure that styles must have been loaded
    if (isStyleLoaded) {
      // If the user wishes to hide the group, the current state should be considered as visible (true)
      // to ensure that toggling it will hide the layers
      let assumedVisibilityState: boolean = false;
      if (props.parentShowChildren) {
        if (showChildren) {
          assumedVisibilityState = true;
        }
      } else {
        assumedVisibilityState = true;
      }
      group.layers.forEach((layer) => {
        toggleMapLayerVisibility(layer.ids, assumedVisibilityState);
      });
      setShowChildren(!assumedVisibilityState);
    }
  };

  /** A method that toggles map layer visibility based on the current state.
   * Currently visible layers will become hidden. 
   * Currently hidden layers will become shown.
   */
  const toggleMapLayerVisibility = (layerIds: string, isVisible: boolean) => {
    // Split layer IDs in case there are multiple
    layerIds.split(" ").forEach((id) => {
      if (isVisible) {
        window.map.setLayoutProperty(id, "visibility", "none");
      } else {
        window.map.setLayoutProperty(id, "visibility", "visible");
      }
    });
  }

  return (
    <div className={styles.treeEntry} key={group.name}>
      <div className={styles.treeEntryHeader}>
        {/* Spacer */}
        <span style={{ width: spacing }} />

        {/* Expand/collapse icon */}
        <MaterialIconButton
          iconName={isExpanded ? "keyboard_arrow_up" : "keyboard_arrow_down"}
          iconStyles={[iconStyles.hover]}
          onClick={toggleExpansion}
        />

        {/* Tree icon, if present */}
        {group.icon != null && (
          <div className={styles.icon + " " + styles.treeIcon}>
            <SVG src={group.icon} />
          </div>
        )}

        {/* Name of group */}
        <div className={styles.textContainer} onClick={toggleExpansion}>
          <span>{group.name}</span>
        </div>

        {/* Visibility state */}
        <MaterialIconButton
          iconName={showChildren ? "visibility" : "visibility_off"}
          iconStyles={[iconStyles.hover]}
          onClick={() => toggleChildrenDisplay(group)}
        />
      </div>

      {/* Conditionally show subgroups when expanded */}
      {
        isExpanded && (
          <div className={styles.treeEntryContent}>
            {group.layers.map((layer) => {
              return (
                <LayerTreeEntry
                  key={layer.address}
                  layer={layer}
                  depth={props.depth + 1}
                  showChildren={showChildren}
                  isStyleLoaded={isStyleLoaded}
                  handleLayerVisibility={toggleMapLayerVisibility}
                />)
            })}

            {group.subGroups.map((subGroup) => {
              return (
                <LayerTreeHeader
                  key={subGroup.name}
                  group={subGroup}
                  depth={props.depth + 1}
                  parentShowChildren={showChildren}
                />)
            })}
          </div>
        )
      }
    </div >
  );
}

/**
 * This component renders the specified map layer.
 *
 * @param {MapLayer} layer The map layer that should be rendered.
 * @param {number} depth The depth of the layer in the layer tree.
 * @param {boolean} showChildren Indicates whether children layers should be shown.
 * @param {boolean} isStyleLoaded Indicates whether the map style is fully loaded.
 * @param {Function} handleLayerVisibility - Function to handle the toggling of layer visibility.
 */
function LayerTreeEntry(props: LayerTreeEntryProps) {
  const layer: MapLayer = props.layer;
  // Size of left hand indentation
  const spacing: string = props.depth * 0.8 + "rem";

  // Initialise the visibility state based on the parent component's visibility and layer's current state
  // Note that when children should NOT be shown, the toggle visibility should always ensure it is hidden by passing in a true state
  const [isVisible, setIsVisible] = useState(props.showChildren ? layer.isVisible : true);

  /** This method toggles the layer visibility in two kind of events:
   * 1) Clicking on specific layer's icon 
   * 2) Whenever the showChildren property is updated
   */
  const toggleLayerVisibility = () => {
    // Ensure that styles must have been loaded
    if (props.isStyleLoaded) {
      const firstLayerId: string = layer.ids.split(" ")[0];
      // Existing visible state before any changes
      const beforeVisibleState: boolean = window.map.getLayoutProperty(firstLayerId, "visibility") === "visible";
      // This boolean is the required state that should toggle either the corresponding hidden or show effect depending on the various states
      const requiredVisibleState: boolean = props.showChildren ?
        beforeVisibleState ? isVisible : false :
        true;

      // Toggle visibility on the map
      props.handleLayerVisibility(layer.ids, requiredVisibleState);
      // Get current visibility state of the layer after any toggling
      const currentVisibleState: boolean = window.map.getLayoutProperty(firstLayerId, "visibility") === "visible";
      setIsVisible(currentVisibleState);
    }
  };

  // Whenever show children property is updated, the layer visiblity should be toggled accordingly depending on the change
  useEffect(() => {
    // Set the current visible state for the first render, else, the behaviour becomes wonky when switching tabs
    toggleLayerVisibility();
  }, [props.showChildren]);

  return (
    <div className={styles.treeEntry} key={layer.name}>
      <div className={styles.treeEntryHeader}>
        {/* Spacer */}
        <span style={{ width: spacing }} />

        <MaterialIconButton
          iconName="subdirectory_arrow_right"
          iconStyles={[iconStyles["small-icon"]]}
        />

        {/* Tree icon, if present */}
        {layer.icon && (
          <div className={styles.icon}>
            <SVG src={layer.icon} />
          </div>
        )}

        {/* Name of group */}
        <div className={styles.textContainer}>
          <span>{layer.name}</span>
        </div>

        {/* Toggle visibility state */}
        <MaterialIconButton
          iconName={isVisible ? "visibility" : "visibility_off"}
          iconStyles={[iconStyles.hover]}
          onClick={toggleLayerVisibility}
        />
      </div>
    </div>
  );
}
