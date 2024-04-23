

import styles from './layer-tree.module.css';
import iconStyles from 'ui/graphic/icon/icon-button.module.css';

import SVG from 'react-inlinesvg';
import React, { useState } from 'react';
import { Map } from 'mapbox-gl';

import { MapLayerGroup, MapLayer } from 'types/map-layer';
import MaterialIconButton from 'ui/graphic/icon/icon-button';
import IconComponent from 'ui/graphic/icon/icon';

// type definition for incoming properties
type LayerTreeHeaderProps = {
  map: Map;
  group: MapLayerGroup;
  depth: number;
  parentShowChildren: boolean;
  setMapGroups: React.Dispatch<React.SetStateAction<MapLayerGroup[]>>;
};

type LayerTreeEntryProps = {
  map: Map;
  layer: MapLayer;
  depth: number;
  handleLayerVisibility: (layerIds: string, isVisible: boolean) => void;
};

/**
 * This component renders the header based on the input map layers and their groups.
 *
 * @param {Map} map The current map instance.
 * @param {MapLayerGroup} group The map layer group to render.
 * @param {number} depth The current depth to this tree.
 * @param {boolean} parentShowChildren An indicator based on the parent node that shows children or not.
*/
export default function LayerTreeHeader(props: LayerTreeHeaderProps) {
  // Size of left hand indentation
  const spacing: string = props.depth * 0.8 + "rem";
  const group: MapLayerGroup = props.group;
  const initialState: boolean = props.parentShowChildren ? group.showChildren : false;
  const [isExpanded, setIsExpanded] = useState<boolean>(initialState);

  // A function to hide or show the current group's content and its associated layers
  const toggleExpansion = () => {
    // The data layers should be shown when parents are expanded, and hidden when closed
    if (props.map?.isStyleLoaded()) {
      // If the user wishes to hide the group, the current state should be considered as visible (true)
      const assumedVisibilityState: boolean = props.parentShowChildren ? isExpanded : true;
      recurseToggleChildVisibility(group, assumedVisibilityState);
      // Update the state of the map groups
      props.setMapGroups(prevMapLayers => recursiveSubGroupFinder(prevMapLayers, group.address));
      setIsExpanded(!isExpanded);
    }
  };

  // A function that recursively toggle the children's layers according to the present state
  const recurseToggleChildVisibility = (group: MapLayerGroup, beforeVisibleState: boolean) => {
    group.subGroups.forEach((subGroup) => {
      recurseToggleChildVisibility(subGroup, beforeVisibleState);
    });
    group.layers.forEach((layer) => {
      toggleMapLayerVisibility(layer.ids, beforeVisibleState);
    });
  }

  // A recursive finder that searches for the subgroup matching the input address.
  // If found, inverse the show children property and update the map groups state.
  const recursiveSubGroupFinder = (mapLayers: MapLayerGroup[], address: string): MapLayerGroup[] => {
    return mapLayers.map(layer => {
      if (layer.address === address) {
        // Found the subgroup with matching address
        // Update the required property
        return { ...layer, showChildren: !layer.showChildren };
      } else if (layer.subGroups && layer.subGroups.length > 0) {
        // Recursively traverse subgroups
        return { ...layer, subGroups: recursiveSubGroupFinder(layer.subGroups, address) };
      }
      return layer;
    });
  };

  /** A method that toggles map layer visibility based on the current state.
   * Currently visible layers will become hidden. 
   * Currently hidden layers will become shown.
   */
  const toggleMapLayerVisibility = (layerIds: string, isVisible: boolean) => {
    // Split layer IDs in case there are multiple
    layerIds.split(" ").forEach((id) => {
      if (isVisible) {
        props.map?.setLayoutProperty(id, "visibility", "none");
      } else {
        props.map?.setLayoutProperty(id, "visibility", "visible");
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
          iconName={isExpanded ? "keyboard_arrow_down" : "keyboard_arrow_right"}
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
      </div>

      {/* Conditionally show subgroups when expanded */}
      {
        isExpanded && (
          <div className={styles.treeEntryContent}>
            {group.layers.map((layer) => {
              return (
                <LayerTreeEntry
                  map={props.map}
                  key={layer.address}
                  layer={layer}
                  depth={props.depth + 1}
                  handleLayerVisibility={toggleMapLayerVisibility}
                />)
            })}

            {group.subGroups.map((subGroup) => {
              return (
                <LayerTreeHeader
                  map={props.map}
                  key={subGroup.name}
                  group={subGroup}
                  depth={props.depth + 1}
                  parentShowChildren={isExpanded}
                  setMapGroups={props.setMapGroups}
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
 * @param {Map} map The current map instance.
 * @param {MapLayer} layer The map layer that should be rendered.
 * @param {number} depth The depth of the layer in the layer tree.
 * @param {Function} handleLayerVisibility - Function to handle the toggling of layer visibility.
 */
function LayerTreeEntry(props: LayerTreeEntryProps) {
  const layer: MapLayer = props.layer;
  const firstLayerId: string = layer.ids.split(" ")[0];
  // Size of left hand indentation
  const spacing: string = props.depth * 0.8 + "rem";

  // Initial visibility state depends on only the layer's visibility
  // It will not depends on the parent component initially, as any possible states becomes tricky to enforce
  const [isVisible, setIsVisible] = useState<boolean>(
    // If the map has loaded (ie users are still on the page but switch components), retrieve the current state
    // Else, follow the data's initial state
    props.map?.loaded() ? props.map?.getLayoutProperty(firstLayerId, "visibility") === "visible" : layer.isVisible
  );

  /** This method toggles the layer visibility when the layer icon is clicked
   */
  const toggleLayerVisibility = () => {
    // Toggle visibility on the map based on current state
    props.handleLayerVisibility(layer.ids, isVisible);
    // Get current visibility state of the layer after any toggling
    setIsVisible(props.map?.getLayoutProperty(firstLayerId, "visibility") === "visible");
  };

  return (
    <div className={styles.treeEntry} key={layer.name}>
      <div className={styles.treeEntryHeader}>
        {/* Spacer */}
        <span style={{ width: spacing }} />

        {/* Layer's icon, if present. Either creates a line or icon for display */}
        {layer.icon && (
          layer.icon.startsWith("#") ?
            <span className={iconStyles["line-icon"]} style={{ background: layer.icon }}></span> :
            <IconComponent icon={layer.icon} classes={iconStyles["small-icon-image"]} />
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
