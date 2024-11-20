

import iconStyles from 'ui/graphic/icon/icon-button.module.css';
import styles from './layer-tree.module.css';

import { Map } from 'mapbox-gl';
import React, { useState } from 'react';
import { useDispatch } from 'react-redux';
import SVG from 'react-inlinesvg';

import { MapLayer, MapLayerGroup } from 'types/map-layer';
import IconComponent from 'ui/graphic/icon/icon';
import MaterialIconButton from 'ui/graphic/icon/icon-button';
import SimpleDropdownField from 'ui/interaction/dropdown/simple-dropdown';
import SearchModal from 'ui/interaction/modal/search/search-modal';
import { setFilterFeatureIris, setFilterLayerIds, setFilterTimes } from 'state/map-feature-slice';

// type definition for incoming properties
interface LayerTreeHeaderProps {
  map: Map;
  group: MapLayerGroup;
  depth: number;
  parentShowChildren: boolean;
  setMapGroups: React.Dispatch<React.SetStateAction<MapLayerGroup[]>>;
}

interface LayerTreeEntryProps {
  map: Map;
  layer: MapLayer;
  depth: number;
  currentGrouping: string;
  handleLayerVisibility: (_layerIds: string, _isVisible: boolean) => void;
}

/**
 * This component renders the header based on the input map layers and their groups.
 *
 * @param {Map} map The current map instance.
 * @param {MapLayerGroup} group The map layer group to render.
 * @param {number} depth The current depth to this tree.
 * @param {boolean} parentShowChildren An indicator based on the parent node that shows children or not.
*/
export default function LayerTreeHeader(props: Readonly<LayerTreeHeaderProps>) {
  // Size of left hand indentation
  const spacing: string = props.depth * 0.8 + "rem";
  const group: MapLayerGroup = props.group;
  const groupings: string[] = props.group.groupings;
  const initialState: boolean = props.parentShowChildren ? group.showChildren : false;
  const dispatch = useDispatch();
  const [isExpanded, setIsExpanded] = useState<boolean>(initialState);
  const [currentGroupingView, setCurrentGroupingView] = useState<string>(groupings.length > 0 ? groupings[0] : "");
  const [isSearchOpenState, setIsSearchOpenState] = useState<boolean>(false);

  // A function to hide or show the current group's content and its associated layers based on the expansion button
  const toggleExpansion = () => {
    // The data layers should be shown when parents are expanded, and hidden when closed
    if (props.map?.isStyleLoaded()) {
      // In the case when parent do not want to show children, the toggle should then inverse this and show it
      const assumedVisibilityState: boolean = props.parentShowChildren ? isExpanded : false;
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
      // The beforeVisibleState should only be used if there is no groupings
      // If there are groupings, the currently selected view will either be closed or open depending on the current isExpanded state
      // Non-selected views and their layers should never be displayed, and will be default to true
      const visibleState: boolean = currentGroupingView === "" ? beforeVisibleState : layer.grouping === currentGroupingView ? isExpanded : true;
      toggleMapLayerVisibility(layer.ids, visibleState);
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

  /** A method that toggles the grouping visibility based on the currently selected view
   */
  const toggleGroupingVisibility = (currentView: string) => {
    // When a user selects a new option, change the visibility so that only the layers of the selected grouping are shown
    group.layers.forEach((layer) => {
      // This state should be the inverse of the toggled state
      // If we want to switch off the layer, it should start as true
      const visibleState: boolean = layer.grouping !== currentView;
      toggleMapLayerVisibility(layer.ids, visibleState);
    });
    // Reorder the groupings so that the selected grouping is always first and this state is saved
    const selectedIndex = groupings.indexOf(currentView);
    if (selectedIndex !== -1) {
      const currentGrouping: string = groupings.splice(selectedIndex, 1)[0];
      groupings.unshift(currentGrouping);
    }
    // Set current grouping view
    setCurrentGroupingView(currentView);
  }

  /** A method that handles any changes required when the user changes the selected option for groupings if available
   */
  const handleGroupingChange = (event: React.ChangeEvent<HTMLSelectElement>) => {
    toggleGroupingVisibility(event.target.value);
  };

  /** A method to open the search modal on click.
  */
  const openSearchModal = () => {
    const layerIds: string[] = group.layers.map(layer => layer.ids)
    // Add filter layer IDs
    dispatch(setFilterLayerIds(layerIds));
    // Reset filtered features state when opened
    dispatch(setFilterFeatureIris([]));
    dispatch(setFilterTimes([]));
    setIsSearchOpenState(true);
  };

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

        {groupings.length > 0 && (
          <div className={styles["dropdown-container"]}>
            <SimpleDropdownField
              options={groupings}
              handleChange={handleGroupingChange}
            />
          </div>
        )}

        {/* A button to open the search modal when available */}
        {group.search && <MaterialIconButton
          iconName={"find_replace"}
          iconStyles={[iconStyles.hover]}
          onClick={openSearchModal}
        />}
        {group.search && isSearchOpenState && <SearchModal
          id={group.search}
          stack={group.stack}
          show={isSearchOpenState}
          setShowState={setIsSearchOpenState}
        />}
      </div>

      {/* Conditionally show subgroups when expanded */}
      {
        isExpanded && (
          <div className={styles.treeEntryContent}>
            {group.layers.map((layer) => {
              if (groupings.length === 0 || layer.grouping === currentGroupingView) {
                return (
                  <LayerTreeEntry
                    map={props.map}
                    key={layer.address}
                    layer={layer}
                    depth={props.depth + 1}
                    currentGrouping={currentGroupingView}
                    handleLayerVisibility={toggleMapLayerVisibility}
                  />)
              }
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
function LayerTreeEntry(props: Readonly<LayerTreeEntryProps>) {
  const layer: MapLayer = props.layer;
  const firstLayerId: string = layer.ids.split(" ")[0];
  // Size of left hand indentation
  const spacing: string = props.depth * 0.8 + "rem";

  // Initial visibility state depends on only the layer's visibility
  // It will not depends on the parent component initially, as any possible states becomes tricky to enforce
  const [isVisible, setIsVisible] = useState<boolean>(
    // If the map has loaded (ie users are still on the page but switch components), retrieve the current state
    // Else, follow the data's initial state
    props.map?.loaded() ? props.map?.getLayoutProperty(firstLayerId, "visibility") === "visible" : props.currentGrouping === layer.grouping || layer.isVisible
  );

  /** This method toggles the layer visibility when the layer icon is clicked
   */
  const toggleLayerVisibility = () => {
    // Toggle visibility on the map based on current state
    props.handleLayerVisibility(layer.ids, isVisible);
    // Get current visibility state of the layer after any toggling
    setIsVisible(props.map?.getLayoutProperty(firstLayerId, "visibility") === "visible");
  };
  let iconDisplay;
  if (layer.icon?.startsWith("l#")) {
    iconDisplay = <span className={iconStyles["line-icon"]} style={{ background: layer.icon.substring(1) }}></span>
  } else if (layer.icon?.startsWith("c#")) {
    iconDisplay = <span className={iconStyles["circle-icon"]} style={{ background: layer.icon.substring(1) }}></span>
  } else {
    iconDisplay = <IconComponent icon={layer.icon} classes={iconStyles["small-icon-image"]} />
  }
  return (
    <div className={styles.treeEntry} key={layer.name}>
      <div className={styles.treeEntryHeader}>
        {/* Spacer */}
        <span style={{ width: spacing }} />

        {/* Layer's icon, if present. Either creates a line or icon for display */}
        {layer.icon && (
          iconDisplay
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
