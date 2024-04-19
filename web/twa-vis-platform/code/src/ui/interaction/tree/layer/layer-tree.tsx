import styles from './layer-tree.module.css';

import React from 'react';
import { Map } from 'mapbox-gl';

import { DataGroup } from 'io/data/data-group';
import { DataLayer } from 'io/data/data-layer';
import { DataStore } from 'io/data/data-store';
import { JsonObject } from 'types/json';
import { MapLayerGroup, MapLayer } from 'types/map-layer';
import { IconSettings } from 'types/settings';
import LayerTreeHeader from './layer-tree-content';

// type definition for incoming properties
type LayerTreeProps = {
  map: React.MutableRefObject<Map>;
  dataStore: DataStore;
  icons: IconSettings;
};

/**
 * Dynamically load and reader controls to toggle groups and layers,
 * with optional icons, in a LayerTree component.
 */
export default function LayerTree(props: LayerTreeProps) {
  const structure: MapLayerGroup[] = parseIntoTreeStucture(props.dataStore, props.icons);
  return <div className={styles.layerTreeContainer}>
    {structure.map((mapLayerGroup) => {
      return (
        <LayerTreeHeader
          map={props.map}
          group={mapLayerGroup}
          depth={0}
          parentShowChildren={mapLayerGroup.showChildren}
          key={mapLayerGroup.name}
        />)
    })}
  </div>;
}

/**
 * Parse the input DataStore structure into one used to build the
 * tree. Note that the tree is not built directly from the DataStore
 * structure as we first need to combine any layers with duplicate
 * names.
 *
 * @param dataStore DataStore containing groups.
 * @param icons The mappings for icon names to their corresponding url.
 */
function parseIntoTreeStucture(dataStore: DataStore, icons: IconSettings): MapLayerGroup[] {
  const root: MapLayerGroup[] = [];
  dataStore.getGroups().forEach((group) => {
    recurseParseTreeStructure(root, group, null, icons);
  });
  return root;
}

/**
 * Recursively parse the input group into the tree compatible structure.
 *
 * @param results array to add new root level nodes too.
 * @param dataGroup current data group.
 * @param parentGroup parent tree group.
 * @param icons The mappings for icon names to their corresponding url.
 */
function recurseParseTreeStructure(
  results: MapLayerGroup[],
  dataGroup: DataGroup,
  parentGroup: MapLayerGroup,
  icons: IconSettings
): void {
  const mapLayerGroup: MapLayerGroup = {
    name: dataGroup.name,
    address:
      parentGroup == null
        ? dataGroup.name
        : parentGroup.address + "." + dataGroup.name,
    icon: dataGroup.treeIcon,
    layers: [],
    subGroups: [],
    showChildren: true,
  };

  // Group layers by user facing name
  const layersByName = dataGroup.dataLayers.reduce(
    (result, layer) => ({
      ...result,
      [layer.name]: [...(result[layer.name] || []), layer],
    }),
    {} as { [key: string]: DataLayer[] }
  );

  // Construct TreeLayer instances
  for (const [key, layers] of Object.entries(layersByName)) {
    const mapLayer: MapLayer = {
      name: key,
      address: dataGroup.name + "." + key,
      ids: collectIDs(layers),
      icon: getIcon(layers, icons),
      isVisible: layers.find(layer => layer.cachedVisibility !== null)?.cachedVisibility,
    };
    mapLayerGroup.layers.push(mapLayer);
  }

  // Recurse down
  for (const subGroup of dataGroup.subGroups) {
    recurseParseTreeStructure(results, subGroup, mapLayerGroup, icons);
  }

  if (parentGroup == null) {
    results.push(mapLayerGroup);
  } else {
    parentGroup.subGroups.push(mapLayerGroup);
  }
}

/**
 *
 * @param layers
 * @returns
 */
function collectIDs(layers: DataLayer[]): string {
  const ids = [];
  for (const layer of layers) {
    ids.push(layer.id);
  }
  return ids.join(" ");
}

/** Retrieve the icon from the current layers. This method will prioritise line colors over the icon if available. 
 *
 * @param {DataLayers[]} layers Layers within the grouped layers.
 * @param {IconSettings} icons The mappings for icon names to their corresponding url.
 * @returns {string} the icon name
 */
function getIcon(layers: DataLayer[], icons: IconSettings): string {
  // Retrieve the line and return its color if available
  const lineLayer: DataLayer = layers.find(layer => layer.definition?.type === 'line');
  if (lineLayer) {
    const paint: JsonObject = lineLayer?.definition?.paint as JsonObject;
    return paint["line-color"] as string;
  }
  // If no line is available, retrieve the icon image if available
  const layer: DataLayer = layers.find(layer => isJsonObject(layer.definition?.layout) && isString(layer.definition.layout["icon-image"]));
  if (layer) {
    const layout: JsonObject = layer?.definition?.layout as JsonObject;
    const iconName: string = layout["icon-image"] as string;
    return icons[iconName];
  }
  // Otherwise, defaults to null
  return null
}

// Disable eslint for the typeguard functions to allow compilation
/* eslint-disable @typescript-eslint/no-explicit-any */
function isJsonObject(obj: any): obj is JsonObject {
  return typeof obj === 'object' && !Array.isArray(obj);
}

function isString(value: any): value is string {
  return typeof value === 'string';
}