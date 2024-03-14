import styles from './layer-tree.module.css';

import React from 'react';

import { DataGroup } from 'io/data/data-group';
import { DataLayer } from 'io/data/data-layer';
import { DataStore } from 'io/data/data-store';
import { MapLayerGroup, MapLayer } from 'types/map-layer';
import LayerTreeHeader from './layer-tree-content';

// type definition for incoming properties
type LayerTreeProps = {
  dataStore: DataStore;
};

/**
 * Dynamically load and reader controls to toggle groups and layers,
 * with optional icons, in a LayerTree component.
 */
export default function LayerTree(props: LayerTreeProps) {
  const structure: MapLayerGroup[] = parseIntoTreeStucture(props.dataStore);
  return <div className={styles.layerTreeContainer}>
    {structure.map((mapLayerGroup) => {
      return (
        <LayerTreeHeader
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
 */
function parseIntoTreeStucture(dataStore: DataStore): MapLayerGroup[] {
  const root: MapLayerGroup[] = [];
  dataStore.getGroups().forEach((group) => {
    recurseParseTreeStructure(root, group, null);
  });
  return root;
}

/**
 * Recursively parse the input group into the tree compatible structure.
 *
 * @param results array to add new root level nodes too.
 * @param dataGroup current data group.
 * @param parentGroup parent tree group.
 */
function recurseParseTreeStructure(
  results: MapLayerGroup[],
  dataGroup: DataGroup,
  parentGroup: MapLayerGroup
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
      icon: layers.find(layer => layer.definition["tree-icon"] !== null)?.definition["tree-icon"] as string,
      isVisible: layers.find(layer => layer.cachedVisibility !== null)?.cachedVisibility,
    };
    mapLayerGroup.layers.push(mapLayer);
  }

  // Recurse down
  for (const subGroup of dataGroup.subGroups) {
    recurseParseTreeStructure(results, subGroup, mapLayerGroup);
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