import styles from './layer-tree.module.css';

import React, { useState } from 'react';
import SVG from 'react-inlinesvg';
import { Icon } from '@mui/material';

import { DataGroup } from 'io/data/data-group';
import { DataLayer } from 'io/data/data-layer';
import { DataStore } from 'io/data/data-store';

// type definition for incoming properties
type LayerTreeProps = {
  dataStore: DataStore;
};

type TreeGroup = {
  name: string;
  address: string;
  subGroups?: TreeGroup[];
  icon?: string;
  layers?: TreeLayer[];
  isOpen?: boolean; // Add a property to track open/closed state
};
type TreeLayer = {
  name: string;
  address: string;
  ids: string;
  icon?: string;
};

/**
 * Dynamically load and reader controls to toggle groups and layers,
 * with optional icons, in a LayerTree component.
 */
export default function LayerTree(props: LayerTreeProps) {
  const structure = parseIntoTreeStucture(props.dataStore);

  const elements: React.ReactElement[] = [];
  structure.forEach((treeGroup) => {
    elements.push(buildGroup(treeGroup, 0));
  });

  return <div className={styles.layerTreeContainer}>{elements}</div>;
}

/**
 * Parse the input DataStore structure into one used to build the
 * tree. Note that the tree is not built directly from the DataStore
 * structure as we first need to combine any layers with duplicate
 * names.
 *
 * @param dataStore DataStore containing groups.
 */
function parseIntoTreeStucture(dataStore: DataStore) {
  const root: TreeGroup[] = [];
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
  results: TreeGroup[],
  dataGroup: DataGroup,
  parentGroup: TreeGroup
) {
  const treeGroup: TreeGroup = {
    name: dataGroup.name,
    address:
      parentGroup == null
        ? dataGroup.name
        : parentGroup.address + "." + dataGroup.name,
    icon: dataGroup.treeIcon,
    layers: [],
    subGroups: [],
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
    const treeLayer: TreeLayer = {
      name: key,
      address: dataGroup.name + "." + key,
      ids: collectIDs(layers),
      icon: findFirstIcon(layers),
    };
    treeGroup.layers.push(treeLayer);
  }

  // Recurse down
  for (const subGroup of dataGroup.subGroups) {
    recurseParseTreeStructure(results, subGroup, treeGroup);
  }

  if (parentGroup == null) {
    results.push(treeGroup);
  } else {
    parentGroup.subGroups.push(treeGroup);
  }
}

/**
 *
 * @param layers
 * @returns
 */
function findFirstIcon(layers: DataLayer[]): string {
  for (const layer of layers) {
    if (layer.definition["tree-icon"] != null) {
      return layer.definition["tree-icon"] as string;
    }
  }
  return null;
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

function buildLayers(group: TreeGroup, depth: number): React.ReactElement[] {
  const elements: React.ReactElement[] = [];

  // Size of left hand indentation
  const spacing: string = depth * 16 + "px";

  group.layers.forEach((layer) => {
    elements.push(
      <div className={styles.treeEntry} key={layer.name}>
        <div className={styles.treeEntryHeader}>
          {/* Spacer */}
          <span style={{ width: spacing }} />

          <div className={styles.icon + " " + styles.small}>
            <Icon className="material-symbols-outlined">
              subdirectory_arrow_right
            </Icon>
          </div>

          {/* Tree icon, if present */}
          {layer.icon != null && (
            <div className={styles.icon}>
              <SVG src={layer.icon} />
            </div>
          )}

          {/* Name of group */}
          <div className={styles.textContainer}>
            <span>{layer.name}</span>
          </div>

          {/* Visibility state */}
          <div className={styles.icon}>
            <Icon className="material-symbols-outlined">visibility</Icon>
          </div>
        </div>
      </div>
    );
  });
  return elements;
}

/**
 *
 * @param item
 */
function buildGroup(item: TreeGroup, depth: number): React.ReactElement {
  // Size of left hand indentation
  const spacing: string = depth * 16 + "px";

  // Get layer components.
  const childLayers = buildLayers(item, depth + 1);

  // Get subgroup components
  const childGroups: React.ReactElement[] = [];
  item.subGroups.forEach((subGroup) => {
    childGroups.push(buildGroup(subGroup, depth + 1));
  });

  const clickAction = (event: React.MouseEvent) => {
    const target = event.target as unknown as HTMLCollection;

    console.log(target);
    //console.log(target.item(0));
    // const contents = target.getElementsByClassName(styles.treeEntryContent);

    // console.log("=== Contents ===");
    // console.log(contents);
  };

  const [isExpanded, setIsExpanded] = useState<boolean>(false); // Default to collapsed

  const toggleExpansion = () => {
    setIsExpanded(!isExpanded);
  };
  return (
    <div className={styles.treeEntry} key={item.name}>
      <div className={styles.treeEntryHeader} onClick={toggleExpansion}>
        {/* Spacer */}
        <span style={{ width: spacing }} />

        {/* Expand/collapse icon */}
        <div className={styles.icon}>
          <Icon className="material-symbols-outlined">keyboard_arrow_up</Icon>
        </div>

        {/* Tree icon, if present */}
        {item.icon != null && (
          <div className={styles.icon + " " + styles.treeIcon}>
            <SVG src={item.icon} />
          </div>
        )}

        {/* Name of group */}
        <div className={styles.textContainer}>
          <span>{item.name}</span>
        </div>

        {/* Visibility state */}
        <div className={styles.icon}>
          <Icon className="material-symbols-outlined">visibility</Icon>
        </div>
      </div>

      {/* Conditionally show subgroups when expanded */}
      {isExpanded && (
        <div className={styles.treeEntryContent}>
          {childLayers}
          {childGroups}
        </div>
      )}
    </div>
  );
}
