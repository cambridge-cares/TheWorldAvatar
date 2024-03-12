import styles from './info-tree.module.css';

import React, { useEffect, useState } from 'react';
import { useSelector } from 'react-redux';
import { skipToken } from '@reduxjs/toolkit/query/react';
import { Icon } from '@mui/material';

import { DataStore } from 'io/data/data-store';
import {
  getProperties,
  getSourceLayerId,
  getLatLng,
} from 'state/map-feature-slice';
import { Attribute, AttributeGroup } from 'types/attribute';
import { useGetMetadataQuery, ApiParams } from 'utils/server-utils';
import { JsonObject } from "types/json";

// type definition for incoming properties
type InfoTreeProps = {
  dataStore: DataStore;
};

const rootKey: string = "meta";
const displayOrderKey: string = "display_order";
const collapseKey: string = "collapse";
const valueKey: string = "value";
const unitKey: string = "unit";

/**
 * InfoTree component responsible for displaying information about the selected
 * geographic feature and its location. It renders the selected feature's
 * details, such as name, description, and IRI, along with the latitude and
 * longitude of the clicked location. It also fetches and displays additional
 * feature information from an external data source via PanelHandler.
 */
export default function InfoTree(props: InfoTreeProps) {
  // State to store the latitude and longitude of the clicked location
  const latLng = useSelector(getLatLng);
  // State to store the currently selected feature's information
  const selectedFeatureProperties = useSelector(getProperties);
  // State to store the modified stack URL
  const [stack, setStack] = useState("");
  // State to store the currently selected source layer
  const selectedSourceLayer = useSelector(getSourceLayerId);
  // State to store the scenario ID
  const [scenarioID, setScenarioID] = useState("sFCkEoNC");
  // State to store fetched additional information about the selected feature
  const [element, setElement] = useState(null);
  // Execute API call
  const { data, error, isFetching } = useGetMetadataQuery(
    getApiParams() ?? skipToken
  );

  function getApiParams(): ApiParams {
    if (
      !selectedFeatureProperties ||
      !selectedFeatureProperties.iri ||
      !stack
    ) {
      return undefined;
    }
    return {
      iri: selectedFeatureProperties.iri,
      stack: stack,
      scenarioID: scenarioID,
    };
  }

  // Effect to update selected feature's stack endpoint each time a new feature is selected
  useEffect(() => {
    if (selectedFeatureProperties && selectedFeatureProperties.iri) {
      // Retrieving constants
      let stackEndpoint: string =
        props.dataStore.getStackEndpoint(selectedSourceLayer);
      setStack(stackEndpoint);
    }
  }, [selectedFeatureProperties]);

  // Effect to display additional feature information retrieved from an agent only once it has been loaded
  useEffect(() => {
    if (isFetching) {
      // WIP: Add required functionality while data is still being fetched
    } else {
      if (data) {
        setElement(parseIntoElements(data));
      } else if (error) {
        if (!selectedFeatureProperties || !selectedFeatureProperties.iri) {
          console.warn("IRI is missing. Data fetching will be skipped.");
        } else if (!stack) {
          console.warn(
            "Feature does not have a defined stack. Data fetching will be skipped."
          );
        } else {
          console.error("Error fetching data:", error);
        }
      }
    }
  }, [isFetching]);

  return (
    <div className={styles.infoPanelContainer}>
      <div className={styles.infoHeadSection}>
        <h2>Feature Information</h2>
      </div>
      {isFetching ? (
        <div className={styles.spinner}>
          <p></p>
          {/* Playholder for adding a loading spinner */}
        </div>
      ) : element ?
        (
          <div className={styles.infoSection}>{element}</div>
        ) : (
          <div className={styles.infoSection}>
            <p>Click to fetch feature information.</p>
          </div>
        )
      }
    </div>
  );
}
/**
 * Parse the JSON data returned from the Feature Info Agent into an attribute group data model.
 * This structure will be used to populate the layer tree.
 *
 * @param {JsonObject} dataResponse JSON data returned from query.
 * @returns {AttributeGroup} The attribute group data model.
 */

function parseIntoElements(dataResponse: JsonObject): React.ReactElement {
  const rootGroup: AttributeGroup = recurseParseAttributeGroup(dataResponse, rootKey);
  return renderGroup(rootGroup, 0);
}
/**
 * Recursively parse the data returned from the Feature Info Agent into the attribute group data model.
 *
 * @param {JsonObject} data The JSON data to parse.
 * @param {string} currentNode The current node that should be parsed.
 * @returns {AttributeGroup} The attribute group data model.
 */
function recurseParseAttributeGroup(data: JsonObject, currentNode: string): AttributeGroup {
  // Initialise new empty array to store the values
  const attributes: Attribute[] = [];
  const subGroups: AttributeGroup[] = [];
  // Retrieve the current node's data object
  const currentDataObject: JsonObject = JSON.parse(JSON.stringify(data[currentNode]));
  const keys: string[] = Object.keys(currentDataObject);

  // If property is included, assign it or defaults to false
  const isCollapsed: boolean = typeof currentDataObject[collapseKey] === "boolean" ? currentDataObject[collapseKey] : false;

  // Display order will follow the indicated order if a display_order property is passed. Else, it will follow the random order returned by the agent.
  const filterValues: string[] = [collapseKey, valueKey, unitKey];
  const displayOrder: string[] = Array.isArray(currentDataObject[displayOrderKey]) ?
    currentDataObject[displayOrderKey].map(item => JSON.stringify(item).replaceAll("\"", "")) : // Convert JSON array to string array without quotes
    keys.filter(key => !filterValues.includes(key)); // Filter out these values

  // Note that the elements will be pushed according to the display order and do not require further processing according to this order
  displayOrder.map((currentVal) => {
    // Parses the attribute for nested values and units
    // Javascript falsy checks returns true for 0. But we wish to accept 0 too
    if (currentDataObject[currentVal][valueKey] || currentDataObject[currentVal][valueKey] === 0) {
      const unit: string = currentDataObject[currentVal][unitKey] ? currentDataObject[currentVal][unitKey].toString() : "";

      attributes.push(parseAttribute(currentVal, currentDataObject[currentVal][valueKey].toString(), unit))
    } else {
      typeof currentDataObject[currentVal] === "string" || typeof currentDataObject[currentVal] === "number" ?
        attributes.push(parseAttribute(currentVal, currentDataObject[currentVal].toString())) : // Simplified attribute parsing
        subGroups.push(recurseParseAttributeGroup(currentDataObject, currentVal));
    }
  });

  const currentGroup: AttributeGroup = {
    name: currentNode,
    attributes: attributes,
    subGroups: subGroups,
    displayOrder: displayOrder,
    isCollapsed: isCollapsed,
  };
  return currentGroup;
}

/**
 * Parses an attribute.
 *
 * @param {string} property The attribute name.
 * @param {string} value The attribute value.
 * @param {string} value The attribute unit if available.
 * @returns {Attribute} The parsed attribute.
 */
function parseAttribute(property: string, value: string, unit: string = ""): Attribute {
  return {
    name: property,
    value: value,
    unit: unit,
  };
}

/**
 * Renders the group with its possible elements. This is a recursive function that can render subGroups as well.
 *
 * @param {AttributeGroup} item The attribute group to render.
 * @param {number} depth The current depth to this group tree.
 * @returns {React.ReactElement} The HTML DOM element for rendering.
 */
function renderGroup(item: AttributeGroup, depth: number): React.ReactElement {
  // Size of left hand indentation
  const spacing: string = Math.max(depth - 1, 0) * 0.5 + "rem"; // Root should not be indented
  // Get attributes.
  const attributes = renderAttribute(item.attributes, depth + 1);
  // Get subgroup components
  const childGroups: React.ReactElement[] = [];
  item.subGroups.map((subGroup) => {
    childGroups.push(renderGroup(subGroup, depth + 1));
  });
  // For non-root elements ie meta
  if (item.name != rootKey) {
    return (
      <div className={styles.treeEntry} key={item.name}>
        <div className={styles.treeEntryHeader}>
          {/* Spacer */}
          <span style={{ width: spacing }} />

          {/* Expand/collapse icon */}
          <div className={styles.icon}>
            <Icon className="material-symbols-outlined">keyboard_arrow_up</Icon>&nbsp;
          </div>

          {/* Header Name */}
          <div className={styles.treeHeaderName}>
            {item.name}
          </div>
        </div>

        {/* Elements */}
        {attributes}
        {childGroups}
      </div>
    );
  } else {
    // For root element
    return (
      <>
        {attributes}
        {childGroups}
      </>
    );
  }
}

/**
 * Renders the attributes into each entry.
 *
 * @param {Attribute[]} attributes The list of attributes that should be rendered.
 * @param {number} depth The current depth to this group tree.
 * @returns {React.ReactElement[]} The list of HTML DOM element for rendering.
 */
function renderAttribute(attributes: Attribute[], depth: number): React.ReactElement[] {
  const elements: React.ReactElement[] = [];

  // Size of left hand indentation
  const spacing: string = (depth + 1) * 0.5 + "rem";

  attributes.map((attribute) => {
    elements.push(
      <p className={styles.treeAttributeEntry} style={{ marginLeft: spacing }} key={attribute.name}>
        {/* Attribute: Value */}
        {attribute.name}:&nbsp;
        <span className={styles.treeAttributeValue}>
          {attribute.value}&nbsp;{attribute.unit}
        </span>
      </p>
    );
  });
  return elements;
}
