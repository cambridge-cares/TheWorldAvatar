import styles from './info-tree.module.css';

import React, { useEffect, useState } from 'react';
import { useDispatch, useSelector } from 'react-redux';

import { getQueryTrigger, getUrl, setQueryTrigger } from 'state/map-feature-slice';
import { Attribute, AttributeGroup } from 'types/attribute';
import { useGetMetadataQuery } from 'utils/server-utils';
import { JsonObject } from "types/json";
import InfoTreeNode from './info-tree-node';

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
export default function InfoTree() {
  // State to store the currently selected feature's endpoint holding their attributes
  const selectedUrl = useSelector(getUrl);
  // State to store fetched additional information about the selected feature
  const [attributeGroup, setAttributeGroup] = useState(null);
  // State to store fetched additional information about the selected feature
  const trigger = useSelector(getQueryTrigger);
  // Execute API call
  const { data, error, isFetching } = useGetMetadataQuery(selectedUrl, { skip: !trigger });
  const dispatch = useDispatch();

  // Effect to display additional feature information retrieved from an agent only once it has been loaded
  useEffect(() => {
    if (isFetching) {
      // WIP: Add required functionality while data is still being fetched
    } else if (error) {
      console.error("Error fetching data:", error);
    } else {
      if (data) {
        const rootGroup: AttributeGroup = recurseParseAttributeGroup(data, rootKey);
        setAttributeGroup(rootGroup);
        dispatch(setQueryTrigger(false));
      };
    };
  }, [isFetching]);

  return (
    <div className={styles.infoPanelContainer}>
      <div className={styles.infoHeadSection}>
        <h2>Feature Information</h2>
      </div>
      {isFetching ? (
        <div className={styles.spinner}></div>
      ) : attributeGroup ?
        (<div className={styles.infoSection}><InfoTreeNode attribute={attributeGroup} /></div>) : (
          <div className={styles.infoSection}>
            <p>Click to fetch feature information.</p>
          </div>
        )
      }
    </div>
  );
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