import styles from './info-tree.module.css';

import React, { useEffect, useRef, useState } from 'react';

import { Attribute, AttributeGroup } from 'types/attribute';
import { TimeSeriesGroup } from 'types/timeseries';
import { JsonObject } from "types/json";
import InfoTreeNode from './info-tree-node';
import InfoTabs from './info-tabs';
import Chart from 'ui/graphic/chart/chart';
import Table from 'ui/graphic/table/table';
import DropdownField, { DropdownFieldOption } from 'ui/interaction/dropdown/dropdown';
import { parseTimeSeries } from 'utils/client-utils';

const rootKey: string = "meta";
const displayOrderKey: string = "display_order";
const collapseKey: string = "collapse";
const valueKey: string = "value";
const unitKey: string = "unit";

type InfoTreeProps = {
  data: JsonObject;
  isFetching: boolean;
  activeTab: {
    index: number;
    setActiveTab: React.Dispatch<React.SetStateAction<number>>;
  }
};

/**
 * This component is responsible for displaying information about the selected geographic feature 
 * such as name, description, and IRI. Data is passed from the parent component so that 
 * the existing state is persisted even if this component is removed.
 * 
 * @param {JsonObject} data The queried data that will be processed for display.
 * @param {boolean} isFetching An indicator if the query is still running.
 */
export default function InfoTree(props: InfoTreeProps) {
  const timeSeries: React.MutableRefObject<TimeSeriesGroup> = useRef(null);
  const options: React.MutableRefObject<DropdownFieldOption[]> = useRef([]);
  const [selectedTimeSeriesOption, setSelectedTimeSeriesOption] = useState(0);

  const consumeTimeSeries = (data: JsonObject) => {
    if (data?.time && Object.keys(data.time).length > 0 ) {
      timeSeries.current = parseTimeSeries(data);
      options.current = [];
      timeSeries.current.data.map((timeSeries, index) => {
        const label: string = timeSeries.unit === "-" ? timeSeries.name : timeSeries.name + " [" + timeSeries.unit + "]";
        options.current.push({ index: index, label: label });
      });
    }
  };
  // Transform time series whenever the info tree is rendered and the data changes
  consumeTimeSeries(props.data); // Required whenever the component is rerendered
  useEffect(() => {
    consumeTimeSeries(props.data);
    setSelectedTimeSeriesOption(0);
  }, [props.data]);

  return (
    <div className={styles.infoPanelContainer}>
      <div className={styles.infoHeadSection}>
        <h2>Feature Information</h2>
        {// Display the tabs only if there are both meta and time
        !props.isFetching && props.data?.meta && props.data?.time && (
          <InfoTabs
            data={props.data}
            activeTab={{
              index: props.activeTab.index,
              setActiveTab: props.activeTab.setActiveTab,
            }}
          />)}
      </div>
      <div className={styles.infoSection}>
        {props.isFetching ? (
          <div className={styles.spinner}></div>
        ) : props.data ?
          // If active tab is 0, render the Metadata Tree
          props.activeTab.index === 0 ? <InfoTreeNode attribute={recurseParseAttributeGroup(props.data, rootKey)} /> :
            // If active tab is not 0 ie 1, render the time series panel
            <>
              <DropdownField options={options.current} selectedIndex={selectedTimeSeriesOption} setSelectedIndex={setSelectedTimeSeriesOption} />
              <Chart data={timeSeries.current} selectedIndex={selectedTimeSeriesOption} />
              <Table group={timeSeries.current} selectedIndex={selectedTimeSeriesOption} />
            </>
          : (<p>Click to fetch feature information.</p>)
        }
      </div>
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
    const currentValue: JsonObject = currentDataObject[currentVal] as JsonObject;
    // Parses the attribute for nested values and units
    // Javascript falsy checks returns true for 0. But we wish to accept 0 too
    if (currentValue[valueKey] || currentValue[valueKey] === 0) {
      const unit: string = currentValue[unitKey] ? currentValue[unitKey].toString() : "";
      attributes.push(parseAttribute(currentVal, currentValue[valueKey].toString(), unit))
    } else {
      typeof currentValue === "string" || typeof currentValue === "number" ?
        attributes.push(parseAttribute(currentVal, currentValue)) : // Simplified attribute parsing
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
 * @param {string} unit The attribute unit if available.
 * @returns {Attribute} The parsed attribute.
 */
function parseAttribute(property: string, value: string, unit: string = ""): Attribute {
  return {
    name: property,
    value: value,
    unit: unit,
  };
}