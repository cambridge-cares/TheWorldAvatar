import styles from './info-tree.module.css';

import React, { useEffect, useRef, useState } from 'react';
import { useDispatch } from 'react-redux';

import { Attribute, AttributeGroup } from 'types/attribute';
import { TimeSeriesGroup } from 'types/timeseries';
import { JsonObject } from "types/json";
import InfoTreeNode from './info-tree-node';
import InfoTabs from './info-tabs';
import { MapFeaturePayload } from 'state/map-feature-slice';
import Chart from 'ui/graphic/chart/chart';
import Table from 'ui/graphic/table/table';
import DropdownField, { DropdownFieldOption } from 'ui/interaction/dropdown/dropdown';
import FeatureSelector from 'ui/interaction//dropdown/feature-selector';
import { parseTimeSeries, setSelectedFeature } from 'utils/client-utils';

const rootKey: string = "meta";
const displayOrderKey: string = "display_order";
const collapseKey: string = "collapse";
const valueKey: string = "value";
const unitKey: string = "unit";

interface InfoTreeProps {
  data: JsonObject;
  isFetching: boolean;
  activeTab: {
    index: number;
    setActiveTab: React.Dispatch<React.SetStateAction<number>>;
  };
  features: MapFeaturePayload[];
}

/**
 * This component is responsible for displaying information about the selected geographic feature 
 * such as name, description, and IRI. Data is passed from the parent component so that 
 * the existing state is persisted even if this component is removed.
 * 
 * @param {JsonObject} data The queried data that will be processed for display.
 * @param {boolean} isFetching An indicator if the query is still running.
 */
export default function InfoTree(props: Readonly<InfoTreeProps>) {
  const dispatch = useDispatch();
  const timeSeries: React.MutableRefObject<TimeSeriesGroup> = useRef(null);
  const options: React.MutableRefObject<DropdownFieldOption[]> = useRef([]);
  const [selectedTimeSeriesOption, setSelectedTimeSeriesOption] = useState(0);

  const consumeTimeSeries = (data: JsonObject) => {
    if (data?.time && Object.keys(data.time).length > 0) {
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

  // A function that renders the required contents for this panel
  const renderPanelContents: () => React.ReactElement = () => {
    // Render loading spinner if it is still fetching data
    if (props.isFetching) {
      return <div className={styles.spinner}></div>;
    }

    // If there are multiple features clicked, activate feature selector to choose only one
    if (props.features.length > 1) {
      return <FeatureSelector features={props.features} />;
    } else if (props.features.length === 1) {
      // When only feature is available, set its properties
      setSelectedFeature(props.features[0], dispatch);
    }
    // If active tab is 0, render the Metadata Tree
    if (props.data?.meta && props.activeTab.index === 0) {
      return <InfoTreeNode attribute={recurseParseAttributeGroup(props.data, rootKey)} />;
    }

    if (props.data?.time && props.activeTab.index > 0) {
      return (
        <>
          <DropdownField options={options.current} selectedIndex={selectedTimeSeriesOption} setSelectedIndex={setSelectedTimeSeriesOption} />
          <Chart data={timeSeries.current} selectedIndex={selectedTimeSeriesOption} />
          <Table group={timeSeries.current} selectedIndex={selectedTimeSeriesOption} />
        </>
      );
    }
    // Placeholder text when there are no initial data or selected feature
    return <p>Click to fetch feature information.</p>;
  }

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
        {renderPanelContents()}
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
  let parsedVal: string = value;
  let parsedUnit: string = unit;
  // For any RDF literals
  if (typeof value === "string" && value.startsWith("\"")) {
    // Extract the value pattern first from the RDF literal
    const valuePattern: RegExp = /"(\d+(?:\.\d+)?)".*/;
    let match: RegExpExecArray | null = valuePattern.exec(value);
    if (match) { parsedVal = match[1]; }
    // Extract the optional unit pattern from the RDF literal
    const optionalUnitPattern: RegExp = /\[(.*?)\]/;
    match = optionalUnitPattern.exec(value);
    if (match) { parsedUnit = match[1]; }
  }
  return {
    name: property,
    value: parsedVal,
    unit: parsedUnit,
  };
}