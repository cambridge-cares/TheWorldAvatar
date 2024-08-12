import moment from 'moment';
import { useEffect, useState } from 'react';

import { useDispatch, useSelector } from 'react-redux';
import { useFetchDataQuery, useFetchDimensionsQuery } from 'state/api/fia-api';
import { getHasExistingData, setHasExistingData } from 'state/floating-panel-slice';
import { Attribute, AttributeGroup } from 'types/attribute';
import { JsonObject } from 'types/json';
import { ScenarioDimensionsData, TimeSeries, TimeSeriesGroup } from 'types/timeseries';

const rootKey: string = "meta";
const displayOrderKey: string = "display_order";
const collapseKey: string = "collapse";
const valueKey: string = "value";
const unitKey: string = "unit";
const iriKey: string = "iri";
const stackKey: string = "stack";


/**
 * A utility function for generating the Feature Info Agent endpoint for a specific feature.
 * 
 * @param {string} iri The IRI of the target feature.
 * @param {string} stack The stack endpoint associated with the target feature.
 * @param {string} scenario The current scenario ID (if any).
*/
export function generateFIAEndpoint(iri: string, stack: string, scenario: string, dimensionSliderValue?: number[] | number): string {
  let url = `${stack}/feature-info-agent/get?iri=${encodeURIComponent(iri)}`;
  if (scenario && stack && iri) {
    url = `${stack}/CReDoAccessAgent/getMetadataPrivate/${scenario}?iri=${encodeURIComponent(iri)}`;
    if (dimensionSliderValue) {
      url += `&time_index=${dimensionSliderValue.toString()}`;
    }
  }
  return url;
}

export function ScenarioDimensionsEndpoint(stack: string, scenario: string): string {
  return `${stack}/getScenarioTimes/${scenario}`;
}

export const useScenarioDimensionsService = (stack: string, scenario: string): { scenarioDimensions: ScenarioDimensionsData; isDimensionsFetching: boolean } => {
  const { data, isFetching } = useFetchDimensionsQuery(ScenarioDimensionsEndpoint(stack, scenario));

  const [scenarioDimensions, setScenarioDimensions] = useState<ScenarioDimensionsData>({});
  const isDimensionsFetching = isFetching;
  useEffect(() => {
    if (!isFetching) {
      // If there is any data retrieved, set that first
      if (data) {
        setScenarioDimensions(data);
      }
    }
  }, [data, isFetching]);

  return { scenarioDimensions, isDimensionsFetching };
}

/**
 * Custom hook for fetching and processing feature information from the Feature Info Agent.
 * If no data is available in the response, it defaults to the inherent feature properties, excluding the 'iri' key.
 * 
 * @param {string} endpoint The target FIA endpoint.
 * @param {string} selectedIri The selected IRI.
 * @param {object} featureProperties The selected feature's inherent properties, which is used as a fallback.
 * 
 */
export const useFeatureInfoAgentService = (endpoint: string, selectedIri: string, featureProperties: object): { attributes: AttributeGroup; timeSeries: TimeSeriesGroup; isFetching: boolean, isUpdating: boolean } => {
  const dispatch = useDispatch();
  const { data, isFetching } = useFetchDataQuery(endpoint);

  const [queriedData, setQueriedData] = useState(null);
  const [isUpdating, setIsUpdating] = useState<boolean>(false);
  const [attributes, setAttributes] = useState<AttributeGroup>(null);
  const [timeSeries, setTimeSeries] = useState<TimeSeriesGroup>(null);

  const hasExistingData: boolean = useSelector(getHasExistingData);

  useEffect(() => {
    if (!isFetching) {
      // If there is any data retrieved, set that first
      if (data && Object.keys(data).length !== 0) {
        setQueriedData(data);
      } else if (featureProperties) {
        // Else default to built-in data that excludes IRI
        const builtInData = {
          meta: {
            Properties: Object.fromEntries(
              Object.entries(featureProperties).filter(([key]) => key !== 'iri')
            ),
          },
        };
        setQueriedData(builtInData);
      }
    }
  }, [data, featureProperties, isFetching]);

  useEffect(() => {
    setIsUpdating(true);
    if (queriedData?.meta && Object.keys(queriedData.meta).length > 0) {
      let latestAttributes: AttributeGroup;
      if (hasExistingData) {
        latestAttributes = recurseUpdateAttributeGroup(attributes, selectedIri, queriedData);
        dispatch(setHasExistingData(false));
      } else {
        latestAttributes = recurseParseAttributeGroup(queriedData, rootKey);
      }
      setAttributes(latestAttributes);
    }

    if (queriedData?.time && Object.keys(queriedData.time).length > 0) {
      const timeSeriesData: TimeSeriesGroup = parseTimeSeries(queriedData);
      setTimeSeries(timeSeriesData);
    }

    // Add a delay of 100ms delay so that the data can be properly propagated to the component
    setTimeout(() => {
      setIsUpdating(false);
    }, 100);
  }, [queriedData]);

  return { attributes, timeSeries, isFetching, isUpdating };
};

/**
 * Recursively searches for the attribute group of interest containing the update IRI, and
 * update its contents with with the updated data returned.
 *
 * @param {AttributeGroup} currentGroup The current attribute group that will be updated.
 * @param {string} updateIri The IRI of interest.
 * @param {JsonObject} updatedData The updated data that is retrieved and should be integrated into the target.
 */
function recurseUpdateAttributeGroup(currentGroup: AttributeGroup, updateIri: string, updatedData: JsonObject): AttributeGroup {
  // When the update IRI is present, replace the contents with the updated data
  if (containsUpdateIri(currentGroup, updateIri)) {
    const revisedAttributes: AttributeGroup = recurseParseAttributeGroup(updatedData, rootKey);
    revisedAttributes.name = currentGroup.name;
    return revisedAttributes;
  } else {
    // Otherwise, recursively look for the attribute group of interest
    const revisedSubGroups: AttributeGroup[] = [];
    currentGroup.subGroups.map(subGroup => {
      // If the current group contains the subgroup of interest, the isCollapsed must be set to false
      if (containsUpdateIri(subGroup, updateIri)) {
        currentGroup.isCollapsed = false;
      }
      const revisedAttributes: AttributeGroup = recurseUpdateAttributeGroup(subGroup, updateIri, updatedData);
      revisedSubGroups.push(revisedAttributes);
    })
    currentGroup.subGroups = revisedSubGroups;
  }
  return currentGroup;
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
  let isCollapsed: boolean = typeof currentDataObject[collapseKey] === "boolean" ? currentDataObject[collapseKey] : false;

  // When subqueries should be executed to retrieve more information, they will only have an iri that will be stored
  let subQueryIri: string;
  let subQueryStack: string;
  if (Object.hasOwn(currentDataObject, iriKey)) {
    // The header should always be collapsed
    isCollapsed = true;
    subQueryIri = currentDataObject[iriKey] as string;
    // Subqueries can still be executed within the same stack endpoint but will be overwritten if another stack is included
    if (Object.hasOwn(currentDataObject, stackKey)) {
      subQueryStack = currentDataObject[stackKey] as string;
    }
  }

  // Display order will follow the indicated order if a display_order property is passed. Else, it will follow the random order returned by the agent.
  const filterValues: string[] = [collapseKey, valueKey, unitKey];
  const displayOrder: string[] = Array.isArray(currentDataObject[displayOrderKey]) ?
    currentDataObject[displayOrderKey].map(item => JSON.stringify(item).replaceAll("\"", "")).filter(displayOrderKey => keys.includes(displayOrderKey)) : // Convert JSON array to string array without quotes
    keys.filter(key => !filterValues.includes(key)); // Filter out these values

  // Note that the elements will be pushed according to the display order and do not require further processing according to this order
  displayOrder.map((currentVal) => {

    const currentValue: JsonObject = currentDataObject[currentVal] as JsonObject;
    // Parses the attribute for nested values and units
    // Javascript falsy checks returns true for 0. But we wish to accept 0 too
    if (Object.hasOwn(currentValue, valueKey) && (currentValue[valueKey] || currentValue[valueKey] === 0)) {
      const unit: string = currentValue[unitKey] ? currentValue[unitKey].toString() : "";
      attributes.push(parseAttribute(currentVal, currentValue[valueKey].toString(), unit))
    } else {
      if (typeof currentValue === "string" || typeof currentValue === "number") { attributes.push(parseAttribute(currentVal, currentValue)) }
      else {
        subGroups.push(recurseParseAttributeGroup(currentDataObject, currentVal));
      }
    }
  });

  const currentGroup: AttributeGroup = {
    name: currentNode,
    attributes: attributes,
    subGroups: subGroups,
    displayOrder: displayOrder,
    isCollapsed: isCollapsed,
    subQueryIri: subQueryIri,
    subQueryStack: subQueryStack,
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

/**
 * Parse the time series data returned into the time series group data model.
 *
 * @param {JsonObject} data The JSON data to parse.
 * @returns {TimeSeriesGroup} The required data model.
 */
function parseTimeSeries(data: JsonObject): TimeSeriesGroup {
  // Initialise new empty array to store the values
  const timeSeries: TimeSeries[] = [];
  const times: moment.Moment[] = [];

  // Parse data response to comply with typescript
  const timeData: JsonObject = JSON.parse(JSON.stringify(data.time))[0];
  const tsNames: Array<string> = JSON.parse(JSON.stringify(timeData.data));
  const tsUnits: Array<string> = JSON.parse(JSON.stringify(timeData.units));
  const tsValues: Array<Array<number>> = JSON.parse(JSON.stringify(timeData.values));
  const tsValueClass: Array<string> = JSON.parse(JSON.stringify(timeData.valuesClass));
  const timeClass: string = timeData.timeClass as string;
  const rawTimes: number[] = JSON.parse(JSON.stringify(timeData.time));

  for (let t = 0; t < rawTimes.length; t++) {
    if (timeClass === "dateTime" || timeClass === "Instant") {
      times.push(moment(rawTimes[t], "YYYY-MM-DD HH:mm:ss"));
    } else if (timeClass === "offsetTime") {
      times.push(moment(rawTimes[t], "HH:mm:ss"));
    }
  }

  // Extract the current values and unit for each time series
  tsNames.map((name, index) => {
    timeSeries.push({
      name: name,
      unit: tsUnits[index],
      values: tsValues[index],
      valuesClass: tsValueClass[index],
    });
  })
  return {
    id: timeData.id as number,
    timeClass: timeData.timeClass as string,
    momentTimes: times,
    times: rawTimes,
    data: timeSeries,
  };
}

/**
 * Verifies if the group contains the update IRI.
 *
 * @param {AttributeGroup} group The target attribute group.
 * @param {string} updateIri The IRI of interest.
 */
function containsUpdateIri(group: AttributeGroup, updateIri: string): boolean {
  return group.attributes.some(attr => attr.name === "iri" && attr.value === updateIri);
}