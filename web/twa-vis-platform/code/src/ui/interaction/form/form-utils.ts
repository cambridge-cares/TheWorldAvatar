import { FieldValues, RegisterOptions } from "react-hook-form";

import { Paths } from "io/config/routes";
import { PropertyShape, VALUE_KEY, ONTOLOGY_CONCEPT_ROOT, OntologyConcept, OntologyConceptMappings, SEARCH_FORM_TYPE } from "types/form";

export const FORM_STATES: Record<string, string> = {
  ID: "id",
  FORM_TYPE: "formType",
  RECURRENCE: "recurrence",
  MON: "monday",
  TUES: "tuesday",
  WED: "wednesday",
  THURS: "thursday",
  FRI: "friday",
  SAT: "saturday",
  SUN: "sunday",
  START_DATE: "start date",
  END_DATE: "end date",
  START_TIME_PERIOD: "search period from",
  END_TIME_PERIOD: "search period to",
  TIME_SLOT_START: "time slot start",
  TIME_SLOT_END: "time slot end",
  LATITUDE: "latitude",
  LONGITUDE: "longitude",
};

/**
 * Initialises a form field based on the property shape. This function will retrieve the default value
 * as well as append the field ID based on the input.
 * 
 * @param {PropertyShape} field The data model for the field of interest.
 * @param {FieldValues} outputState The current state storing existing form values.
 * @param {string} fieldId The field ID that should be generated.
 */
export function initFormField(field: PropertyShape, outputState: FieldValues, fieldId: string): PropertyShape {
  // If no default value is available, value will default to null
  outputState[fieldId] = getDefaultVal(fieldId, field.defaultValue?.value, outputState.formType);
  // Update property shape with field ID property
  return {
    ...field,
    fieldId
  };
}

/**
 * Get the default value based on the inputs. If default value is given, this will be return, otherwise,
 * it depends on the field name.
 * 
 * @param {string} field The field of interest.
 * @param {string} defaultValue Default value retrieved from the backend, if any.
 * @param {string} formType The type of form.
 */
export function getDefaultVal(field: string, defaultValue: string, formType: string): boolean | number | string {
  if (field == FORM_STATES.ID) {
    // ID property should only be randomised for the add form type, else, use the default value
    if (formType == Paths.REGISTRY_ADD || formType == SEARCH_FORM_TYPE) {
      return Math.random().toString(16).slice(2);
    }
    // Retrieve only the ID without any prefix
    return defaultValue.split("/").pop();
  }

  if (field == FORM_STATES.RECURRENCE) {
    // Recurrence property should have a value of 1 for the add form type, else, use the default value
    if (formType == Paths.REGISTRY_ADD || formType == SEARCH_FORM_TYPE) {
      return 1;
    }
    if (defaultValue === "P1D") {
      return 0;
    }
    // Retrieve and parse the recurrent digit based on default value
    const match: RegExpMatchArray = /P(\d+)D/.exec(defaultValue);
    if (match) {
      return parseInt(match[1], 10) / 7; // The recurrence interval should be divided by 7
    }
  }

  if ([FORM_STATES.SUN, FORM_STATES.MON, FORM_STATES.TUES, FORM_STATES.WED, FORM_STATES.THURS, FORM_STATES.FRI, FORM_STATES.SAT].includes(field)) {
    // Any day of week property should default to false for add form type, else, use the default value
    if (formType == Paths.REGISTRY_ADD || formType == SEARCH_FORM_TYPE) {
      return false;
    }
    // Default value can be null, and should return false if null
    return !!defaultValue;
  }

  // WIP: Set default value Singapore for any City Field temporarily
  // Default values should not be hardcoded here but retrieved in a config instead
  const defaultVal: string = field.includes("city") ? "Singapore" : "";
  // Returns the default value if passed, or else, nothing
  return defaultValue ?? defaultVal;
}

/**
 * Generate the RegisterOptions required for react-hook-form inputs based on user requirements.
 * 
 * @param {PropertyShape} field The SHACL restrictions for the specific property
 * @param {string} formType The type of form.
 */
export function getRegisterOptions(field: PropertyShape, formType: string): RegisterOptions {
  const options: RegisterOptions = {};

  // The field is required if this is currently not the search form and SHACL defines them as optional
  // Also required for start and end search period
  if ((formType != SEARCH_FORM_TYPE && (Number(field.minCount?.[VALUE_KEY]) === 1 && Number(field.maxCount?.[VALUE_KEY]) === 1)) ||
    (field.fieldId == FORM_STATES.START_TIME_PERIOD || field.fieldId == FORM_STATES.END_TIME_PERIOD)) {
    options.required = "Required";
  }

  // For numerical values which must have least meet the min inclusive target
  if (field.minInclusive) {
    options.min = {
      value: Number(field.minInclusive[VALUE_KEY]),
      message: `Please enter a number that is ${field.minInclusive[VALUE_KEY]} or greater!`,
    };
  } else if (field.minExclusive) {
    options.min = {
      value: Number(field.minExclusive[VALUE_KEY]) + 0.1,
      message: `Please enter a number greater than ${field.minExclusive[VALUE_KEY]}!`,
    };
  }

  // For numerical values which must have least meet the max inclusive target
  if (field.maxInclusive) {
    options.max = {
      value: Number(field.maxInclusive[VALUE_KEY]),
      message: `Please enter a number that is ${field.maxInclusive[VALUE_KEY]} or smaller!`,
    };
  } else if (field.maxExclusive) {
    options.max = {
      value: Number(field.maxExclusive[VALUE_KEY]) + 0.1,
      message: `Please enter a number less than  ${field.maxExclusive[VALUE_KEY]}!`,
    };
  }

  if (field.minLength) {
    options.minLength = {
      value: Number(field.minLength[VALUE_KEY]),
      message: `Input requires at least ${field.minLength[VALUE_KEY]} letters!`,
    };
  }
  if (field.maxLength) {
    options.maxLength = {
      value: Number(field.maxLength[VALUE_KEY]),
      message: `Input has exceeded maximum length of ${field.maxLength[VALUE_KEY]} letters!`,
    };
  }

  // For any custom patterns
  if (field.pattern) {
    // Change message if only digits are allowed
    const msg: string = field.pattern[VALUE_KEY] === "^\\d+$" ? `Only numerical inputs are allowed!` :
      `This field must follow the pattern ${field.pattern[VALUE_KEY]}`;
    options.pattern = {
      value: new RegExp(field.pattern[VALUE_KEY]),
      message: msg,
    };
  }
  return options;
}

/**
 * Parse the concepts into the mappings required for display.
 * 
 * @param {OntologyConcept[]} concepts Array of concepts for sorting.
 * @param {string} priority The priority concept that we must find and separate.
 */
export function parseConcepts(concepts: OntologyConcept[], priority: string): OntologyConceptMappings {
  const results: OntologyConceptMappings = {};
  // Ensure that there is a root mapping to collect all parent's information
  results[ONTOLOGY_CONCEPT_ROOT] = [];
  // For handling the priority concept
  let priorityConcept: OntologyConcept;
  // Store the parent key value
  const parentNodes: string[] = [];

  concepts.map(concept => {
    // Store the priority option if found
    if (concept.label.value === priority || concept.type.value === priority) {
      priorityConcept = concept;
    }

    // If it has a parent, the concept should be appended to its parent key
    if (concept.parent) {
      const parentInstance = concept.parent.value;
      // Add a new array if the mapping does not exist
      if (!results[parentInstance]) {
        results[parentInstance] = [];
        parentNodes.push(parentInstance);
      }
      results[parentInstance].push(concept);
    } else {
      // Else if it is a parent, push it to the root mapping
      results[ONTOLOGY_CONCEPT_ROOT].push(concept);
    }
  });
  sortRootConcepts(results, priorityConcept, parentNodes);
  sortChildrenConcepts(results, priorityConcept);
  return results;
}

/**
  * Sorts the root/parents concepts.
  * 
  * @param {OntologyConceptMappings} mappings Newly parsed mappings from the inputs.
  * @param {OntologyConcept} priority The priority concept that we must find and separate.
  * @param {string[]} parentNodes A list of parent nodes for sorting.
  */
function sortRootConcepts(mappings: OntologyConceptMappings, priority: OntologyConcept, parentNodes: string[]): void {
  let priorityConcept: OntologyConcept;
  let parentConcepts: OntologyConcept[] = [];
  let childlessConcepts: OntologyConcept[] = [];
  // Process the concepts to map them
  mappings[ONTOLOGY_CONCEPT_ROOT].map(concept => {
    // Priority may either be a child or parent concept and we should store the right concept
    if (priority && (concept.type.value == priority.parent?.value || concept.label.value == priority.parent?.value
      || concept.label.value == priority.label?.value
    )) {
      // If this is the priority concept, store it directly, and do not sort it out as it will be appended to the first of the array
      priorityConcept = concept;
    } else {
      // If this is a parent concept with children
      if (parentNodes.includes(concept.type.value)) {
        parentConcepts.push(concept);
      } else {
        childlessConcepts.push(concept);
      }
    }
  });
  // Sort the various concepts
  parentConcepts = parentConcepts.sort((a, b) => a.label.value.localeCompare(b.label.value));
  childlessConcepts = childlessConcepts.sort((a, b) => a.label.value.localeCompare(b.label.value));
  // The final sequence should be the prioritised concept if available, followed by childless and then parent concepts.
  mappings[ONTOLOGY_CONCEPT_ROOT] = priorityConcept ? [priorityConcept, ...childlessConcepts, ...parentConcepts] :
    [...childlessConcepts, ...parentConcepts];
}

/**
  * Sorts the children concepts.
  * 
  * @param {OntologyConceptMappings} mappings Newly parsed mappings from the inputs.
  * @param {OntologyConcept} priority The priority concept that we must find and separate.
  */
function sortChildrenConcepts(mappings: OntologyConceptMappings, priority: OntologyConcept): void {
  Object.keys(mappings).map(parentKey => {
    // Ensure that this is not the root
    if (parentKey != ONTOLOGY_CONCEPT_ROOT) {
      // Attempt to find the match concept
      const matchedConcept: OntologyConcept = mappings[parentKey].find(concept => concept.type?.value == priority?.label?.value);
      // Filter out the matching concept if it is present, and sort the children out
      const sortedChildren: OntologyConcept[] = mappings[parentKey].filter(concept => concept.type?.value != priority?.label?.value)
        .sort((a, b) => a.label.value.localeCompare(b.label.value));
      // Append the matching concept to the start if it is present
      if (matchedConcept) { sortedChildren.unshift(matchedConcept); }
      // Overwrite the mappings with the sorted mappings
      mappings[parentKey] = sortedChildren;
    }
  })
}

/**
 * Retrieve the concept that matches the target value in the input mappings.
 * 
 * @param {OntologyConceptMappings} mappings The mappings of option values for the dropdown.
 * @param {string} targetValue The target value for matching with in the concept's type.
 */
export function getMatchingConcept(mappings: OntologyConceptMappings, targetValue: string): OntologyConcept {
  let match: OntologyConcept;
  Object.keys(mappings).map(key => {
    const matchedConcept: OntologyConcept = mappings[key].find(concept => concept.type?.value == targetValue);
    if (matchedConcept) {
      match = matchedConcept;
    }
  });
  return match;
}