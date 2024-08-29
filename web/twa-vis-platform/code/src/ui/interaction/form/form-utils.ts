import { PathNames } from "io/config/routes";
import { FieldValues, RegisterOptions } from "react-hook-form";
import { PropertyShape, VALUE_KEY } from "types/form";

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
  outputState[fieldId] = getDefaultVal(fieldId, field.defaultValue, outputState.formType);
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
    if (formType == PathNames.REGISTRY_ADD) {
      return Math.random().toString(16).slice(2);
    }
    // Retrieve only the ID without any prefix
    return defaultValue.split("/").pop();
  }

  if (field == FORM_STATES.RECURRENCE) {
    // Recurrence property should have a value of 1 for the add form type, else, use the default value
    if (formType == PathNames.REGISTRY_ADD) {
      return 1;
    }
    // Retrieve and parse the recurrent digit based on default value
    const match: RegExpMatchArray = /P(\d+)D/.exec(defaultValue);
    if (match) {
      return parseInt(match[1], 10) / 7; // The recurrence interval should be divided by 7
    }
  }

  if ([FORM_STATES.SUN, FORM_STATES.MON, FORM_STATES.TUES, FORM_STATES.WED, FORM_STATES.THURS, FORM_STATES.FRI, FORM_STATES.SAT].includes(field)) {
    // Any day of week property should default to false for add form type, else, use the default value
    if (formType == PathNames.REGISTRY_ADD) {
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
 */
export function getRegisterOptions(field: PropertyShape): RegisterOptions {
  const options: RegisterOptions = {};

  // Add register options if the field is required
  if (Number(field.minCount?.[VALUE_KEY]) === 1 && Number(field.maxCount?.[VALUE_KEY]) === 1) {
    options.required = "This field is required!";
  }

  return options;
}

/**
 * Searches for the required field based on the existing fields in the form.
 * 
 * @param {FieldValues} fields Mapping of all form fields.
 * @param {string} targetKey Substring for searching the required field.
 */
export function searchField(fields: FieldValues, targetKey: string): string {
  const targetField: string[] = Object.keys(fields)
    .filter(field => field.toLowerCase().replace(/\s+/g, "").includes(targetKey.toLowerCase().replace(/\s+/g, "")));
  if (targetField.length != 1) {
    console.warn(`There is no field containing the target key: ${targetKey}!`);
  }
  return targetField[0];
}