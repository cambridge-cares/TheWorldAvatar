import styles from './field.module.css';

import React from 'react';
import { FieldError, UseFormReturn } from 'react-hook-form';

import { PropertyShape, VALUE_KEY } from 'types/form';
import { FORM_STATES, getRegisterOptions } from 'ui/interaction/form/form-utils';
import FormInputContainer from './form-input-container';

interface FormDateTimePickerProps {
  field: PropertyShape;
  form: UseFormReturn;
  defaultDateTime?: string;
  styles?: {
    label?: string[],
  };
}

/**
 * This component renders a date time picker for the form.
 * 
 * @param {PropertyShape} field The form field data model. 
 * @param {UseFormReturn} form A react-hook-form hook containing methods and state for managing the associated form.
 * @param {string} defaultDateTime Optional default date time value for the input. Typically used for existing values.
 * @param {string[]} styles.label Optional styles for the label element.
 */
export default function FormDateTimePicker(props: Readonly<FormDateTimePickerProps>) {
  const dateType: string = "date";
  const timeType: string = "time";

  let formatLabel: string;
  // Retrieve input type based on field input
  let inputType: string;
  if (props.field.datatype === dateType) {
    inputType = dateType;
    formatLabel = "DD/MM/YYYY";
  } else if (props.field.datatype === timeType) {
    inputType = timeType;
    formatLabel = "HH:MM";
  } else {
    inputType = "datetime-local";
    formatLabel = "DD/MM/YYYY HH:MM";
  }

  // Retrieve current date or time depending on field required
  let currentDateTime: string = new Date().toISOString();
  if (props.defaultDateTime) {
    currentDateTime = props.defaultDateTime;
  } else if (props.field.datatype === dateType) {
    currentDateTime = currentDateTime.split("T")[0];
  } else if (props.field.datatype === timeType) {
    currentDateTime = currentDateTime.split("T")[1].split(":")[0] + ":00";
  }

  // Initialise default form state value if there are no default values
  if (props.form.getValues(props.field.fieldId) === "") {
    props.form.setValue(props.field.fieldId, currentDateTime);
  }

  return (
    <FormInputContainer
      field={props.field}
      error={props.form.formState.errors[props.field.fieldId] as FieldError}
      labelStyles={props.styles?.label}
      formatLabel={formatLabel}
    >
      <input
        id={props.field.fieldId}
        className={styles["dtpicker"]}
        type={inputType}
        aria-label={props.field.name[VALUE_KEY]}
        {...props.form.register(props.field.fieldId, getRegisterOptions(props.field, props.form.getValues(FORM_STATES.FORM_TYPE)))}
      />
    </FormInputContainer>
  );
}
