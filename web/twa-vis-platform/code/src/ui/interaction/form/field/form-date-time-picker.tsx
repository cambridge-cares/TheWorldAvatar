import styles from './field.module.css';

import React, { useState } from 'react';
import { FieldError, UseFormReturn } from 'react-hook-form';
import { Icon } from '@mui/material';

import { PropertyShape, VALUE_KEY } from 'types/form';
import FormErrorComponent from 'ui/text/error/form-error';
import { getRegisterOptions } from 'ui/interaction/form/form-utils';
import { parseWordsForLabels } from 'utils/client-utils';

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
  const labelClassNames: string = props.styles?.label?.join(" ");
  const [showDescription, setShowDescription] = useState<boolean>(false);

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

  const toggleDescription = () => {
    setShowDescription(!showDescription);
  };

  return (
    <>
      <label className={labelClassNames} htmlFor={props.field.fieldId}>
        <Icon className={`${styles["info-icon"]} material-symbols-outlined`} onClick={toggleDescription}>info</Icon>
        <span className={styles["field-text"]}>{parseWordsForLabels(props.field.name[VALUE_KEY])}{props.form.formState.errors[props.field.fieldId] && "*"}</span>
        <span className={styles["format-label"]}>{formatLabel}</span>
      </label>
      <p className={`${styles["info-text"]} ${showDescription ? styles["info-text-show"] : styles["info-text-hidden"]}`}>
        <b className={styles["field-text"]}>Description:</b> {props.field.description[VALUE_KEY]}
      </p>
      <input
        id={props.field.fieldId}
        className={styles["dtpicker"]}
        type={inputType}
        aria-label={props.field.name[VALUE_KEY]}
        {...props.form.register(props.field.fieldId, getRegisterOptions(props.field))}
      />
      {/* Return error for failed validation */}
      <FormErrorComponent
        error={props.form.formState.errors[props.field.fieldId] as FieldError}
      />
    </>
  );
}
