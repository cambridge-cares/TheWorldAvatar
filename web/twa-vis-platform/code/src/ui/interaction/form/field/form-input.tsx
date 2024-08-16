import styles from './field.module.css';

import React, { useState } from 'react';
import { FieldError, UseFormReturn } from 'react-hook-form';
import { Icon } from '@mui/material';

import { PropertyShape, VALUE_KEY } from 'types/form';
import FormErrorComponent from 'ui/text/error/form-error';
import { getRegisterOptions } from 'ui/interaction/form/form-utils';
import { parseWordsForLabels } from 'utils/client-utils';

export interface InputFieldProps {
  field: PropertyShape;
  form: UseFormReturn;
  options?: {
    disabled?: boolean;
  };
  styles?: {
    label?: string[],
    input?: string[],
  };
}

/**
 * This component renders an input field for a form. Note that number inputs will be set to 2 decimal places.
 * 
 * @param {PropertyShape} field The SHACL shape property for this field. 
 * @param {UseFormReturn} form A react-hook-form hook containing methods and state for managing the associated form.
 * @param {boolean} options.required Optional indicator for validation that this field is required. Defaults to false.
 * @param {string[]} styles.label Optional styles for the label element.
 * @param {string[]} styles.input Optional styles for the input element.
 */
export default function FormInputField(props: Readonly<InputFieldProps>) {
  const [showDescription, setShowDescription] = useState<boolean>(false);
  const labelClassNames: string = props.styles?.label?.join(" ");
  const inputClassNames: string = props.styles?.input?.join(" ");
  const label: string = props.field.name[VALUE_KEY];
  // Disabled inputs should provide only text input
  const inputType: string = props.options?.disabled || props.field.datatype === "string" ? "text" : "number";

  const toggleDescription = () => {
    setShowDescription(!showDescription);
  };

  return (
    <>
      <label className={labelClassNames} htmlFor={props.field.fieldId}>
        <Icon className={`${styles["info-icon"]} material-symbols-outlined`} onClick={toggleDescription}>info</Icon>
        <span className={styles["field-text"]}>{parseWordsForLabels(label)}{props.form.formState.errors[props.field.fieldId] && "*"}</span>
      </label>
      <p className={`${styles["info-text"]} ${showDescription ? styles["info-text-show"] : styles["info-text-hidden"]}`}>
        <b className={styles["field-text"]}>Description:</b> {props.field.description[VALUE_KEY]}
      </p>
      <input
        id={props.field.fieldId}
        type={inputType}
        className={`${inputClassNames} ${props.options?.disabled && (styles["input-disabled"] + " " + styles["field-disabled"])}`}
        step={props.field.datatype === "decimal" ? "0.01" : undefined}
        placeholder={`Add ${label} here`}
        readOnly={props.options?.disabled}
        aria-label={label}
        {...props.form.register(props.field.fieldId, getRegisterOptions(
          false,
        ))}
      />
      {/* Return error for failed validation */}
      <FormErrorComponent
        error={props.form.formState.errors[props.field.fieldId] as FieldError}
      />
    </>
  );
}