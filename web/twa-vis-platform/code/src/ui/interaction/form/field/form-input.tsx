import styles from './field.module.css';

import React from 'react';
import { FieldError, UseFormReturn } from 'react-hook-form';

import { PropertyShape, VALUE_KEY } from 'types/form';
import { FORM_STATES, getRegisterOptions } from 'ui/interaction/form/form-utils';
import FormInputContainer from './form-input-container';

export interface InputFieldProps {
  field: PropertyShape;
  instanceType: string;
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
 * @param {boolean} options.disabled Optional indicator if the field should be disabled. Defaults to false.
 * @param {string[]} styles.label Optional styles for the label element.
 * @param {string[]} styles.input Optional styles for the input element.
 */
export default function FormInputField(props: Readonly<InputFieldProps>) {
  const inputClassNames: string = props.styles?.input?.join(" ");
  const label: string = `${props.field.name[VALUE_KEY]} of ${props.instanceType.replace("_", " ")}`;
  // Disabled inputs should provide only text input
  const inputType: string = props.options?.disabled || props.field.datatype === "string" ? "text" : "number";
  return (
    <FormInputContainer
      field={props.field}
      error={props.form.formState.errors[props.field.fieldId] as FieldError}
      labelStyles={props.styles?.label}
    >
      <input
        id={props.field.fieldId}
        type={inputType}
        className={`${inputClassNames} ${props.options?.disabled && (styles["input-disabled"] + " " + styles["field-disabled"])}`}
        step={props.field.datatype === "decimal" ? "0.00000000000000001" : undefined}
        placeholder={`Add ${label} here`}
        readOnly={props.options?.disabled}
        aria-label={label}
        {...props.form.register(props.field.fieldId, getRegisterOptions(props.field, props.form.getValues(FORM_STATES.FORM_TYPE)))}
      />
    </FormInputContainer>
  );
}