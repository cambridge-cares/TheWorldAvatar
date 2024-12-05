import styles from './input.module.css';
import fieldStyles from '../field.module.css';

import React from 'react';
import { FieldError, UseFormReturn } from 'react-hook-form';

import { PropertyShape, VALUE_KEY } from 'types/form';
import { FORM_STATES, getRegisterOptions } from 'ui/interaction/form/form-utils';
import FormInputContainer from '../form-input-container';

export interface FormInputMinMaxFieldProps {
  field: PropertyShape;
  form: UseFormReturn;
  styles?: {
    label?: string[],
  };
}

/**
 * This component renders two input fields for the minimum and maximum values for a form. Note that number inputs will be set to 2 decimal places.
 * 
 * @param {PropertyShape} field The SHACL shape property for this field. 
 * @param {UseFormReturn} form A react-hook-form hook containing methods and state for managing the associated form.
 * @param {string[]} styles.label Optional styles for the label element.
 */
export default function FormInputMinMaxField(props: Readonly<FormInputMinMaxFieldProps>) {
  const label: string = props.field.name[VALUE_KEY];
  const originalField: string = props.field.fieldId;
  const minFieldId: string = "min " + originalField;
  const maxFieldId: string = "max " + originalField;

  return (
    <FormInputContainer
      field={props.field}
      error={props.form.formState.errors[originalField] as FieldError}
      labelStyles={props.styles?.label}
    >
      <div className={styles["min-max-container"]}>
        <div>
          <label className={props.styles?.label.join(" ")} htmlFor={minFieldId}>
            <span className={fieldStyles["field-text"]}>Min:</span>
          </label>
          <input
            id={minFieldId}
            type={"number"}
            className={styles["min-max-input-value"]}
            step={props.field.datatype === "decimal" ? "0.01" : "1"}
            placeholder={"Enter"}
            aria-label={label}
            {...props.form.register(minFieldId, getRegisterOptions(props.field, props.form.getValues(FORM_STATES.FORM_TYPE)))}
          />
        </div>
        <div className={styles["min-max-divider"]}></div>
        <div>
          <label className={props.styles?.label.join(" ")} htmlFor={maxFieldId}>
            <span className={fieldStyles["field-text"]}>Max:</span>
          </label>
          <input
            id={maxFieldId}
            type={"number"}
            className={styles["min-max-input-value"]}
            step={props.field.datatype === "decimal" ? "0.01" : "1"}
            placeholder={"Enter"}
            aria-label={label}
            {...props.form.register(maxFieldId, getRegisterOptions(props.field, props.form.getValues(FORM_STATES.FORM_TYPE)))}
          />
        </div>
      </div>
    </FormInputContainer>
  );
}