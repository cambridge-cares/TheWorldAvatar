import styles from './field.module.css';

import React from 'react';
import { FieldError, UseFormReturn } from 'react-hook-form';

import { getRegisterOptions } from 'ui/interaction/form/form-utils';
import { PropertyShape } from 'types/form';
import FormInputContainer from './form-input-container';

interface FormSelectorProps {
  field: PropertyShape;
  selectOptions: string[];
  selectLabels: string[];
  form: UseFormReturn;
  options?: {
    disabled?: boolean;
  };
  styles?: {
    label?: string[],
  };
}

/**
 * This component renders a dropdown selector for a dependent form section within the form.
 * 
 * @param {PropertyShape} field The field name that will be assigned to the form state.
 * @param {string[]} selectOptions The list of option values for the dropdown.
 * @param {string[]} selectLabels The list of option labels that will be displayed on the dropdown.
 * @param {UseFormReturn} form A react-hook-form hook containing methods and state for managing the associated form.
 * @param {boolean} options.disabled Optional indicator if the field should be disabled. Defaults to false.
 * @param {string[]} styles.label Optional styles for the label element.
 */
export default function DependentFormSelector(props: Readonly<FormSelectorProps>) {
  return (
    <FormInputContainer
      field={props.field}
      error={props.form.formState.errors[props.field.fieldId] as FieldError}
      labelStyles={props.styles?.label}
    >
      <select
        id={props.field.fieldId}
        className={styles["selector"]}
        aria-label={props.field.fieldId}
        disabled={props.options?.disabled}
        {...props.form.register(props.field.fieldId, getRegisterOptions(props.field))}
      >
        {props.selectOptions.map((option, index) => (
          <option key={option} value={option}>
            {props.selectLabels[index]}
          </option>
        ))}
      </select>
    </FormInputContainer>
  );
}