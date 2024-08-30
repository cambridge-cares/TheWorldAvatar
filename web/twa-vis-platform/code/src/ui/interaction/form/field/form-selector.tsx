import styles from './field.module.css';

import React from 'react';
import { Control, FieldError, FieldValues, UseFormReturn, useWatch } from 'react-hook-form';

import { OntologyConcept, PropertyShape, VALUE_KEY } from 'types/form';
import { getRegisterOptions } from 'ui/interaction/form/form-utils';
import FormInputContainer from './form-input-container';

interface FormSelectorProps {
  field: PropertyShape;
  selectOptions: OntologyConcept[];
  form: UseFormReturn;
  styles?: {
    label?: string[],
  };
}

/**
 * This component renders a dropdown selector for the form.
 * 
 * @param {PropertyShape} field The field name that will be assigned to the form state.
 * @param {OntologyConcept[]} selectOptions The list of option values for the dropdown.
 * @param {UseFormReturn} form A react-hook-form hook containing methods and state for managing the associated form.
 * @param {string[]} styles.label Optional styles for the label element.
 */
export default function FormSelector(props: Readonly<FormSelectorProps>) {
  const label: string = props.field.name[VALUE_KEY];
  const control: Control = props.form.control;
  const currentOption: string = useWatch<FieldValues>({
    control,
    name: props.field.fieldId,
  });

  const selectedOption: OntologyConcept = props.selectOptions.find(option => option.type === currentOption);

  return (
    <FormInputContainer
      field={props.field}
      error={props.form.formState.errors[props.field.fieldId] as FieldError}
      labelStyles={props.styles?.label}
      selectedOption={selectedOption}
    >
      <select
        id={props.field.fieldId}
        className={styles["selector"]}
        aria-label={label}
        {...props.form.register(props.field.fieldId, getRegisterOptions(props.field))}
      >
        {props.selectOptions.map((option, index) => (
          <option key={option.label + index} value={option.type}>
            {option.label}
          </option>
        ))}
      </select>
    </FormInputContainer>
  );
}