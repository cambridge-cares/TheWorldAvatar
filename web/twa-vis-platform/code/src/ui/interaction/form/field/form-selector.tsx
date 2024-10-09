import styles from './field.module.css';

import React from 'react';
import { Control, FieldError, FieldValues, UseFormReturn, useWatch } from 'react-hook-form';

import { ONTOLOGY_CONCEPT_ROOT, OntologyConcept, OntologyConceptMappings, PropertyShape, VALUE_KEY } from 'types/form';
import { FORM_STATES, getRegisterOptions, getMatchingConcept } from 'ui/interaction/form/form-utils';
import FormInputContainer from './form-input-container';

interface FormSelectorProps {
  field: PropertyShape;
  selectOptions: OntologyConceptMappings;
  form: UseFormReturn;
  styles?: {
    label?: string[],
  };
}

/**
 * This component renders a dropdown selector for the form.
 * 
 * @param {PropertyShape} field The field name that will be assigned to the form state.
 * @param {OntologyConceptMappings} selectOptions The mappings of option values for the dropdown.
 * @param {UseFormReturn} form A react-hook-form hook containing methods and state for managing the associated form.
 * @param {string[]} styles.label Optional styles for the label element.
 */
export default function FormSelector(props: Readonly<FormSelectorProps>) {
  const control: Control = props.form.control;
  const currentOption: string = useWatch<FieldValues>({
    control,
    name: props.field.fieldId,
  });
  const label: string = props.field.name[VALUE_KEY];

  if (props.selectOptions[ONTOLOGY_CONCEPT_ROOT]) {
    
    const selectedOption: OntologyConcept = getMatchingConcept(props.selectOptions, currentOption);

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
          {...props.form.register(props.field.fieldId, getRegisterOptions(props.field, props.form.getValues(FORM_STATES.FORM_TYPE)))}
        >
          {props.selectOptions[ONTOLOGY_CONCEPT_ROOT].map((option, index) => {
            const parentKey: string = option.type.value;
            // If there are children options, return an opt group with the children options
            if (props.selectOptions[parentKey]) {
              return <optgroup key={option.label.value + index} label={option.label.value}>
                {props.selectOptions[parentKey].map(childOption =>
                  <option key={childOption.label.value} value={childOption.type.value}>
                    {childOption.label.value}
                  </option>
                )}
              </optgroup>
            } else {
              // Return the childless group as an option
              return (
                <option key={option.label.value + index} value={option.type.value}>
                  {option.label.value}
                </option>
              )
            }
          })}
        </select>
      </FormInputContainer>
    );
  }
}