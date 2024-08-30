import styles from './field.module.css';

import React, { useState } from 'react';
import { Control, FieldError, FieldValues, UseFormReturn, useWatch } from 'react-hook-form';
import { Icon } from '@mui/material';

import { OntologyConcept, PropertyShape, VALUE_KEY } from 'types/form';
import FormErrorComponent from 'ui/text/error/form-error';
import { getRegisterOptions } from 'ui/interaction/form/form-utils';
import { parseWordsForLabels } from 'utils/client-utils';

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
  const labelClassNames: string = props.styles?.label?.join(" ");
  const label: string = parseWordsForLabels(props.field.name[VALUE_KEY]);
  const [showDescription, setShowDescription] = useState<boolean>(false);

  const control: Control = props.form.control;
  const currentOption: string = useWatch<FieldValues>({
    control,
    name: props.field.fieldId,
  });

  const selectedOption: OntologyConcept = props.selectOptions.find(option => option.type === currentOption);

  const toggleDescription = () => {
    setShowDescription(!showDescription);
  };
  return (
    <>
      <label className={labelClassNames} htmlFor={props.field.fieldId}>
        <Icon className={`${styles["info-icon"]} material-symbols-outlined`} onClick={toggleDescription}>info</Icon>
        <span className={styles["field-text"]}>{label}{props.form.formState.errors[props.field.fieldId] && "*"}</span>
      </label>
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
      <p className={`${styles["info-text"]} ${showDescription ? styles["info-text-show"] : styles["info-text-hidden"]}`}>
        <b className={styles["field-text"]}>Description:</b> {props.field.description[VALUE_KEY]}
        <br /><br />
        <b className={styles["field-text"]}>{selectedOption?.label}:</b> {selectedOption?.description}
      </p>
      {/* Return error for failed validation */}
      <FormErrorComponent
        error={props.form.formState.errors[props.field.fieldId] as FieldError}
      />
    </>
  );
}