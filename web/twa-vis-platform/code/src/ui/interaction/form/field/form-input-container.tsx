import styles from './field.module.css';

import React, { useState } from 'react';
import { FieldError } from 'react-hook-form';
import { Icon } from '@mui/material';

import { OntologyConcept, PropertyShape, VALUE_KEY } from 'types/form';
import FormErrorComponent from 'ui/text/error/form-error';
import { parseWordsForLabels } from 'utils/client-utils';

export interface FormInputContainerProps {
  field: PropertyShape;
  error: FieldError;
  children: React.ReactNode;
  formatLabel?: string;
  labelStyles?: string[];
  selectedOption?: OntologyConcept;
}

/**
 * This component acts as a container with duplicate elements for a form input.
 * 
 * @param {PropertyShape} field The SHACL shape property for this field. 
 * @param {FieldError} error A react-hook-form error object if an error is present.
 * @param {React.ReactNode} children Children elements for the container.
 * @param {OntologyConcept} selectedOption Optional selected option description.
 * @param {string[]} labelStyles Optional styles for the label element.
 */
export default function FormInputContainer(props: Readonly<FormInputContainerProps>) {
  const [showDescription, setShowDescription] = useState<boolean>(false);
  const labelClassNames: string = props.labelStyles?.join(" ");
  const label: string = props.field.name[VALUE_KEY];

  const toggleDescription = () => {
    setShowDescription(!showDescription);
  };

  return (
    <>
      <label className={labelClassNames} htmlFor={props.field.fieldId}>
        {props.field.description[VALUE_KEY] != "" &&
          <Icon className={`${styles["info-icon"]} material-symbols-outlined`} onClick={toggleDescription}>info</Icon>
        }
        <span className={styles["field-text"]}>{parseWordsForLabels(label)}{props.error && "*"}</span>
        {props.formatLabel && <span className={styles["format-label"]}>{props.formatLabel}</span>}
      </label>
      {props.children}
      {props.field.description[VALUE_KEY] != "" &&
        <p className={`${styles["info-text"]} ${showDescription ? styles["info-text-show"] : styles["info-text-hidden"]}`}>
          <b className={styles["field-text"]}>Description:</b> {props.field.description[VALUE_KEY]}
          {props.selectedOption && (<>
            <br /><br />
            <b className={styles["field-text"]}>{props.selectedOption?.label.value}:</b> {props.selectedOption?.description.value}
          </>)}
        </p>
      }
      {/* Return error for failed validation */}
      <FormErrorComponent
        error={props.error}
      />
    </>
  );
}