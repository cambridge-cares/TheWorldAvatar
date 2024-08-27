import styles from './field.module.css';
import generalStyles from '../form.module.css';

import React, { useEffect, useRef, useState } from 'react';
import { UseFormReturn } from 'react-hook-form';

import { PathNames } from 'io/config/routes';
import { OntologyConcept, PropertyShape, VALUE_KEY } from 'types/form';
import LoadingSpinner from 'ui/graphic/loader/spinner';
import { reorderConcepts, sortConcepts } from 'utils/client-utils';
import { getAvailableTypes } from 'utils/server-actions';
import FormInputField from './form-input';
import FormDateTimePicker from './form-date-time-picker';
import FormSelector from './form-selector';
import { FORM_STATES } from '../form-utils';

interface FormFieldProps {
  entityType: string;
  agentApi: string;
  field: PropertyShape;
  form: UseFormReturn;
  options?: {
    disabled?: boolean;
  };
}

/**
 * Renders a form field as part of the form component.
 * 
 * @param {string} entityType The type of entity.
 * @param {string} agentApi The target endpoint for contacting the backend agent.
 * @param {PropertyShape} field The form field data model.
 * @param {UseFormReturn} form A react-hook-form hook containing methods and state for managing the associated form.
 * @param {string} defaultText Optional default text for the input.
 * @param {boolean} options.disabled Optional indicator if the field should be disabled. Defaults to false.
 */
export default function FormFieldComponent(props: Readonly<FormFieldProps>) {
  const effectRan = useRef(false);
  const [dropdownValues, setDropdownValues] = useState<OntologyConcept[]>([]);
  const [isFetching, setIsFetching] = useState<boolean>(true);

  // A hook that fetches all concepts for select input on first render
  useEffect(() => {
    // Declare an async function that retrieves all entity concepts for specific attributes
    const getEntityConcepts = async (field: PropertyShape, form: UseFormReturn) => {
      setIsFetching(true);
      let concepts: OntologyConcept[];
      switch (field.name[VALUE_KEY].toLowerCase()) {
        case "status":
          concepts = await getAvailableTypes(props.agentApi, `${props.entityType}/status`);
          break;
        case "type":
          concepts = await getAvailableTypes(props.agentApi, props.entityType);
          break;
        case "country":
        case "bin type":
        case "service":
        case "service type":
        case "truck type":
        case "waste category": {
          // Remove the second half of field value to get required type
          const typeRoute: string = props.field.name[VALUE_KEY].split(" ")[0];
          concepts = await getAvailableTypes(props.agentApi, typeRoute);
          break;
        }
        default:
          concepts = null;
          break;
      }
      if (concepts && concepts.length > 0 && !props.options.disabled) {
        let sortedConcepts: OntologyConcept[] = sortConcepts(concepts);
        sortedConcepts = reorderConcepts(sortedConcepts, form.getValues(field.fieldId));
        if (field.name[VALUE_KEY].toLowerCase() === "country") { sortedConcepts = reorderConcepts(sortedConcepts, "Singapore"); }
        setDropdownValues(sortedConcepts);
        form.setValue(field.fieldId, sortedConcepts[0].type);
      }
      setIsFetching(false);
    }

    if ((props.form.getValues(FORM_STATES.FORM_TYPE) != PathNames.REGISTRY || props.form.getValues(FORM_STATES.FORM_TYPE) != PathNames.REGISTRY_DELETE)
      && props.field.in && !effectRan.current) {
      getEntityConcepts(props.field, props.form);
    }
    // Control flow of data fetching on first and remount to ensure only one fetch request is executed in development mode
    // Read this for more details: https://stackoverflow.com/a/74609594
    return () => { effectRan.current = true };
  }, []);


  if (props.options?.disabled || (
    props.field.datatype && ["string", "integer", "decimal"].includes(props.field.datatype)
  )) {
    return (
      <div className={styles["form-field-container"]}>
        <div className={styles["form-input-container"]}>
          <FormInputField
            field={props.field}
            form={props.form}
            options={{
              disabled: props.options?.disabled,
            }}
            styles={{
              label: [styles["form-input-label"]],
              input: [styles["form-input-value"]],
            }}
          />
        </div>
      </div>
    );
  } else if (props.field.datatype && ["dateTime", "date", "time"].includes(props.field.datatype)) {
    return (
      <div className={styles["form-field-container"]}>
        <div className={styles["form-input-container"]}>
          <FormDateTimePicker
            field={props.field}
            form={props.form}
            styles={{
              label: [styles["form-input-label"]],
            }}
          />
        </div>
      </div>
    );
  } else if (props.field.in) {
    return (
      <div className={styles["form-field-container"]}>
        {isFetching && (
          <div className={generalStyles["loader-container"]}>
            <LoadingSpinner isSmall={true} />
          </div>)
        }
        <div className={styles["form-input-container"]}>
          <FormSelector
            field={props.field}
            form={props.form}
            selectOptions={dropdownValues.map(concept => concept.type)}
            selectLabels={dropdownValues.map(concept => concept.label)}
            styles={{
              label: [styles["form-input-label"]],
            }}
          />
        </div>
      </div>
    );
  }
}