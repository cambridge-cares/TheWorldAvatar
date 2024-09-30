import styles from './input.module.css';

import React, { useEffect, useRef, useState } from 'react';
import { Control, FieldError, FieldValues, UseFormReturn, useWatch } from 'react-hook-form';

import { Paths } from 'io/config/routes';
import { defaultSearchOption, ONTOLOGY_CONCEPT_ROOT, OntologyConcept, OntologyConceptMappings, PropertyShape, VALUE_KEY } from 'types/form';
import { FORM_STATES, getRegisterOptions, getMatchingConcept, parseConcepts } from 'ui/interaction/form/form-utils';
import { getAvailableTypes } from 'utils/server-actions';
import FormInputContainer from '../form-input-container';

interface FormSelectorProps {
  instanceType: string;
  agentApi: string;
  field: PropertyShape;
  form: UseFormReturn;
  setIsFetching: React.Dispatch<React.SetStateAction<boolean>>;
  styles?: {
    label?: string[],
  };
}

/**
 * This component renders a dropdown selector for the form.
 * 
 * @param {string} instanceType The type of entity.
 * @param {string} agentApi The target agent endpoint for any registry related functionalities.
 * @param {PropertyShape} field The field name that will be assigned to the form state.
 * @param {UseFormReturn} form A react-hook-form hook containing methods and state for managing the associated form.
 * @param {string[]} styles.label Optional styles for the label element.
 */
export default function FormSelector(props: Readonly<FormSelectorProps>) {
  const control: Control = props.form.control;
  const currentOption: string = useWatch<FieldValues>({
    control,
    name: props.field.fieldId,
  });

  const effectRan = useRef(false);
  const [dropdownValues, setDropdownValues] = useState<OntologyConceptMappings>({});

  // A hook that fetches all concepts for select input on first render
  useEffect(() => {
    // Declare an async function that retrieves all entity concepts for specific attributes
    const getEntityConcepts = async (field: PropertyShape, form: UseFormReturn) => {
      props.setIsFetching(true);
      let concepts: OntologyConcept[];
      // WIP: Refactor to remove this as the data can be retrieved on the backend
      switch (field.name[VALUE_KEY].toLowerCase()) {
        case "status":
          concepts = await getAvailableTypes(props.agentApi, `${props.instanceType}/status`);
          break;
        case "type":
          concepts = await getAvailableTypes(props.agentApi, props.instanceType);
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
          concepts = await getAvailableTypes(props.agentApi, field.name[VALUE_KEY].toLowerCase().replace(/\s+/g, ""));
          break;
      }

      if (concepts && concepts.length > 0) {
        let firstOption: string = form.getValues(field.fieldId);
        // WIP: Set default value Singapore for any Country Field temporarily
        // Default values should not be hardcoded here but retrieved in a config instead
        if (field.name[VALUE_KEY].toLowerCase() === "country") {
          firstOption = "Singapore";
        }
        // Add the default search option only if this is the search form
        if (form.getValues(FORM_STATES.FORM_TYPE) === Paths.SEARCH) {
          firstOption = defaultSearchOption.label.value;
          concepts.unshift(defaultSearchOption);
        }
        const sortedConceptMappings: OntologyConceptMappings = parseConcepts(concepts, firstOption);
        setDropdownValues(sortedConceptMappings);
        // First option should be set if available, else the first parent value should be prioritised
        const firstRootOption: OntologyConcept = sortedConceptMappings[ONTOLOGY_CONCEPT_ROOT][0];
        form.setValue(field.fieldId,
          sortedConceptMappings[firstRootOption.type.value] ? sortedConceptMappings[firstRootOption.type.value][0]?.type?.value
            : firstRootOption?.type?.value);
      }
      props.setIsFetching(false);
    }

    if (!effectRan.current) {
      getEntityConcepts(props.field, props.form);
    }
    // Control flow of data fetching on first and remount to ensure only one fetch request is executed in development mode
    // Read this for more details: https://stackoverflow.com/a/74609594
    return () => { effectRan.current = true };
  }, []);
  const label: string = props.field.name[VALUE_KEY];

  if (dropdownValues[ONTOLOGY_CONCEPT_ROOT]) {

    const selectedOption: OntologyConcept = getMatchingConcept(dropdownValues, currentOption);

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
          {dropdownValues[ONTOLOGY_CONCEPT_ROOT].map((option, index) => {
            const parentKey: string = option.type.value;
            // If there are children options, return an opt group with the children options
            if (dropdownValues[parentKey]) {
              return <optgroup key={option.label.value + index} label={option.label.value}>
                {dropdownValues[parentKey].map(childOption =>
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