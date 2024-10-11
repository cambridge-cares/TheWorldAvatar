import styles from '../form.module.css';
import fieldStyles from '../field/field.module.css';

import React, { useEffect, useState } from 'react';
import { Control, FieldValues, UseFormReturn, useWatch } from 'react-hook-form';
import { useRouter } from 'next/navigation';

import { Paths } from 'io/config/routes';
import { defaultSearchOption, FormOptionType, ID_KEY, PropertyShape, RegistryFieldValues, SEARCH_FORM_TYPE, VALUE_KEY } from 'types/form';
import MaterialIconButton from 'ui/graphic/icon/icon-button';
import LoadingSpinner from 'ui/graphic/loader/spinner';
import { getAfterDelimiter } from 'utils/client-utils';
import { getData } from 'utils/server-actions';
import { FORM_STATES } from '../form-utils';
import DependentFormSelector from '../field/dependent-form-selector';

interface DependentFormSectionProps {
  agentApi: string;
  dependentProp: PropertyShape;
  form: UseFormReturn;
  shapeToFieldMap: Map<string, string>;
}

/**
 * This component renders a form section that has dependencies on related entities.
 * 
 * @param {string} agentApi The target agent endpoint for any registry related functionalities.
 * @param {PropertyShape} dependentProp The dependent property's SHACL restrictions.
 * @param {UseFormReturn} form A react-hook-form hook containing methods and state for managing the associated form.
 */
export function DependentFormSection(props: Readonly<DependentFormSectionProps>) {
  const router = useRouter();
  const label: string = props.dependentProp.name[VALUE_KEY];
  const queryEntityType: string = label.trim().replace(/\s+/g, "_"); // Ensure that all spaces are replaced with _
  const formType: string = props.form.getValues(FORM_STATES.FORM_TYPE);
  const control: Control = props.form.control;
  const [isFetching, setIsFetching] = useState<boolean>(true);
  const [selectElements, setSelectElements] = useState<FormOptionType[]>([]);
  // If there is a need, retrieve the parent field name. Parent field depends on the shape to field mappings.
  // Else, it should remain as an empty string
  const firstRelevantShapeKey: string = props.dependentProp.nodeKind ? props.dependentProp.qualifiedValueShape.find(shape => {
    const parentField: string | undefined = props.shapeToFieldMap.get(shape[ID_KEY]);
    return parentField && parentField.trim() !== "";
  })[ID_KEY] : "";
  const parentField: string = firstRelevantShapeKey != "" ? props.shapeToFieldMap.get(firstRelevantShapeKey)
    : firstRelevantShapeKey;

  const currentParentOption: string = useWatch<FieldValues>({
    control,
    name: parentField,
  });
  const currentOption: string = useWatch<FieldValues>({
    control,
    name: props.dependentProp.fieldId,
  });


  // A hook that fetches the list of dependent entities for the dropdown selector
  // If parent options are available, the list will be refetched on parent option change
  useEffect(() => {
    // Declare an async function to retrieve the list of dependent entities for the dropdown selector
    const getDependencies = async (entityType: string, field: PropertyShape, form: UseFormReturn) => {
      setIsFetching(true);
      let entities: RegistryFieldValues[] = [];
      // If there is supposed to be a parent element, retrieve the data associated with the selected parent option
      if (field.nodeKind) {
        if (currentParentOption) {
          entities = await getData(props.agentApi, parentField, getAfterDelimiter(currentParentOption, "/"), entityType);
        }
        // If there is no valid parent option, there should be no entity
      } else if (formType === Paths.REGISTRY || formType === Paths.REGISTRY_DELETE) {
        // Retrieve only one entity to reduce query times as users cannot edit anything in view or delete mode
        entities = await getData(props.agentApi, entityType, getAfterDelimiter(field.defaultValue.value, "/"));
      } else {
        entities = await getData(props.agentApi, entityType);
      }

      // By default, use the first option's id
      let defaultId: string = entities[0]?.id.value;
      // Search form should always target default value
      if (props.form.getValues(FORM_STATES.FORM_TYPE) === SEARCH_FORM_TYPE) {
        defaultId = defaultSearchOption.type.value;
        // If there is a default value, search and use the option matching the default instance's local name
      } else if (field.defaultValue) {
        const defaultValueId: string = getAfterDelimiter(field.defaultValue.value, "/");
        defaultId = entities.find(entity => getAfterDelimiter(entity.id.value, "/") === defaultValueId).id.value;
      }
      // Set the form value to the default value if available, else, default to the first option
      form.setValue(field.fieldId, defaultId);

      const formFields: FormOptionType[] = [];

      // Retrieve and set the display field accordingly
      if (entities.length > 0) {
        const fields: string[] = Object.keys(entities[0]);
        let displayField: string;
        if (fields.includes("name")) {
          displayField = "name";
        } else if (fields.includes("street")) {
          displayField = "street";
        } else {
          displayField = Object.keys(fields).find((key => key != "id" && key != "iri"));
        }
        entities.map(entity => {
          const formOption: FormOptionType = {
            value: entity.id.value,
            label: entity[displayField]?.value,
          };
          formFields.push(formOption);
        })
      }
      // Sort the fields by the labels
      formFields.sort((a, b) => {
        return a.label.localeCompare(b.label);
      });
      // Add the default search option only if this is the search form
      if (props.form.getValues(FORM_STATES.FORM_TYPE) === SEARCH_FORM_TYPE) {
        // Default option should only use empty string "" as the value
        formFields.unshift({
          label: defaultSearchOption.label.value,
          value: defaultSearchOption.type.value,
        });
      }
      // Update select options
      setSelectElements(formFields);
      setIsFetching(false);
    }

    getDependencies(queryEntityType, props.dependentProp, props.form);
  }, [currentParentOption]);

  // An event handler that will navigate to the required add form when clicked
  const openAddSubEntityModal = () => {
    let url: string = `../add/${queryEntityType}`;
    if (formType != Paths.REGISTRY_ADD) {
      url = `../${url}`;
    }
    router.push(url);
  };

  // An event handler that will navigate to the required view form when clicked
  const openViewSubEntityModal = () => {
    let url: string = `../view/${queryEntityType}/${getAfterDelimiter(currentOption, "/")}`;
    // Other form types will have an extra path for the entity id, except for ADD
    if (formType != Paths.REGISTRY_ADD) {
      url = `../${url}`;
    }
    window.open(url, "_blank");
  };

  // The fieldset should only be displayed if it either does not have parent elements (no nodekind) or 
  // the parent element has been queried and selected
  if ((!props.dependentProp.nodeKind) || (props.dependentProp.nodeKind && currentParentOption && currentParentOption != "")) {
    return (
      <fieldset className={styles["form-dependent-fieldset"]}>
        {isFetching &&
          <div className={styles["loader-container"]}>
            <LoadingSpinner isSmall={true} />
          </div>
        }
        {!isFetching && selectElements.length > 0 && (
          <div className={fieldStyles["form-field-container"]}>
            <div className={fieldStyles["form-input-container"]}>
              <DependentFormSelector
                field={props.dependentProp}
                form={props.form}
                fieldOptions={selectElements}
                options={{
                  disabled: formType == Paths.REGISTRY || formType == Paths.REGISTRY_DELETE
                }}
                styles={{
                  label: [styles["form-input-label"]],
                }} />
            </div>
          </div>
        )}
        <div className={styles["form-dependent-button-layout"]}>
          {!isFetching && formType != SEARCH_FORM_TYPE && (selectElements.length > 0 ? <MaterialIconButton
            iconName={"expand_circle_right"}
            className={styles["button"] + " " + styles["button-layout"]}
            text={{
              styles: [styles["button-text"]],
              content: `View more details`
            }}
            onClick={openViewSubEntityModal}
          /> : <p style={{ margin: "0 0.75rem", padding: "0.2rem 0.25rem" }} className={styles["button-text"]}>
            No {label} detected
          </p>
          )}
          {(formType != Paths.REGISTRY && formType != Paths.REGISTRY_DELETE && formType != SEARCH_FORM_TYPE) && (
            <MaterialIconButton
              iconName={"add"}
              className={styles["button"] + " " + styles["button-layout"]}
              text={{
                styles: [styles["button-text"]],
                content: `New ${label}`
              }}
              onClick={openAddSubEntityModal}
            />
          )}
        </div>
      </fieldset>);
  }
}