import styles from '../form.module.css';
import fieldStyles from '../field/field.module.css';

import React, { useEffect, useMemo, useState } from 'react';
import { Control, FieldValues, UseFormReturn, useWatch } from 'react-hook-form';
import { useRouter } from 'next/navigation';

import { PathNames } from 'io/config/routes';
import { FormTemplate, ID_KEY, PROPERTY_GROUP_TYPE, PropertyGroup, PropertyShape, TYPE_KEY, VALUE_KEY } from 'types/form';
import MaterialIconButton from 'ui/graphic/icon/icon-button';
import LoadingSpinner from 'ui/graphic/loader/spinner';
import { getAfterDelimiter, isValidIRI, parseWordsForLabels } from 'utils/client-utils';
import { getData, getFormTemplate } from 'utils/server-actions';
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
 * @param {string} agentApi The target endpoint for contacting the backend agent.
 * @param {PropertyShape} dependentProp The dependent property's SHACL restrictions.
 * @param {UseFormReturn} form A react-hook-form hook containing methods and state for managing the associated form.
 */
export function DependentFormSection(props: Readonly<DependentFormSectionProps>) {
  const router = useRouter();
  const label: string = props.dependentProp.name[VALUE_KEY];
  const formType: string = props.form.getValues(FORM_STATES.FORM_TYPE);
  const control: Control = props.form.control;
  const [isFetching, setIsFetching] = useState<boolean>(true);
  const [selectElements, setSelectElements] = useState<FieldValues[]>([]);
  const [depFields, setDepFields] = useState<FieldValues[]>([]);

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

  // Retrieve the query entity type based on the class input
  const getQueryEntityType = (entityClass: string): string => {
    if (entityClass) {
      if (entityClass.endsWith("FormalOrganization")) {
        return "client";
      } else if (entityClass.endsWith("Facility")) {
        return "facility";
      } else if (entityClass.endsWith("ServiceProvider")) {
        return "serviceprovider";
      } else if (entityClass.endsWith("Employee")) {
        return "employee";
      }
    }
    return "";
  };

  // Cache the result to reduce rerender calls
  const queryEntityType: string = useMemo(
    () => getQueryEntityType(props.dependentProp.class[ID_KEY]),
    [props.dependentProp.class[ID_KEY]]
  );

  // A hook that fetches the list of dependent entities for the dropdown selector
  // If parent options are available, the list will be refetched on parent option change
  useEffect(() => {
    // Declare an async function to retrieve the list of dependent entities for the dropdown selector
    const getDependencies = async (entityType: string, field: PropertyShape, form: UseFormReturn) => {
      setIsFetching(true);
      let entities: FieldValues[] = [];
      // If there is supposed to be a parent element, retrieve the data associated with the selected parent option
      if (field.nodeKind) {
        if (currentParentOption) {
          entities = await getData(props.agentApi, parentField, getAfterDelimiter(currentParentOption, "/"), entityType);
        }
        // If there is no valid parent option, there should be no entity
      } else if (formType === PathNames.REGISTRY || formType === PathNames.REGISTRY_DELETE) {
        // Retrieve only one entity to reduce query times as users cannot edit anything in view or delete mode
        entities = await getData(props.agentApi, entityType, getAfterDelimiter(field.defaultValue, "/"));
      } else {
        entities = await getData(props.agentApi, entityType);
      }

      // Set the form value to the default value if available, else, default to the first option
      form.setValue(field.fieldId, field.defaultValue ? field.defaultValue : entities[0]?.id);

      // Update dropdown options
      setSelectElements(entities);
      setDepFields([]);
      setIsFetching(false);
    }

    getDependencies(queryEntityType, props.dependentProp, props.form);
  }, [currentParentOption]);

  // A hook that fetches the dependent fields based on the currently selected option
  useEffect(() => {
    // Declare an async function to retrieve the metadata for the target dependent entity
    const getDependentFields = async (entityType: string, id: string) => {
      setIsFetching(true);

      // MUST retrieve a form template for the target ID
      const template: FormTemplate = await getFormTemplate(props.agentApi, entityType, getAfterDelimiter(id, "/"));
      // Iterate over the template to populate the dependent fields based on the available properties
      const dependentFields: FieldValues[] = []
      if (template)
        template.property.map(currentProp => {
          if (currentProp[TYPE_KEY].includes(PROPERTY_GROUP_TYPE)) {
            // For any groups, their label must include the group name
            const propGroup: PropertyGroup = currentProp as PropertyGroup;
            propGroup.property.map(currentSubProp => {
              const fieldName: string = currentSubProp.name[VALUE_KEY];
              // IDs and parent entity should not be included
              if (![FORM_STATES.ID].includes(fieldName) && !isValidIRI(currentSubProp.defaultValue)) {
                dependentFields.push({
                  label: parseWordsForLabels(`${propGroup.label[VALUE_KEY]} ${fieldName}`),
                  value: currentSubProp.defaultValue,
                });
              }
            })
          } else {
            // For any non-grouped properties
            const fieldProp: PropertyShape = currentProp as PropertyShape;
            const fieldName: string = fieldProp.name[VALUE_KEY];
            // IDs and parent entity should not be included
            if (![FORM_STATES.ID].includes(fieldName) && !isValidIRI(fieldProp.defaultValue)) {
              dependentFields.push({
                label: parseWordsForLabels(fieldName),
                value: fieldProp.defaultValue,
              });
            }
          }
        });
      setDepFields(dependentFields);
      setIsFetching(false);
    }

    // If any fetching is still in progress, this should not be re-executed
    if (currentOption) {
      getDependentFields(queryEntityType, currentOption);
    }
  }, [currentOption]);

  // An event handler that will navigate to the required add form when clicked
  const openAddSubEntityModal = () => {
    let url: string = `../add/${queryEntityType}`;
    if (formType != PathNames.REGISTRY_ADD) {
      url = `../${url}`;
    }
    router.push(url);
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
        {!isFetching && (
          <div className={fieldStyles["form-field-container"]}>
            <div className={fieldStyles["form-input-container"]}>
              {selectElements.length > 0 && <label className={fieldStyles["form-input-label"]} htmlFor={props.dependentProp.fieldId}>
                <span className={fieldStyles["field-text"]}>
                  {parseWordsForLabels(label)}
                </span>
              </label>}
              {(formType != PathNames.REGISTRY && formType != PathNames.REGISTRY_DELETE) &&
                (selectElements.length > 0 ?
                  <DependentFormSelector
                    field={props.dependentProp}
                    form={props.form}
                    selectOptions={selectElements.map(entity => entity.id)}
                    selectLabels={selectElements.map(entity => entity.name ?? entity.first_name)}
                  /> :
                  <p className={styles["button-text"]}>No {label} detected</p>
                )
              }
            </div>
            {(formType != PathNames.REGISTRY && formType != PathNames.REGISTRY_DELETE) && (
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
        )}
        {!isFetching && depFields[0]?.value && (
          <div className={styles["form-dependent-field-container"]}>
            {depFields.map((depField, index) => {
              return <div className={styles["form-dependent-field-line"]} key={`${depField.label} ${index}`}>
                <div className={styles["form-dependent-field-label"]} >
                  {`${depField.label}:`}
                </div>
                <div className={styles["form-dependent-field-value"]} >
                  {depField.value}
                </div>
              </div>
            })}
          </div>
        )
        }
      </fieldset>);
  }
}