import styles from '../form.module.css';
import fieldStyles from '../field/field.module.css';

import React, { useEffect, useMemo, useState } from 'react';
import { FieldValues, UseFormReturn } from 'react-hook-form';
import { useRouter } from 'next/navigation';

import { PathNames } from 'io/config/routes';
import { FormTemplate, ID_KEY, PROPERTY_GROUP_TYPE, PropertyGroup, PropertyShape, TYPE_KEY, VALUE_KEY } from 'types/form';
import MaterialIconButton from 'ui/graphic/icon/icon-button';
import LoadingSpinner from 'ui/graphic/loader/spinner';
import { parseWordsForLabels } from 'utils/client-utils';
import { getData, getFormTemplate } from 'utils/server-actions';
import { FORM_STATES } from '../form-utils';
import DependentFormSelector from '../field/dependent-form-selector';

interface DependentFormSectionProps {
  agentApi: string;
  dependentProp: PropertyShape;
  form: UseFormReturn;
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
  const parentId: string = `parent ${props.dependentProp.fieldId}`;
  const parentEntityLabel: string = props.dependentProp.qualifiedValueShape?.name[VALUE_KEY] ?? null;

  const currentParentOption: string = props.form.watch(parentId);
  const currentOption: string = props.form.watch(props.dependentProp.fieldId);

  const [isFetching, setIsFetching] = useState<boolean>(true);
  const [parentSelectElements, setParentSelectElements] = useState<FieldValues[]>(null);
  const [selectElements, setSelectElements] = useState<FieldValues[]>([]);
  const [depFields, setDepFields] = useState<FieldValues[]>([]);

  // Retrieve the query entity type based on the class input
  const getQueryEntityType = (entityClass: string): string => {
    if (entityClass) {
      if (entityClass.includes("Client") || entityClass.includes("Employer")) {
        return "client";
      } else if (entityClass.includes("ServiceProvider")) {
        return "serviceprovider";
      } else if (entityClass.includes("Employee")) {
        return "employee";
      }
    }
    return "";
  };

  const parentEntity: string = getQueryEntityType(props.dependentProp.qualifiedValueShape?.targetClass[ID_KEY]);

  // Cache the result to reduce rerender calls
  const queryEntityType: string = useMemo(
    () => getQueryEntityType(props.dependentProp.class[ID_KEY]),
    [props.dependentProp.class[ID_KEY]]
  );

  // A hook that fetches parent entities on first render if available
  useEffect(() => {
    // Declare an async function to retrieve any parent entity if available for the dropdown selector
    const getParentDependencies = async (field: PropertyShape, form: UseFormReturn) => {
      let parentEntities: FieldValues[] = [];
      if (field.qualifiedValueShape) {
        if (formType === PathNames.REGISTRY || formType === PathNames.REGISTRY_DELETE) {
          // Retrieve only one entity to reduce query times especially as users cannot edit anything in view mode
          parentEntities = await getData(props.agentApi, parentEntity, field.qualifiedValueShape.defaultValue.split("/").pop());
        } else {
          parentEntities = await getData(props.agentApi, parentEntity);
        }
      }

      // Create a new form field for the parent entity
      form.setValue(parentId, field.qualifiedValueShape?.defaultValue ? field.qualifiedValueShape.defaultValue.split("/").pop() : parentEntities[0]?.id);
      // Update dropdown options
      setParentSelectElements(parentEntities);
    }
    getParentDependencies(props.dependentProp, props.form);
  }, []);

  // A hook that fetches the list of dependent entities for the dropdown selector 
  // once parent elements are queried and/or users change the currently select parent option
  useEffect(() => {
    // Declare an async function to retrieve the list of dependent entities for the dropdown selector
    const getDependencies = async (entityType: string, field: PropertyShape, form: UseFormReturn) => {
      setIsFetching(true);
      let entities: FieldValues[] = [];
      // If there is supposed to be a parent element, retrieve the data associated with the selected parent option
      // If there is no valid parent option, there should be no entity

      if (field.qualifiedValueShape) {
        if (currentParentOption) {
          entities = await getData(props.agentApi, parentEntity, currentParentOption, entityType);
        }
      } else if (formType === PathNames.REGISTRY || formType === PathNames.REGISTRY_DELETE) {
        // Retrieve only one entity to reduce query times especially as users cannot edit anything in view mode
        entities = await getData(props.agentApi, entityType, field.defaultValue.split("/").pop());
      } else {
        entities = await getData(props.agentApi, entityType);
      }

      // Set the form value to the default value if available, else, default to the first option
      form.setValue(field.fieldId, field.defaultValue ? field.defaultValue.split("/").pop() : entities[0]?.id);

      // Update dropdown options
      setSelectElements(entities);
      setDepFields([]);
      setIsFetching(false);
    }

    if (parentSelectElements) {
      getDependencies(queryEntityType, props.dependentProp, props.form);
    }
  }, [parentSelectElements, currentParentOption]);

  // A hook that fetches the dependent fields based on the currently selected option
  useEffect(() => {
    // Declare an async function to retrieve the metadata for the target dependent entity
    const getDependentFields = async (entityType: string, id: string) => {
      setIsFetching(true);
      // MUST retrieve a form template for the target ID
      const template: FormTemplate = await getFormTemplate(props.agentApi, entityType, id);
      // Iterate over the template to populate the dependent fields based on the available properties
      const dependentFields: FieldValues[] = []
      template.property.map(currentProp => {
        if (currentProp[TYPE_KEY].includes(PROPERTY_GROUP_TYPE)) {
          // For any groups, their label must include the group name
          const propGroup: PropertyGroup = currentProp as PropertyGroup;
          propGroup.property.map(currentSubProp => {
            const fieldName: string = currentSubProp.name[VALUE_KEY];
            // IDs and parent entity should not be included
            if (![FORM_STATES.ID, parentEntityLabel].includes(fieldName)) {
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
          if (![FORM_STATES.ID, parentEntityLabel].includes(fieldName)) {
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
    if (currentOption && !isFetching) {
      getDependentFields(queryEntityType, currentOption);
    }
  }, [currentOption]);

  // An event handler that will navigate to the required add form when clicked
  const openAddSubEntityModal = () => {
    let url: string = `../add/${queryEntityType}`;
    if (formType != PathNames.REGISTRY_ADD) {
      url = `../../add/${queryEntityType}`;
    }
    router.push(url);
  };

  return (
    <fieldset className={styles["form-fieldset"]}>
      <legend className={styles["form-fieldset-label"]}>{parseWordsForLabels(label)}</legend>
      {isFetching &&
        <div className={styles["loader-container"]}>
          <LoadingSpinner isSmall={true} />
        </div>
      }
      {/** Parent entity selection should only appear if there are any valid parent option or elements */}
      {!isFetching && parentSelectElements.length > 0 && (formType != PathNames.REGISTRY || formType === PathNames.REGISTRY_DELETE) && (
        <div className={fieldStyles["form-field-container"]}>
          <div className={fieldStyles["form-input-container"]}>
            <label className={fieldStyles["form-input-label"]} htmlFor={parentId}>
              <span className={fieldStyles["field-text"]}>
                {`Select ${parentEntityLabel}`}
              </span>
            </label>
            <DependentFormSelector
              field={{
                ...props.dependentProp,
                fieldId: parentId,
              }}
              form={props.form}
              selectOptions={parentSelectElements.map(entity => entity.id)}
              selectLabels={parentSelectElements.map(entity => entity.name)}
            />
          </div>
        </div>
      )}
      {!isFetching && (formType != PathNames.REGISTRY || formType === PathNames.REGISTRY_DELETE) && (
        <div className={fieldStyles["form-field-container"]}>
          <div className={fieldStyles["form-input-container"]}>
            {parentSelectElements.length > 0 && selectElements.length > 0 && <label className={fieldStyles["form-input-label"]} htmlFor={props.dependentProp.fieldId}>
              <span className={fieldStyles["field-text"]}>
                {parseWordsForLabels(label)}
              </span>
            </label>}
            {selectElements.length > 0 ?
              <DependentFormSelector
                field={props.dependentProp}
                form={props.form}
                selectOptions={selectElements.map(entity => entity.id)}
                selectLabels={selectElements.map(entity => entity.name)}
              /> :
              <p className={styles["button-text"]}>No {label} detected</p>
            }
          </div>
          <MaterialIconButton
            iconName={"add"}
            className={styles["button"] + " " + styles["button-layout"]}
            text={{
              styles: [styles["button-text"]],
              content: `New ${label}`
            }}
            onClick={openAddSubEntityModal}
          />
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