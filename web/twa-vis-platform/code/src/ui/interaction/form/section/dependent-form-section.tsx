import styles from '../form.module.css';
import fieldStyles from '../field/field.module.css';

import React, { useEffect, useMemo, useState } from 'react';
import { Control, FieldValues, UseFormReturn, useWatch } from 'react-hook-form';
import { useRouter } from 'next/navigation';

import { PathNames } from 'io/config/routes';
import { ID_KEY, PropertyShape, VALUE_KEY } from 'types/form';
import MaterialIconButton from 'ui/graphic/icon/icon-button';
import LoadingSpinner from 'ui/graphic/loader/spinner';
import { getAfterDelimiter, parseWordsForLabels } from 'utils/client-utils';
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

      // By default, use the first option's id
      let defaultId: string = entities[0]?.id;
      // If there is a default value, search and use the option matching the default instance's local name
      if (field.defaultValue) {
        const defaultValueId: string = getAfterDelimiter(field.defaultValue, "/");
        defaultId = entities.find(entity => getAfterDelimiter(entity.id, "/") === defaultValueId).id;
      }
      // Set the form value to the default value if available, else, default to the first option
      form.setValue(field.fieldId, defaultId);

      // Update dropdown options
      setSelectElements(entities);
      setIsFetching(false);
    }

    getDependencies(queryEntityType, props.dependentProp, props.form);
  }, [currentParentOption]);

  // An event handler that will navigate to the required add form when clicked
  const openAddSubEntityModal = () => {
    let url: string = `../add/${queryEntityType}`;
    if (formType != PathNames.REGISTRY_ADD) {
      url = `../${url}`;
    }
    router.push(url);
  };

  // An event handler that will navigate to the required view form when clicked
  const openViewSubEntityModal = () => {
    let url: string = `../view/${queryEntityType}/${getAfterDelimiter(currentOption, "/")}`;
    // Other form types will have an extra path for the entity id, except for ADD
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
              {selectElements.length > 0 ?
                <DependentFormSelector
                  field={props.dependentProp}
                  form={props.form}
                  selectOptions={selectElements.map(entity => entity.id)}
                  selectLabels={selectElements.map(entity => entity.name ?? entity.first_name)}
                  options={{
                    disabled: formType == PathNames.REGISTRY || formType == PathNames.REGISTRY_DELETE
                  }}
                /> :
                <p className={styles["button-text"]}>No {label} detected</p>
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
        {!isFetching && selectElements.length > 0 && <MaterialIconButton
          iconName={"expand_circle_right"}
          className={styles["button"] + " " + styles["button-layout"]}
          text={{
            styles: [styles["button-text"]],
            content: `View more details`
          }}
          onClick={openViewSubEntityModal}
        />
        }
      </fieldset>);
  }
}