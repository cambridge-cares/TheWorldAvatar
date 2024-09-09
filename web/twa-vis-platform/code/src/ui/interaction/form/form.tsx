import React, { useEffect, useState } from 'react';
import { FieldValues, useForm, UseFormReturn } from 'react-hook-form';
import { usePathname } from 'next/navigation';

import { PathNames } from 'io/config/routes';
import { FormTemplate, ID_KEY, PROPERTY_GROUP_TYPE, PropertyGroup, PropertyShape, PropertyShapeOrGroup, TYPE_KEY, VALUE_KEY } from 'types/form';
import LoadingSpinner from 'ui/graphic/loader/spinner';
import { getAfterDelimiter } from 'utils/client-utils';
import { HttpResponse, addEntity, deleteEntity, getFormTemplate, updateEntity } from 'utils/server-actions';
import { FORM_STATES, initFormField } from './form-utils';
import FormFieldComponent from './field/form-field';
import FormSection from './section/form-section';
import { DependentFormSection } from './section/dependent-form-section';
import FormSchedule from './section/form-schedule';

interface FormComponentProps {
  formRef: React.MutableRefObject<HTMLFormElement>;
  entityType: string;
  formType: string;
  agentApi: string;
  setResponse: React.Dispatch<React.SetStateAction<HttpResponse>>;
  onSubmittingChange: (isSubmitting: boolean) => void;
}

/**
 * This component renders a dynamic form component that generates inputs based on its inputs.
 * 
 * @param { React.MutableRefObject<HTMLFormElement>} formRef Reference to the form element.
 * @param {string} entityType The type of entity.
 * @param {string} formType The type of submission. Valid inputs include add and update.
 * @param {string} agentApi The target agent endpoint for any registry related functionalities.
 * @param {React.Dispatch<React.SetStateAction<HttpResponse>>} setResponse A dispatch function for setting the response after submission.
 * @param onSubmittingChange A function to handle the changes needed when submission should occur.
 */
export function FormComponent(props: Readonly<FormComponentProps>) {
  const id: string = getAfterDelimiter(usePathname(), "/");
  const [formTemplate, setFormTemplate] = useState<FormTemplate>(null);
  const [shapeToFieldName, setShapeToFieldName] = useState<Map<string, string>>(new Map<string, string>());
  const disableAllInputs: boolean = props.formType === PathNames.REGISTRY || props.formType === PathNames.REGISTRY_DELETE;

  // Sets the default value with the requested function call
  const form: UseFormReturn = useForm({
    defaultValues: async (): Promise<FieldValues> => {
      // All forms will require an ID to be assigned
      const initialState: FieldValues = {
        formType: props.formType, // Store form type for easy access and reduce need to pass parameters to child
      };

      // Retrieve template from APIs
      let template: FormTemplate;
      // For add form, get a blank template
      if (props.formType == PathNames.REGISTRY_ADD) {
        template = await getFormTemplate(props.agentApi, props.entityType);
      } else {
        // For edit and view, get template with values
        template = await getFormTemplate(props.agentApi, props.entityType, id);
      }

      const updatedProperties: PropertyShapeOrGroup[] = template.property.map(field => {
        // Properties as part of a group
        if (field[TYPE_KEY].includes(PROPERTY_GROUP_TYPE)) {
          const fieldset: PropertyGroup = field as PropertyGroup;
          const properties: PropertyShape[] = fieldset.property.map(fieldProp => {
            // For any schedule related information, it should follow groupless properties
            if (fieldset.label[VALUE_KEY].includes("schedule")) {
              return initFormField(fieldProp, initialState, fieldProp.name[VALUE_KEY]);
            }
            // For property shapes with no node kind property
            // Add node shapes and their corresponding field name to the map to facilite parent dependencies links
            if (!fieldProp.nodeKind) {
              fieldProp.qualifiedValueShape?.map(nodeShape => {
                setShapeToFieldName(new Map(shapeToFieldName).set(nodeShape[ID_KEY], fieldProp.name[VALUE_KEY]));
              });
            }
            // Update and set property field ids to include their group name
            // Append field id with group name as prefix
            const fieldId: string = `${fieldset.label[VALUE_KEY]} ${fieldProp.name[VALUE_KEY]}`;
            return initFormField(fieldProp, initialState, fieldId);
          })
          // Update the property group with updated properties
          return {
            ...fieldset,
            property: properties,
          }
        } else {
          const fieldShape: PropertyShape = field as PropertyShape;
          // For property shapes with no node kind property
          // Add node shapes and their corresponding field name to the map to facilite parent dependencies links
          if (!fieldShape.nodeKind) {
            fieldShape.qualifiedValueShape?.map(nodeShape => {
              setShapeToFieldName(new Map(shapeToFieldName).set(nodeShape[ID_KEY], fieldShape.name[VALUE_KEY]));
            });
          }
          // For groupless properties, their field ID will be directly set without further parsing
          return initFormField(fieldShape, initialState, fieldShape.name[VALUE_KEY]);
        }
      });

      setFormTemplate({
        ...template,
        property: updatedProperties
      });
      return initialState;
    }
  });

  // A function to initiate the form submission process
  const onSubmit = form.handleSubmit(async (formData: FieldValues) => {
    let pendingResponse: HttpResponse;
    if (formData[FORM_STATES.RECURRENCE]) {
      formData = {
        ...formData,
        recurrence: `P${formData[FORM_STATES.RECURRENCE] * 7}D`,
      }
    }

    switch (props.formType.toLowerCase()) {
      case PathNames.REGISTRY_ADD: {
        pendingResponse = await addEntity(props.agentApi, formData, props.entityType);
        break;
      }
      case PathNames.REGISTRY_DELETE: {
        pendingResponse = await deleteEntity(props.agentApi, formData[FORM_STATES.ID], props.entityType);
        break;
      }
      case PathNames.REGISTRY_EDIT: {
        pendingResponse = await updateEntity(props.agentApi, formData, props.entityType);
        break;
      }
      default:
        break;
    }
    props.setResponse(pendingResponse);
  });

  useEffect(() => {
    props.onSubmittingChange(form.formState?.isSubmitting);
  }, [form.formState?.isSubmitting]);

  return (
    <form ref={props.formRef} onSubmit={onSubmit}>
      {form.formState.isLoading ?
        <LoadingSpinner isSmall={false} /> :
        formTemplate.property.map((field, index) => {
          if (field[TYPE_KEY].includes(PROPERTY_GROUP_TYPE)) {
            const fieldset: PropertyGroup = field as PropertyGroup;
            if (fieldset.label[VALUE_KEY].includes("schedule")) {
              return <FormSchedule
                key={fieldset[ID_KEY] + index}
                group={fieldset}
                form={form}
                options={{
                  disabled: disableAllInputs,
                }}
              />
            }
            return <FormSection
              key={fieldset[ID_KEY] + index}
              entityType={props.entityType}
              agentApi={props.agentApi}
              group={fieldset}
              form={form}
              options={{
                disabled: disableAllInputs,
              }}
            />
          } else {
            const fieldProp: PropertyShape = field as PropertyShape;
            const disableId: boolean = props.formType === PathNames.REGISTRY_EDIT && fieldProp.name[VALUE_KEY] === FORM_STATES.ID ? true : disableAllInputs;
            if (fieldProp.class) {
              return <DependentFormSection
                key={fieldProp.name[VALUE_KEY] + index}
                agentApi={props.agentApi}
                dependentProp={fieldProp}
                form={form}
                shapeToFieldMap={shapeToFieldName}
              />
            }
            return <FormFieldComponent
              key={fieldProp.name[VALUE_KEY] + index}
              entityType={props.entityType}
              agentApi={props.agentApi}
              field={fieldProp}
              form={form}
              options={{
                disabled: disableId,
              }}
            />
          }
        })}
    </form>
  );
}