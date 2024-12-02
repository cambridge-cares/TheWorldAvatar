import React, { useState } from 'react';
import { useDispatch } from 'react-redux';
import { FieldValues, useForm, UseFormReturn } from 'react-hook-form';
import { usePathname } from 'next/navigation';

import { Paths } from 'io/config/routes';
import { setFilterFeatureIris, setFilterTimes } from 'state/map-feature-slice';
import { FormTemplate, ID_KEY, PROPERTY_GROUP_TYPE, PropertyGroup, PropertyShape, PropertyShapeOrGroup, SEARCH_FORM_TYPE, TYPE_KEY, VALUE_KEY } from 'types/form';
import LoadingSpinner from 'ui/graphic/loader/spinner';
import { getAfterDelimiter } from 'utils/client-utils';
import { HttpResponse, addEntity, deleteEntity, getFormTemplate, getMatchingInstances, updateEntity } from 'utils/server-actions';
import { FORM_STATES, initFormField } from './form-utils';
import FormFieldComponent from './field/form-field';
import FormSection from './section/form-section';
import { DependentFormSection } from './section/dependent-form-section';
import FormSchedule, { daysOfWeek } from './section/form-schedule';
import FormSearchPeriod from './section/form-search-period';
import FormGeocoder from './section/form-geocoder';

interface FormComponentProps {
  formRef: React.MutableRefObject<HTMLFormElement>;
  entityType: string;
  formType: string;
  agentApi: string;
  setResponse: React.Dispatch<React.SetStateAction<HttpResponse>>;
  primaryInstance?: string;
  isPrimaryEntity?: boolean;
}

/**
 * This component renders a dynamic form component that generates inputs based on its inputs.
 * 
 * @param { React.MutableRefObject<HTMLFormElement>} formRef Reference to the form element.
 * @param {string} entityType The type of entity.
 * @param {string} formType The type of submission. Valid inputs include add and update.
 * @param {string} agentApi The target agent endpoint for any registry related functionalities.
 * @param {React.Dispatch<React.SetStateAction<HttpResponse>>} setResponse A dispatch function for setting the response after submission.
 * @param {string} primaryInstance An optional instance for the primary entity.
 * @param {boolean} isPrimaryEntity An optional indicator if the form is targeting a primary entity.
 */
export function FormComponent(props: Readonly<FormComponentProps>) {
  const id: string = getAfterDelimiter(usePathname(), "/");
  const dispatch = useDispatch();
  const [formTemplate, setFormTemplate] = useState<FormTemplate>(null);
  const [shapeToFieldName, setShapeToFieldName] = useState<Map<string, string>>(new Map<string, string>());
  const disableAllInputs: boolean = props.formType === Paths.REGISTRY || props.formType === Paths.REGISTRY_DELETE;

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
      if (props.formType == Paths.REGISTRY_ADD || props.formType == SEARCH_FORM_TYPE) {
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
    // For single service
    if (formData[FORM_STATES.RECURRENCE] == 0) {
      const startDate: string = formData[FORM_STATES.START_DATE];
      const dateObject: Date = new Date(startDate);
      const dayOfWeek = daysOfWeek[dateObject.getUTCDay()];

      formData = {
        ...formData,
        recurrence: "P1D",
        "end date": startDate, // End date must correspond to start date
        [dayOfWeek]: true, // Ensure the corresponding day of week is true
      }
      // For alternate day service
    } else if (formData[FORM_STATES.RECURRENCE] == -1) {
      formData = {
        ...formData,
        recurrence: "P2D",
      }
      // For regular service
    } else if (formData[FORM_STATES.RECURRENCE]) {
      formData = {
        ...formData,
        recurrence: `P${formData[FORM_STATES.RECURRENCE] * 7}D`,
      }
    }

    switch (props.formType.toLowerCase()) {
      case Paths.REGISTRY_ADD: {
        pendingResponse = await addEntity(props.agentApi, formData, props.entityType);
        // For registry's primary entity, a draft lifecycle must also be generated
        if (props.isPrimaryEntity && pendingResponse.success) {
          pendingResponse = await addEntity(props.agentApi, {
            contract: pendingResponse.iri,
            ...formData
          }, "contracts/draft");
        }
        break;
      }
      case Paths.REGISTRY_DELETE: {
        pendingResponse = await deleteEntity(props.agentApi, formData[FORM_STATES.ID], props.entityType);
        break;
      }
      case Paths.REGISTRY_EDIT: {
        if (props.isPrimaryEntity) {
          const reqBody: string = JSON.stringify({
            ...formData,
            contract: props.primaryInstance,
          });
          pendingResponse = await updateEntity(`${props.agentApi}/contracts/draft`, reqBody);
        } else {
          const reqBody: string = JSON.stringify({
            ...formData,
            entity: props.entityType,
          });
          pendingResponse = await updateEntity(`${props.agentApi}/${props.entityType}/${formData.id}`, reqBody);
        }
        break;
      }
      case SEARCH_FORM_TYPE: {
        // For interacting with min and max fields in the search form
        Object.keys(formData).forEach(field => {
          // Append range key to field if they have min and max fields and values in either their min or max field
          // eslint-disable-next-line no-prototype-builtins
          if (formData.hasOwnProperty(`min ${field}`) && formData.hasOwnProperty(`max ${field}`) &&
            (formData[`min ${field}`] != undefined || formData[`max ${field}`] != undefined)) {
            formData = {
              ...formData,
              [field]: "range",
            }
          }
        });

        pendingResponse = await getMatchingInstances(props.agentApi, props.entityType, formData);
        if (pendingResponse.success) {
          if (pendingResponse.message === "[]") {
            pendingResponse.success = false;
            pendingResponse.message = "No matching feature found! Please refine your search parameters.";
          } else {
            dispatch(setFilterFeatureIris(JSON.parse(pendingResponse.message)));
            pendingResponse.message = "Found matching features! Updating the visualisation...";
          }
          if (formData[FORM_STATES.START_TIME_PERIOD] && formData[FORM_STATES.END_TIME_PERIOD]) {
            // Only display this message if there is no features based on static meta data but the search period is required
            if (!pendingResponse.success) {
              pendingResponse.success = true;
              pendingResponse.message = "No matching feature found! Attempting to display all features within the selected time period...";
            }
            // Convert date to UNIX Epoch Timestamp
            const startTime: number = Math.floor(new Date(formData[FORM_STATES.START_TIME_PERIOD]).getTime() / 1000);
            const endTime: number = Math.floor(new Date(formData[FORM_STATES.END_TIME_PERIOD]).getTime() / 1000);
            dispatch(setFilterTimes([startTime, endTime]));
          }
        }
        break;
      }
      default:
        break;
    }
    props.setResponse(pendingResponse);
  });

  return (
    <form ref={props.formRef} onSubmit={onSubmit}>
      {form.formState.isLoading ?
        <LoadingSpinner isSmall={false} /> :
        formTemplate.property.map((field, index) => {
          if (field[TYPE_KEY].includes(PROPERTY_GROUP_TYPE)) {
            const fieldset: PropertyGroup = field as PropertyGroup;
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
            // If this is a hidden field, hide the field
            if (fieldProp.maxCount && parseInt(fieldProp.maxCount[VALUE_KEY]) === 0) {
              return <></>;
            }
            const disableId: boolean = props.formType === Paths.REGISTRY_EDIT && fieldProp.name[VALUE_KEY] === FORM_STATES.ID ? true : disableAllInputs;
            if (fieldProp.class) {
              if (fieldProp.class[ID_KEY] === "https://spec.edmcouncil.org/fibo/ontology/FND/DatesAndTimes/FinancialDates/RegularSchedule") {
                return <FormSchedule
                  key={fieldProp.name[VALUE_KEY] + index}
                  fieldId={fieldProp.name[VALUE_KEY]}
                  agentApi={props.agentApi}
                  form={form}
                  options={{
                    disabled: disableAllInputs,
                  }}
                />
              }
              if (fieldProp.class[ID_KEY] === "https://spec.edmcouncil.org/fibo/ontology/FND/Places/Locations/PhysicalLocation") {
                return <FormGeocoder
                  key={fieldProp.name[VALUE_KEY] + index}
                  agentApi={props.agentApi}
                  field={fieldProp}
                  form={form}
                />;
              }
              if (props.formType === SEARCH_FORM_TYPE && fieldProp.class[ID_KEY] === "https://www.theworldavatar.com/kg/ontotimeseries/TimeSeries") {
                return <FormSearchPeriod
                  key={fieldProp.name[VALUE_KEY] + index}
                  form={form}
                />;
              }
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