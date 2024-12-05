import React from 'react';
import { FieldValues, SubmitHandler, useForm, UseFormReturn } from 'react-hook-form';

import { Paths } from 'io/config/routes';
import { PropertyShape, VALUE_KEY } from 'types/form';
import LoadingSpinner from 'ui/graphic/loader/spinner';
import { initFormField } from '../form-utils';
import FormFieldComponent from '../field/form-field';

interface FormComponentProps {
  agentApi: string;
  entityType: string;
  formRef: React.MutableRefObject<HTMLFormElement>;
  fields: PropertyShape[];
  submitAction: SubmitHandler<FieldValues>;
}

/**
 * This component renders a simple form template with only field inputs.
 * 
 * @param {string} agentApi The target agent endpoint for any registry related functionalities.
 * @param {string} entityType The type of entity.
 * @param {React.MutableRefObject<HTMLFormElement>} formRef Reference to the form element.
 * @param {PropertyShape[]} fields The fields to render.
 * @param {SubmitHandler<FieldValues>} submitAction Action to be taken when submitting the form.
 */
export function FormTemplate(props: Readonly<FormComponentProps>) {
  // Sets the default value with the requested function call if any
  const form: UseFormReturn = useForm({
    defaultValues: async (): Promise<FieldValues> => {
      // All forms will require an ID to be assigned
      const initialState: FieldValues = {
        formType: Paths.REGISTRY_ADD, // DEFAULT TO ADD TYPE
      };
      props.fields.map(field => initFormField(field, initialState, field.name[VALUE_KEY]));
      return initialState;
    }
  });

  return (
    <form ref={props.formRef} onSubmit={form.handleSubmit(props.submitAction)}>
      {form.formState.isLoading ?
        <LoadingSpinner isSmall={false} /> :
        props.fields.map((field, index) => {
          return <FormFieldComponent
            key={field.name[VALUE_KEY] + index}
            entityType={props.entityType}
            agentApi={props.agentApi}
            field={field}
            form={form}
          />
        })}
    </form>
  );
}