import React from 'react';
import { Controller, FieldError, UseFormReturn } from 'react-hook-form';
import Select from 'react-select';

import { FormOptionType, PropertyShape } from 'types/form';
import FormInputContainer from './form-input-container';
import { selectorStyles } from 'ui/css/selector-style';

interface FormSelectorProps {
  field: PropertyShape;
  fieldOptions: FormOptionType[];
  form: UseFormReturn;
  options?: {
    disabled?: boolean;
  };
  styles?: {
    label?: string[],
  };
}

/**
 * This component renders a dropdown selector for a dependent form section within the form.
 * 
 * @param {PropertyShape} field The field name that will be assigned to the form state.
 * @param {FormOptionType[]} fieldOptions The list of field options for the selector.
 * @param {UseFormReturn} form A react-hook-form hook containing methods and state for managing the associated form.
 * @param {boolean} options.disabled Optional indicator if the field should be disabled. Defaults to false.
 * @param {string[]} styles.label Optional styles for the label element.
 */
export default function DependentFormSelector(props: Readonly<FormSelectorProps>) {
  return (
    <FormInputContainer
      field={props.field}
      error={props.form.formState.errors[props.field.fieldId] as FieldError}
      labelStyles={props.styles?.label}
    >
      <Controller
        name={props.field.fieldId}
        control={props.form.control}
        defaultValue={props.form.getValues(props.field.fieldId)}
        render={({ field: { value, onChange } }) => (
          <Select
            styles={selectorStyles}
            unstyled
            options={props.fieldOptions}
            value={props.fieldOptions.find(option => option.value === value)}
            onChange={(selectedOption) => onChange((selectedOption as FormOptionType).value)}
            isLoading={false}
            isMulti={false}
            isSearchable={true}
            isDisabled={props.options.disabled}
          />
        )}
      />
    </FormInputContainer>
  );
}