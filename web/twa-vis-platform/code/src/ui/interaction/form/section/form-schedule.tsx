import styles from '../form.module.css';
import fieldStyles from '../field/field.module.css';

import React from 'react';
import { UseFormReturn } from 'react-hook-form';

import { PropertyGroup, VALUE_KEY } from 'types/form';
import { parseWordsForLabels } from 'utils/client-utils';
import FormCheckboxField from '../field/form-checkbox-field';
import { FORM_STATES, getRegisterOptions } from '../form-utils';

interface FormScheduleProps {
  group: PropertyGroup;
  form: UseFormReturn;
  options?: {
    disabled?: boolean;
  };
}
/**
 * This component renders a form schedule as a form section.
 * 
 * @param {PropertyGroup} group Fieldset group model.
 * @param {UseFormReturn} form A react-hook-form hook containing methods and state for managing the associated form.
 * @param {boolean} options.disabled Optional indicator if the fields should be disabled. Defaults to false.
 */
export default function FormSchedule(props: Readonly<FormScheduleProps>) {
  const daysOfWeek: string[] = [FORM_STATES.SUN, FORM_STATES.MON, FORM_STATES.TUES, FORM_STATES.WED, FORM_STATES.THURS, FORM_STATES.FRI, FORM_STATES.SAT];
  const daysOfWeekLabel: string[] = ["Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat"];

  return (
    <fieldset className={styles["form-fieldset"]} style={{ marginBottom: "1rem" }}>
      <legend className={styles["form-fieldset-label"]}>
        {parseWordsForLabels(props.group.label[VALUE_KEY])}
      </legend>
      <div className={styles["form-fieldset-contents"]}>
        <div className={styles["schedule-occurrence-container"]}>
          <span className={fieldStyles["field-text"]}>Repeat every</span>
          <input
            id={FORM_STATES.RECURRENCE}
            type={"number"}
            className={`${styles["schedule-occurrence-input"]} ${props.options?.disabled && styles["field-disabled"]}`}
            step={"1"}
            readOnly={props.options?.disabled}
            aria-label={FORM_STATES.RECURRENCE}
            {...props.form.register(FORM_STATES.RECURRENCE, getRegisterOptions(
              true,
            ))}
          />
          <span className={fieldStyles["field-text"]}>week</span>
        </div>
        <div className={styles["schedule-day-container"]}>
          {daysOfWeek.map((dayOfWeek, index) => {
            return <div
              key={dayOfWeek + index}
              className={fieldStyles["form-input-container"]}
            >
              <FormCheckboxField
                field={dayOfWeek}
                label={daysOfWeekLabel[index]}
                form={props.form}
                options={props.options}
              />
            </div>
          })}
        </div>
      </div>
    </fieldset>);
}