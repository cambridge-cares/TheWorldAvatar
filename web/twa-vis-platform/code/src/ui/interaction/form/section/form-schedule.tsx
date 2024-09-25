import styles from '../form.module.css';
import fieldStyles from '../field/field.module.css';

import React, { useState } from 'react';
import { UseFormReturn } from 'react-hook-form';

import { PropertyGroup, VALUE_KEY } from 'types/form';
import { parseWordsForLabels } from 'utils/client-utils';
import FormCheckboxField from '../field/form-checkbox-field';
import FormDateTimePicker from '../field/form-date-time-picker';
import { FORM_STATES } from '../form-utils';

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
  const singleService: string = "Single Service";
  const regularService: string = "Regular Service";

  // Define the state to store the selected value
  const [selectedServiceOption, setSelectedServiceOption] = useState<string>(
    props.form.getValues(FORM_STATES.RECURRENCE) == 0 ? singleService : regularService
  );

  // Handle change event for the select input
  const handleServiceChange = (event: React.ChangeEvent<HTMLSelectElement>) => {
    if (event.target.value === singleService) {
      props.form.setValue(FORM_STATES.RECURRENCE, 0);
    } else {
      props.form.setValue(FORM_STATES.RECURRENCE, 1);
    }
    setSelectedServiceOption(event.target.value);
  };

  return (
    <fieldset className={styles["form-fieldset"]} style={{ marginBottom: "1rem" }}>
      <legend className={styles["form-fieldset-label"]}>
        {parseWordsForLabels(props.group.label[VALUE_KEY])}
      </legend>
      <div className={styles["form-fieldset-contents"]}>
        <div className={styles["schedule-occurrence-container"]}>
          <label className={fieldStyles["field-text"]} htmlFor="select-input">Service type: </label>
          <select
            id="select-input"
            value={selectedServiceOption}
            className={fieldStyles["selector"]}
            disabled={props.options?.disabled}
            onChange={handleServiceChange}
          >
            <option value={singleService}>{singleService}</option>
            <option value={regularService}>{regularService}</option>
          </select>
        </div>
        {selectedServiceOption === regularService && <div className={styles["schedule-occurrence-container"]}>
          <span className={fieldStyles["field-text"]}>Repeat every</span>
          <input
            id={FORM_STATES.RECURRENCE}
            type={"number"}
            className={`${styles["schedule-occurrence-input"]} ${props.options?.disabled && styles["field-disabled"]}`}
            step={"1"}
            readOnly={props.options?.disabled}
            aria-label={FORM_STATES.RECURRENCE}
            {...props.form.register(FORM_STATES.RECURRENCE)}
          />
          <span className={fieldStyles["field-text"]}>week</span>
          <br /><br />
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
        }
        <div className={styles["time-slot-container"]}>
          <h1 className={fieldStyles["field-text"]} style={{ margin: "0.5rem 0.75rem" }}>Time Slot</h1>
          <div className={styles["form-fieldset-contents"]}>
            <div className={fieldStyles["form-field-container"]}>
              <div className={fieldStyles["form-input-container"]}>
                <FormDateTimePicker
                  field={{
                    "@id": "string",
                    "@type": "http://www.w3.org/ns/shacl#PropertyShape",
                    name: { "@value": "from" },
                    fieldId: FORM_STATES.TIME_SLOT_START,
                    datatype: "time",
                    description: { "@value": "" },
                    order: 0,
                  }}
                  form={props.form}
                  options={props.options}
                  styles={{
                    label: [fieldStyles["form-input-label"]],
                  }}
                />
              </div>
            </div>
            <div className={fieldStyles["form-field-container"]}>
              <div className={fieldStyles["form-input-container"]}>
                <FormDateTimePicker
                  field={{
                    "@id": "string",
                    "@type": "http://www.w3.org/ns/shacl#PropertyShape",
                    name: { "@value": "to" },
                    fieldId: FORM_STATES.TIME_SLOT_END,
                    datatype: "time",
                    description: { "@value": "" },
                    order: 0,
                  }}
                  form={props.form}
                  options={props.options}
                  styles={{
                    label: [fieldStyles["form-input-label"]],
                  }}
                />
              </div>
            </div>
          </div>
        </div>
      </div>
    </fieldset>);
}