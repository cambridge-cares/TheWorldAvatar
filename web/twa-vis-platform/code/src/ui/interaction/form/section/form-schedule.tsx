import styles from '../form.module.css';
import fieldStyles from '../field/field.module.css';

import React, { useMemo, useState } from 'react';
import { UseFormReturn } from 'react-hook-form';
import Select from 'react-select';

import { FormOptionType, SEARCH_FORM_TYPE } from 'types/form';
import { selectorStyles } from 'ui/css/selector-style';
import { parseWordsForLabels } from 'utils/client-utils';
import FormCheckboxField from '../field/form-checkbox-field';
import { FORM_STATES } from '../form-utils';
import FormFieldComponent from '../field/form-field';
import { Paths } from 'io/config/routes';

interface FormScheduleProps {
  fieldId: string;
  form: UseFormReturn;
  options?: {
    disabled?: boolean;
  };
}

export const daysOfWeek: string[] = [FORM_STATES.SUN, FORM_STATES.MON, FORM_STATES.TUES, FORM_STATES.WED, FORM_STATES.THURS, FORM_STATES.FRI, FORM_STATES.SAT];

/**
 * This component renders a form schedule as a form section.
 * 
 * @param {string} fieldId Field name.
 * @param {UseFormReturn} form A react-hook-form hook containing methods and state for managing the associated form.
 * @param {boolean} options.disabled Optional indicator if the fields should be disabled. Defaults to false.
 */
export default function FormSchedule(props: Readonly<FormScheduleProps>) {
  const formType: string = props.form.getValues(FORM_STATES.FORM_TYPE);
  const daysOfWeekLabel: string[] = ["Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat"];
  const singleService: string = "Single Service";
  const regularService: string = "Regular Service";
  const alternateService: string = "Alternate Day Service";
  const scheduleType: string = "schedule";

  // Define the state to store the selected value
  const [selectedServiceOption, setSelectedServiceOption] = useState<string>(
    props.form.getValues(FORM_STATES.RECURRENCE) == 0 ? singleService :
      props.form.getValues(FORM_STATES.RECURRENCE) == -1 ? alternateService : regularService
  );

  if (formType == Paths.REGISTRY_ADD || formType == SEARCH_FORM_TYPE) {
    props.form.setValue(FORM_STATES.RECURRENCE, 1);
  }

  // Updates the service description whenever the service option changes
  const serviceDescription = useMemo((): string => {
    if (selectedServiceOption === singleService) {
      return "A one-off service that will occur once on the specified date.";
    } else if (selectedServiceOption === alternateService) {
      return "A service that will occur on every alternate day within the specified period.";
    } else {
      return "A service that will occur regularly based on the schedule within the specified period.";
    }
  }, [selectedServiceOption]);

  // Handle change event for the select input
  const handleServiceChange = (value: string) => {
    if (value === singleService) {
      props.form.setValue(FORM_STATES.RECURRENCE, 0);
    } else if (value === alternateService) {
      props.form.setValue(FORM_STATES.RECURRENCE, -1);
    } else {
      props.form.setValue(FORM_STATES.RECURRENCE, 1);
    }
    setSelectedServiceOption(value);
  };

  return (
    <fieldset className={styles["form-fieldset"]} style={{ marginBottom: "1rem" }}>
      <legend className={styles["form-fieldset-label"]}>
        {parseWordsForLabels(props.fieldId)}
      </legend>
      <div className={styles["form-fieldset-contents"]}>
        <div className={styles["schedule-occurrence-container"]}>
          <label className={fieldStyles["field-text"]} htmlFor="select-input">Service type:</label>
          <Select
            styles={selectorStyles}
            unstyled
            options={[{ label: singleService, value: singleService }, { label: regularService, value: regularService }, { label: alternateService, value: alternateService }]}
            value={{ label: selectedServiceOption, value: selectedServiceOption }}
            onChange={(selectedOption) => handleServiceChange((selectedOption as FormOptionType).value)}
            isLoading={false}
            isMulti={false}
            isSearchable={true}
          />
          <p className={fieldStyles["info-text"]}>
            <b className={fieldStyles["field-text"]}>Description: </b>
            {serviceDescription}
          </p>
        </div>
        <FormFieldComponent
          entityType={scheduleType}
          field={{
            "@id": "string",
            "@type": "http://www.w3.org/ns/shacl#PropertyShape",
            name: { "@value": FORM_STATES.START_DATE },
            fieldId: FORM_STATES.START_DATE,
            datatype: "date",
            description: { "@value": "Effective start date of service" },
            order: 0,
          }}
          form={props.form}
          options={props.options}
        />
        {selectedServiceOption != singleService && <FormFieldComponent
          entityType={scheduleType}
          field={{
            "@id": "string",
            "@type": "http://www.w3.org/ns/shacl#PropertyShape",
            name: { "@value": FORM_STATES.END_DATE },
            fieldId: FORM_STATES.END_DATE,
            datatype: "date",
            description: { "@value": "Effective end date of service" },
            order: 0,
          }}
          form={props.form}
          options={props.options}
        />}
        {selectedServiceOption === regularService && <div className={styles["schedule-occurrence-container"]}>
          <span className={fieldStyles["field-text"]}>Repeat once every</span>
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
            <FormFieldComponent
              entityType={scheduleType}
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
            />
            <FormFieldComponent
              entityType={scheduleType}
              field={{
                "@id": "string",
                "@type": "http://www.w3.org/ns/shacl#PropertyShape",
                name: { "@value": "to" },
                fieldId: FORM_STATES.TIME_SLOT_END,
                datatype: "time",
                description: { "@value": "" },
                order: 1,
              }}
              form={props.form}
              options={props.options}
            />
          </div>
        </div>
      </div>
    </fieldset>);
}