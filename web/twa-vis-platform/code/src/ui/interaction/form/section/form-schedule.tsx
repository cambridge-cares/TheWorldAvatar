import styles from '../form.module.css';
import fieldStyles from '../field/field.module.css';

import React, { useEffect, useMemo, useState } from 'react';
import { UseFormReturn } from 'react-hook-form';
import Select from 'react-select';

import { FormOptionType, RegistryFieldValues, SEARCH_FORM_TYPE } from 'types/form';
import { selectorStyles } from 'ui/css/selector-style';
import { parseWordsForLabels } from 'utils/client-utils';
import FormCheckboxField from '../field/form-checkbox-field';
import { FORM_STATES, getDefaultVal } from '../form-utils';
import FormFieldComponent from '../field/form-field';
import { Paths } from 'io/config/routes';
import { sendGetRequest } from 'utils/server-actions';
import LoadingSpinner from 'ui/graphic/loader/spinner';

interface FormScheduleProps {
  fieldId: string;
  agentApi: string;
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
 * @param {string} agentApi The target agent endpoint for any registry related functionalities.
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
  const isDisabledOption: { disabled: boolean; } = { disabled: formType == Paths.REGISTRY || formType == Paths.REGISTRY_DELETE };
  const [isLoading, setIsLoading] = useState<boolean>(true);
  // Define the state to store the selected value
  const [selectedServiceOption, setSelectedServiceOption] = useState<string>(
    props.form.getValues(FORM_STATES.RECURRENCE) == 0 ? singleService :
      props.form.getValues(FORM_STATES.RECURRENCE) == -1 ? alternateService : regularService
  );

  useEffect(() => {
    const getAndSetScheduleDefaults = async (): Promise<void> => {
      const response: string = await sendGetRequest(`${props.agentApi}/contracts/schedule/${props.form.getValues("id")}`);
      const jsonResponse: RegistryFieldValues = JSON.parse(response);

      // Retrieve recurrence and selected service option
      const recurrence: number = getDefaultVal(FORM_STATES.RECURRENCE, jsonResponse[FORM_STATES.RECURRENCE].value, formType) as number;
      setSelectedServiceOption(recurrence == 0 ? singleService : recurrence == -1 ? alternateService : regularService);
      props.form.setValue(FORM_STATES.RECURRENCE, recurrence);

      props.form.setValue(FORM_STATES.START_DATE, getDefaultVal(FORM_STATES.START_DATE, jsonResponse[FORM_STATES.START_DATE.replace(/\s+/g, "_")].value, formType));
      props.form.setValue(FORM_STATES.END_DATE, getDefaultVal(FORM_STATES.END_DATE, jsonResponse[FORM_STATES.END_DATE.replace(/\s+/g, "_")].value, formType));
      props.form.setValue(FORM_STATES.TIME_SLOT_START, getDefaultVal(FORM_STATES.TIME_SLOT_START, jsonResponse["start_time"].value, formType));
      props.form.setValue(FORM_STATES.TIME_SLOT_END, getDefaultVal(FORM_STATES.TIME_SLOT_END, jsonResponse["end_time"].value, formType));
      daysOfWeek.map(dayOfWeek => {
        props.form.setValue(dayOfWeek, getDefaultVal(dayOfWeek, jsonResponse[dayOfWeek].value, formType));
      });
      setIsLoading(false);
    }
    if (formType == Paths.REGISTRY_ADD || formType == SEARCH_FORM_TYPE) {
      props.form.setValue(FORM_STATES.RECURRENCE, 1);
      setIsLoading(false);
    } else {
      getAndSetScheduleDefaults();
    }
  }, [])

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
      {isLoading && <LoadingSpinner isSmall={true} />}
      {!isLoading && <div className={styles["form-fieldset-contents"]}>
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
            isDisabled={formType == Paths.REGISTRY || formType == Paths.REGISTRY_DELETE}
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
          options={isDisabledOption}
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
          options={isDisabledOption}
        />}
        {selectedServiceOption === regularService && <div className={styles["schedule-occurrence-container"]}>
          <span className={fieldStyles["field-text"]}>Repeat once every</span>
          <input
            id={FORM_STATES.RECURRENCE}
            type={"number"}
            className={`${styles["schedule-occurrence-input"]} ${props.options?.disabled && styles["field-disabled"]}`}
            step={"1"}
            readOnly={formType == Paths.REGISTRY || formType == Paths.REGISTRY_DELETE}
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
                  options={isDisabledOption}
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
              options={isDisabledOption}
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
              options={isDisabledOption}
            />
          </div>
        </div>
      </div>}
    </fieldset>);
}