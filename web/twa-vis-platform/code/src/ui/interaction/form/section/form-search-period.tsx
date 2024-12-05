import styles from '../form.module.css';

import React from 'react';
import { UseFormReturn } from 'react-hook-form';

import { FORM_STATES } from '../form-utils';
import FormFieldComponent from '../field/form-field';

interface FormSearchPeriodProps {
  form: UseFormReturn;
}

/**
 * This component renders a form section displaying the search period.
 * 
 * @param {UseFormReturn} form A react-hook-form hook containing methods and state for managing the associated form.
 */
export default function FormSearchPeriod(props: Readonly<FormSearchPeriodProps>) {
  return (
    <fieldset className={styles["form-fieldset"]} style={{ marginBottom: "1rem" }}>
      <legend className={styles["form-fieldset-label"]}>Search Period</legend>
      <div className={styles["form-fieldset-contents"]}>
        <FormFieldComponent
          entityType={""}
          field={{
            "@id": "string",
            "@type": "http://www.w3.org/ns/shacl#PropertyShape",
            name: { "@value": "from" },
            fieldId: FORM_STATES.START_TIME_PERIOD,
            datatype: "dateTime",
            description: { "@value": "Select the starting date/time for your search." },
            order: 0,
          }}
          form={props.form}
        />
        <FormFieldComponent
          entityType={""}
          field={{
            "@id": "string",
            "@type": "http://www.w3.org/ns/shacl#PropertyShape",
            name: { "@value": "to" },
            fieldId: FORM_STATES.END_TIME_PERIOD,
            datatype: "dateTime",
            description: { "@value": "Select the ending date/time for your search." },
            order: 0,
          }}
          form={props.form}
        />
      </div>
    </fieldset>);
}