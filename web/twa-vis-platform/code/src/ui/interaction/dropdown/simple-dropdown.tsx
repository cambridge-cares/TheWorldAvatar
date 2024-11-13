import styles from './dropdown.module.css';

import React from 'react';


interface SimpleDropdownFieldProps {
  options: string[];
  handleChange: (_event: React.ChangeEvent<HTMLSelectElement>) => void;
}

/**
 * This component renders a simple dropdown field without a title populated with the options.
 * 
 * @param {string[]} options The list of options to render.
 */
export default function SimpleDropdownField(props: Readonly<SimpleDropdownFieldProps>) {
  return (
    <select className={styles["simple-selector"]} onChange={props.handleChange}>
      {props.options.map((option) => (
        <option key={option} value={option}>
          {option}
        </option>
      ))}
    </select>
  );
}