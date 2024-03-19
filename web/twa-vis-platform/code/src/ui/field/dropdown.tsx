import styles from './dropdown.module.css';

import React from 'react';


export type DropdownFieldOption = {
  index: number;
  label: string;
};

type DropdownFieldProps = {
  options: DropdownFieldOption[];
  selectedIndex: number;
  setSelectedIndex: React.Dispatch<React.SetStateAction<number>>;
};

/**
 * This component renders a dropdown field populated with the options from the data.
 * 
 * @param {DropdownFieldOption[]} options The list of options to render.
 * @param {number} selectedIndex The currently selected index.
 * @param {React.Dispatch<React.SetStateAction<number>>} setSelectedIndex The method to update the selected index based on the event.
 */
export default function DropdownField(props: DropdownFieldProps) {
  const handleChange = (event: React.ChangeEvent<HTMLSelectElement>) => {
    props.setSelectedIndex(parseInt(event.target.value));
  };

  return (
    <div className={styles["dropdown-container"]}>
      <label className={styles["label"]} htmlFor="mySelect">&ensp;Select time series:&ensp;</label>
      <select className={styles["selector"]} value={props.selectedIndex} style={{ fontSize: "0.85rem" }} onChange={handleChange}>
        {props.options.map((option) => (
          <option key={option.index} value={option.index}>
            {option.label}
          </option>
        ))}
      </select>
    </div>
  );
};