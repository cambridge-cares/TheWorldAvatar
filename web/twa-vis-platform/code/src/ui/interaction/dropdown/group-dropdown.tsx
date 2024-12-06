import styles from './dropdown.module.css';

import React from 'react';

interface GroupDropdownFieldProps {
  placeholderText: string,
  options: string[];
  groups: string[];
  handleChange: (_event: React.ChangeEvent<HTMLSelectElement>) => void;
}

interface SelectGroupMappings {
  [group: string]: string[]
}

/**
 * This component renders a dropdown field with a designated group for each option.
 * 
 * @param {string} placeholderText Placeholder text for default.
 * @param {string[]} options The list of options to render.
 * @param {string[]} groups The list of groups associated with the options.
 * @param {Function} handleChange A function that handles the event when selecting a new element.
*/
export default function GroupDropdownField(props: Readonly<GroupDropdownFieldProps>) {
  return (
    <select className={styles["simple-selector"]} defaultValue={"DEFAULT"} onChange={props.handleChange}>
      <option value="DEFAULT" disabled>{props.placeholderText}</option>
      {Object.entries(genMappings(props.groups, props.options)).map(([group, options]) => (
        <optgroup key={group} label={group}>
          {options.map((option, index) => (
            <option key={group + option + index} value={option}>{option}</option>
          ))}
        </optgroup>
      ))}
    </select>
  );
}

/** A function that generate the group mappings for the dropdown selection.
 * 
 * @param {string[]} options The list of options to render.
 * @param {string[]} groups The list of groups associated with the options.
*/
function genMappings(groups: string[], options: string[]): SelectGroupMappings {
  const mapping: SelectGroupMappings = {};
  groups.map((group, index) => {
    const groupOption: string = options[index];
    mapping[group] = mapping[group] ? [groupOption].concat(mapping[group]) : [groupOption];
  });
  return mapping;
}