import { GroupBase, StylesConfig } from 'react-select';

import { FormOptionType } from 'types/form';

// Selector styles for react select
export const selectorStyles: StylesConfig<FormOptionType | GroupBase<FormOptionType>, false, GroupBase<FormOptionType>> = {
  control: (provided, { isDisabled }) => ({
    ...provided,
    outline: "1px solid var(--border-primary)",
    borderRadius: "5px",
    minHeight: "3vh",
    maxHeight: "3vh",
    width: "100%",
    maxWidth: "6rem",
    padding: "0 0.5rem",
    margin: "0",

    cursor: isDisabled ? "not-allowed !important" : "default",
    border: isDisabled ? "1px inset var(--border-primary)" : "none",
    backgroundColor: isDisabled ? "var(--background-secondary)" : "var(--background-tertiary)",
    opacity: isDisabled ? "0.75" : "1",
  }),
  dropdownIndicator: (provided, { isDisabled }) => ({
    ...provided,
    color: isDisabled ? "var(--background-secondary)" : "var(--text-color-primary)",
  }),
  menu: (provided) => ({
    ...provided,
    backgroundColor: "var(--background-tertiary)",
    boxShadow: "rgba(0, 0, 0, 0.24) 0px 3px 8px",
    margin: "0.25rem 0",
    minWidth: "100%",
    width: "fit-content",
  }),
  noOptionsMessage: (provided) => ({
    ...provided,
    padding: "0.25rem 0.5rem",
    backgroundColor: "var(--background-tertiary)",
    boxShadow: "rgba(100, 100, 111, 0.2) 0px 7px 29px 0px",
    color: "var(--background-inverse-primary)",
    fontSize: "var(--font-size-tertiary-text)",
  }),
  placeholder: (provided) => ({
    ...provided,
    color: "var(--background-inverse-primary)",
    fontSize: "var(--font-size-tertiary-text)",
  }),
  singleValue: (provided, { isDisabled }) => ({
    ...provided,
    color: isDisabled ? "var(--text-color-secondary)" : "var(--background-inverse-primary)",
    fontSize: "var(--font-size-tertiary-text)",
  }),
  input: (provided) => ({
    ...provided,
    color: "var(--background-inverse-primary)",
    fontSize: "var(--font-size-tertiary-text)",
  }),
  group: (provided) => ({
    ...provided,
    backgroundColor: "var(--background-tertiary)",
  }),
  groupHeading: (provided) => ({
    ...provided,
    color: "var(--text-color-primary)",
    textTransform: "none",
    textWrap: "nowrap",
    fontWeight: "600",
    fontSize: "var(--font-size-tertiary-text)",
    padding: "0.25rem 0.5rem",
    borderTop: "1px solid  var(--border-secondary)",
    borderBottom: "1px solid  var(--border-secondary)",
  }),
  option: (provided, { isDisabled, isSelected }) => {
    return {
      ...provided,
      backgroundColor: "var(--background-tertiary)",
      color: isSelected ? "var(--text-color-secondary)" : "var(--background-inverse-primary)",
      cursor: isDisabled ? 'not-allowed' : 'default',
      fontSize: "var(--font-size-tertiary-text)",
      "&:hover": {
        color: isSelected ? "var(--text-color-secondary)" : "var(--text-color-links-hover)",
      },
      padding: "0.25rem 0.5rem",
      textWrap: "nowrap",
      minWidth: "100%",
      width: "fit-content",
    };
  },
};