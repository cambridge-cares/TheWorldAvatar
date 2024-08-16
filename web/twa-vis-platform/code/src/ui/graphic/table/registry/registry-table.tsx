import styles from './registry.table.module.css';
import iconStyles from 'ui/graphic/icon/icon-button.module.css';

import React, { useEffect, useMemo, useState } from 'react';
import { FieldValues } from 'react-hook-form';

import StatusComponent from 'ui/text/status/status';
import IconComponent from 'ui/graphic/icon/icon';
import { MaterialIconButtonWithIndex } from 'ui/graphic/icon/icon-button';
import { parseWordsForLabels } from 'utils/client-utils';

interface RegistryTableProps {
  fields: FieldValues[];
  clickEventHandlers?: { [key: string]: (index: number) => void };
  limit?: number;
}

/**
 * This component renders a registry of table based on the input headers and rows.
 * 
 * @param {FieldValues[]} fields The field values for the table.
 * @param {Function} clickEventHandlers Event on button click.
 * @param {number} limit Optional limit to the number of columns shown.
 */
export default function RegistryTable(props: Readonly<RegistryTableProps>) {
  const [statusCol, setStatusCol] = useState<number>(-1);
  const [headers, setHeaders] = useState<string[]>([]);

  // A hook that parses the headers whenever fields are refetched
  useEffect(() => {
    const parseHeaders = (fields: FieldValues[], limit: number): string[] => {
      return Object.keys(fields[0]).sort((current, next) => {
        // Always move the id first if available, else, name should be first
        if (current === "id") return -1;
        if (next === "id") return 1;
        if (current === "name") return -1;
        if (next === "name") return 1;
        return 0; // Keep other items in the same order
      }).slice(0, limit);
    }

    if (props.fields?.length > 0) {
      setHeaders(parseHeaders(props.fields, props.limit));
    } else {
      setHeaders([]);
    }
  }, [props.fields]);

  const headerElements: React.ReactNode = useMemo(() => {
    if (headers?.length > 0) {
      // Returns the first header as an icon and the remaining header items
      return [<th key={"header-order"}><IconComponent classes={iconStyles["small-icon"]} icon="sort" /></th>,
      ...headers.map((header, colIndex) => {
        if (header.toLowerCase() === "status") {
          setStatusCol(colIndex);
        }
        return <th key={header + colIndex}>{parseWordsForLabels(header)}</th>
      })];
    } else {
      return <th></th>;
    }
  }, [headers]);

  const rowElements: React.ReactNode = useMemo(() => {
    if (props.fields?.length > 0) {
      return props.fields.map((row, rowIndex) => (
        <tr key={row[headers[0]] + rowIndex}>
          <td>
            {props.clickEventHandlers && <div className={styles["table-icon-cell"]}>
              <MaterialIconButtonWithIndex
                iconName="edit"
                iconStyles={[iconStyles["small-icon"], styles["expand-icon"]]}
                index={rowIndex}
                onButtonClick={props.clickEventHandlers["edit"]}
              />
              <MaterialIconButtonWithIndex
                iconName="expand_circle_right"
                iconStyles={[iconStyles["small-icon"], styles["expand-icon"]]}
                index={rowIndex}
                onButtonClick={props.clickEventHandlers["view"]}
              />
              <MaterialIconButtonWithIndex
                iconName="delete"
                iconStyles={[iconStyles["small-icon"], styles["expand-icon"]]}
                index={rowIndex}
                onButtonClick={props.clickEventHandlers["delete"]}
              />
            </div>
            }
          </td>
          {headers.map((column, colIndex) => {
            const columnVal: string = column != "id" ? parseWordsForLabels(row[column]) : row[column];
            return colIndex == statusCol ?
              (<td key={column + colIndex}><StatusComponent status={columnVal} /> </td>) :
              (<td key={column + colIndex} >{columnVal}</td>)
          }
          )}
        </tr>
      ))
    }
  }, [props.fields, statusCol]);

  return (
    <table className={styles["table"]}>
      <thead>
        <tr className={styles["header"]}>
          {headerElements}
        </tr>
      </thead>
      <tbody>
        {rowElements}
      </tbody>
    </table>
  );
}