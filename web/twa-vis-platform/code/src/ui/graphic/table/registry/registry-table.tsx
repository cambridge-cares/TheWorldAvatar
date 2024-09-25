import styles from './registry.table.module.css';

import React from 'react';

import { RegistryFieldValues } from 'types/form';
import StatusComponent from 'ui/text/status/status';
import { parseWordsForLabels } from 'utils/client-utils';
import { ColumnDef, flexRender, getCoreRowModel, useReactTable } from '@tanstack/react-table';
import { FieldValues } from 'react-hook-form';

interface RegistryTableProps {
  instances: RegistryFieldValues[];
  clickEventHandlers: { [key: string]: (index: number) => void };
  limit?: number;
}

/**
 * This component renders a registry of table based on the input headers and rows.
 * 
 * @param {RegistryFieldValues[]} instances The instance values for the table.
 * @param {Function} clickEventHandlers Event on button click.
 * @param {number} limit Optional limit to the number of columns shown.
 */
export default function RegistryTable(props: Readonly<RegistryTableProps>) {
  // Generate a list of column headings
  const columns: ColumnDef<Record<string, string>>[] = React.useMemo(() => {
    if (props.instances?.length === 0) return [];
    return Object.keys(props.instances[0]).map(field => ({
      id: field,
      accessorKey: field,
      header: parseWordsForLabels(field),
    }));
  }, [props.instances]);

  // Parse row values
  const data: FieldValues[] = React.useMemo(() => {
    if (props.instances?.length === 0) return [];
    // Extract only the value into the data to simplify
    return props.instances.map(instance => {
      const flattenInstance: Record<string, string> = {};
      Object.keys(instance).map(field => {
        flattenInstance[field] = instance[field].value
      })
      return flattenInstance;
    });
  }, [props.instances]);


  const table = useReactTable({
    data,
    columns,
    getCoreRowModel: getCoreRowModel(),
  });

  return (
    <table className={styles["table"]}>
      <thead className={styles["header"]}>
        {table.getHeaderGroups().map(headerGroup => (
          <tr key={headerGroup.id}>
            {headerGroup.headers.map(header => (
              <th key={header.id}>
                {header.isPlaceholder
                  ? null
                  : flexRender(
                    header.column.columnDef.header,
                    header.getContext()
                  )}
              </th>
            ))}
          </tr>
        ))}
      </thead>
      <tbody>
        {table.getRowModel().rows.map(row => (
          <tr key={row.id}>
            {row.getVisibleCells().map(cell => (
              <td key={cell.id}>
                {flexRender(cell.column.columnDef.cell, cell.getContext())}
              </td>
            ))}
          </tr>
        ))}
      </tbody>
    </table>
  );
}