import styles from './registry.table.module.css';
import iconStyles from 'ui/graphic/icon/icon-button.module.css';

import React from 'react';
import { FieldValues } from 'react-hook-form';
import { CellContext, ColumnDef, DisplayColumnDef, flexRender, getCoreRowModel, useReactTable } from '@tanstack/react-table';

import { RegistryFieldValues } from 'types/form';
import { getAfterDelimiter, isValidIRI, parseWordsForLabels } from 'utils/client-utils';
import RegistryRowActions from './actions/registry-table-action';
import IconComponent from 'ui/graphic/icon/icon';
import StatusComponent from 'ui/text/status/status';

interface RegistryTableProps {
  recordType: string;
  instances: RegistryFieldValues[];
  limit?: number;
}

/**
 * This component renders a registry of table based on the inputs.
 * 
 * @param {string} recordType The type of the record.
 * @param {RegistryFieldValues[]} instances The instance values for the table.
 * @param {number} limit Optional limit to the number of columns shown.
 */
export default function RegistryTable(props: Readonly<RegistryTableProps>) {
  // Generate a list of column headings
  const columns: ColumnDef<Record<string, string>>[] = React.useMemo(() => {
    if (props.instances?.length === 0) return [];
    // Include an additional action column definition
    const actionCol: DisplayColumnDef<Record<string, string>> = {
      id: "actions",
      header: () => (<IconComponent classes={iconStyles["small-icon"]} icon="linear_scale" />),
      cell: (context: CellContext<Record<string, string>, unknown>) => {
        const recordId: string = isValidIRI(context.row.original.id) ?
          getAfterDelimiter(context.row.original.id, "/")
          : context.row.original.id;
        return (<RegistryRowActions
          recordId={recordId}
          recordType={props.recordType}
        />)
      },
    }
    return [actionCol, ...Object.keys(props.instances[0]).map(field => ({
      id: field,
      accessorKey: field,
      header: parseWordsForLabels(field),
      cell: (context: CellContext<Record<string, string>, unknown>) => {
        // Render status differently
        if (context.column.id.toLowerCase() === "status") {
          return (<StatusComponent status={`${context.getValue()}`} />);
        }
        if (context.getValue()) {
          return parseWordsForLabels(`${context.getValue()}`);
        }
        return "";
      }
    }))];
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