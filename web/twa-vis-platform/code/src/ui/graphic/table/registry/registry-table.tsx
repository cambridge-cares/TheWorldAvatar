import styles from './registry.table.module.css';

import React from 'react';
import { FieldValues } from 'react-hook-form';

import { RegistryFieldValues } from 'types/form';
import { parseWordsForLabels } from 'utils/client-utils';
import RegistryRowActions from './actions/registry-table-action';
import StatusComponent from 'ui/text/status/status';

import { DataGrid, GridColDef, GridRenderCellParams } from '@mui/x-data-grid';
import Box from '@mui/material/Box';
import { RegistryTableTheme } from './registry-table-theme';

interface RegistryTableProps {
  recordType: string;
  isTaskPage: boolean;
  instances: RegistryFieldValues[];
  setTaskId: React.Dispatch<React.SetStateAction<string>>;
  setTaskStatus: React.Dispatch<React.SetStateAction<string>>;
  limit?: number;
}

/**
 * This component renders a registry of table based on the inputs.
 * 
 * @param {string} recordType The type of the record.
 * @param {boolean} isTaskPage Indicator if the table is currently on the task view.
 * @param {RegistryFieldValues[]} instances The instance values for the table.
 * @param setTaskId A dispatch method to set task id when required.
 * @param setTaskStatus A dispatch method to set task status when required.
 * @param {number} limit Optional limit to the number of columns shown.
 */
export default function RegistryTable(props: Readonly<RegistryTableProps>) {
  // Generate a list of column headings
  // const columns: ColumnDef<Record<string, string>>[] = React.useMemo(() => {
  const columns: GridColDef[] = React.useMemo(() => {
    if (props.instances?.length === 0) return [];
    return [
      {
        field: "actions",
        headerName: "",
        width: 100,
        renderCell: (params) => {
          return (<RegistryRowActions
            recordType={props.recordType}
            isTaskPage={props.isTaskPage}
            row={params.row}
            setTaskId={props.setTaskId}
            setTaskStatus={props.setTaskStatus}
          />);
        }
      },
      ...Object.keys(props.instances[0]).map(field => ({
        field,
        headerName: parseWordsForLabels(field),
        width: 150, // Adjust the width as needed
        renderCell: (params: GridRenderCellParams) => {
          // Render status differently
          if (field.toLowerCase() === "status") {
            return (<StatusComponent status={`${params.value}`} />);
          }
          if (params.value) {
            return parseWordsForLabels(`${params.value}`);
          }
          return "";
        }
      }))
    ];

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

  return (
    <RegistryTableTheme >
      <Box>
        <DataGrid
          className={styles["table"]}
          rows={data}
          columns={columns}
          initialState={{
            pagination: {
              paginationModel: {
                pageSize: 5,
              },
            },
          }}
          pageSizeOptions={[5]}
          checkboxSelection={false}
          disableRowSelectionOnClick={false}
          autosizeOnMount={true}
          getRowId={(row) => row.id || row.iri}
        />
      </Box>
    </RegistryTableTheme>
  )
}