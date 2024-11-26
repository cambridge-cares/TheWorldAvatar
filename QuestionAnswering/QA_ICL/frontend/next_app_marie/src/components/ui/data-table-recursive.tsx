'use client'

import * as React from 'react'

import { Row } from '@tanstack/react-table'
import { DataTable, DataTableUIOptions } from './data-table'

export type DataTableCellValue =
  | null
  | undefined
  | string
  | number
  | string[]
  | number[]
  | DataTableRecursiveDataProps

export interface DataTableRow {
  [key: string]: DataTableCellValue
}

export interface DataTableRecursiveDataProps {
  columns: string[]
  data: DataTableRow[]
}

export type DataTableRecursiveProps = React.HTMLAttributes<HTMLDivElement> &
  DataTableRecursiveDataProps &
  DataTableUIOptions

export function DataTableRecursive({
  columns,
  data,
  ...props
}: DataTableRecursiveProps) {
  const processedColumns = columns.map(h => ({
    accessorFn: (row: DataTableRow) => row[h],
    header: h,
    cell: ({ row }: { row: Row<DataTableRow> }) => {
      const val = row.getValue(h) as DataTableCellValue
      if (!val) {
        return ''
      } else if (typeof val === 'string' || typeof val === 'number') {
        return val
      } else if (Array.isArray(val)) {
        return (
          <ul>
            {val.map((elem, idx) => (
              <li key={idx}>{elem}</li>
            ))}
          </ul>
        )
      } else if ('columns' in val) {
        // TODO: Check which typescript and @types/react versions support
        // type-narrowing with React.isValidElement instead of checking
        // existence of a field in DataTableBaseProps
        return <DataTableRecursive columns={val.columns} data={val.data} />
      } else {
        return val
      }
    },
  }))

  return <DataTable columns={processedColumns} data={data} {...props} />
}
