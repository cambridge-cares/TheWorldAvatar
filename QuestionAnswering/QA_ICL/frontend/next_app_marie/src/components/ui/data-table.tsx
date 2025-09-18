'use client'

import {
  ColumnDef,
  flexRender,
  getCoreRowModel,
  getPaginationRowModel,
  useReactTable,
} from '@tanstack/react-table'

import {
  Table,
  TableBody,
  TableCell,
  TableHead,
  TableHeader,
  TableRow,
} from '@/components/ui/table'
import React from 'react'
import {
  Select,
  SelectContent,
  SelectItem,
  SelectTrigger,
  SelectValue,
} from './select'
import { ScrollArea, ScrollBar } from './scroll-area'
import { Button } from './button'
import {
  DoubleArrowLeftIcon,
  DoubleArrowRightIcon,
} from '@radix-ui/react-icons'
import { cn } from '@/lib/utils'

export interface DataTableUIOptions {
  numbered?: boolean
  paginated?: boolean
  initialPageSize?: number
  bordered?: boolean
  scrollable?: boolean
}

export type DataTableProps<TData> = React.HTMLAttributes<HTMLDivElement> &
  DataTableUIOptions & {
    columns: ColumnDef<TData, any>[]
    data: TData[]
  }

export function DataTable<TData>({
  columns,
  data,
  className,
  numbered = false,
  paginated = false,
  initialPageSize = 10,
  bordered = false,
  scrollable = false,
  ...props
}: DataTableProps<TData>) {
  const processedColumns = React.useMemo(
    () =>
      numbered
        ? [
            { accessorKey: 'num', header: 'No.' } as ColumnDef<TData, any>,
          ].concat(columns)
        : columns,
    [numbered, columns]
  )
  const processedData = React.useMemo(
    () =>
      numbered ? data.map((datum, idx) => ({ num: idx + 1, ...datum })) : data,
    [numbered, data]
  )

  const [pagination, setPagination] = React.useState({
    pageIndex: 0,
    pageSize: initialPageSize,
  })

  const table = useReactTable({
    data: processedData,
    columns: processedColumns,
    getCoreRowModel: getCoreRowModel(),
    ...(paginated
      ? {
          getPaginationRowModel: getPaginationRowModel(),
          onPaginationChange: setPagination,
          state: { pagination },
        }
      : {}),
  })

  const borderClassName = bordered ? 'rounded-md border' : ''
  const tableComponent = (
    <Table>
      <TableHeader
        className={cn(
          bordered ? 'bg-secondary' : '',
          scrollable ? 'sticky top-0' : ''
        )}
      >
        {table.getHeaderGroups().map(headerGroup => (
          <TableRow key={headerGroup.id}>
            {headerGroup.headers.map(header => (
              <TableHead key={header.id} colSpan={header.colSpan}>
                {header.isPlaceholder
                  ? null
                  : flexRender(
                      header.column.columnDef.header,
                      header.getContext()
                    )}
              </TableHead>
            ))}
          </TableRow>
        ))}
      </TableHeader>
      <TableBody>
        {table.getRowModel().rows?.length ? (
          table.getRowModel().rows.map(row => (
            <TableRow
              key={row.id}
              data-state={row.getIsSelected() && 'selected'}
            >
              {row.getVisibleCells().map(cell => {
                const value = cell.getValue();

                return (
                  <TableCell 
                    key={cell.id}
                  >
                    {Array.isArray(value) && value.length > 1 ? (
                      <div className="flex flex-col divide-y divide-gray-300">
                        {value.map((item, idx) => (
                          <div 
                            key={idx} 
                            className={` flex items-center justify-center pl-2 py-1 `}
                          >
                            {typeof item === 'object' ? JSON.stringify(item) : String(item)}
                          </div>
                        ))}
                      </div>
                    ) : (
                        <div className="flex items-center justify-center h-full">
                          {flexRender(cell.column.columnDef.cell, cell.getContext())}
                        </div>
                    )}
                  </TableCell>
                );
              })}
            </TableRow>
          ))
        ) : (
          <TableRow>
            <TableCell
              colSpan={processedColumns.length}
              className='h-24 text-center'
            >
              No results.
            </TableCell>
          </TableRow>
        )}
      </TableBody>
    </Table>
  )

  return (
    <div className={cn('flex flex-col space-y-2', className)} {...props}>
      {paginated && (
        <div className='flex items-center space-x-2'>
          <Select
            value={`${table.getState().pagination.pageSize}`}
            onValueChange={value => {
              table.setPageSize(Number(value))
            }}
          >
            <SelectTrigger className='h-8 w-[70px]'>
              <SelectValue placeholder={table.getState().pagination.pageSize} />
            </SelectTrigger>
            <SelectContent side='top'>
              {[5, 10, 15, 20].map(pageSize => (
                <SelectItem key={pageSize} value={`${pageSize}`}>
                  {pageSize}
                </SelectItem>
              ))}
            </SelectContent>
          </Select>
          <p className='text-sm'>entries per page</p>
        </div>
      )}
      {scrollable ? (
        <ScrollArea className={cn('w-full', borderClassName)}>
          {tableComponent}
          <ScrollBar orientation='horizontal' />
        </ScrollArea>
      ) : (
        <div className={cn('w-full', borderClassName)}>{tableComponent}</div>
      )}
      {paginated && (
        <div className='flex justify-between'>
          <div className='text-sm text-right'>
            Page {table.getState().pagination.pageIndex + 1} of{' '}
            {table.getPageCount()}
          </div>
          <div className='flex items-center justify-end space-x-2'>
            <Button
              variant='outline'
              className='hidden h-8 w-8 p-0 lg:flex'
              onClick={() => table.setPageIndex(0)}
              disabled={!table.getCanPreviousPage()}
            >
              <span className='sr-only'>Go to first page</span>
              <DoubleArrowLeftIcon className='h-4 w-4' />
            </Button>
            <Button
              variant='outline'
              size='sm'
              onClick={() => table.previousPage()}
              disabled={!table.getCanPreviousPage()}
            >
              Previous
            </Button>
            <Button
              variant='outline'
              size='sm'
              onClick={() => table.nextPage()}
              disabled={!table.getCanNextPage()}
            >
              Next
            </Button>
            <Button
              variant='outline'
              className='hidden h-8 w-8 p-0 lg:flex'
              onClick={() => table.setPageIndex(table.getPageCount() - 1)}
              disabled={!table.getCanNextPage()}
            >
              <span className='sr-only'>Go to last page</span>
              <DoubleArrowRightIcon className='h-4 w-4' />
            </Button>
          </div>
        </div>
      )}
    </div>
  )
}
