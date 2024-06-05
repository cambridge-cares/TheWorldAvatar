"use client"

import * as React from "react"

import {
  Column,
  Row,
  flexRender,
  getCoreRowModel,
  getPaginationRowModel,
  useReactTable,
} from "@tanstack/react-table"
import { DoubleArrowLeftIcon, DoubleArrowRightIcon } from "@radix-ui/react-icons"

import { TableDataBase, TableDataRow, TableDataValue } from "@/lib/model"
import { cn } from "@/lib/utils"
import {
  Table,
  TableBody,
  TableCell,
  TableHead,
  TableHeader,
  TableRow,
} from "@/components/ui/table"
import { Button } from "@/components/ui/button"
import { ScrollArea, ScrollBar } from "@/components/ui/scroll-area"
import { Select, SelectContent, SelectItem, SelectTrigger, SelectValue } from "@/components/ui/select"


export type DataTableProps = React.HTMLAttributes<HTMLDivElement> & TableDataBase

export type DataTableBaseProps = DataTableProps & {
  paginated?: boolean
  bordered?: boolean
  scrollable?: boolean
}

type TableDataRowNumbered = TableDataRow & { num: number }

function DataTableBase({
  columns,
  data,
  className,
  ...props
}: DataTableBaseProps) {
  const processedColumns = React.useMemo(
    () => ["num"].concat(columns),
    [columns]
  )
  const processedData = React.useMemo(
    () => data.map((datum, idx) => ({ num: idx + 1, ...datum })),
    [data]
  )

  const { paginated = false, bordered = false, scrollable = false, ...otherProps } = props

  const columnsOption = processedColumns.map(h => ({
    id: h,
    accessorFn: (row: TableDataRowNumbered) => row[h],
    header: ({ column }: { column: Column<TableDataRowNumbered> }) => {
      return (
        <div>{h === "num" ? "No." : h}</div>
      )
    },
    cell: ({ row }: { row: Row<TableDataRowNumbered> }) => {
      const val = row.getValue(h) as TableDataValue;
      if (!(val)) {
        return ""
      } else if (Array.isArray(val)) {
        return (<ul>{val.map((elem, idx) => <li key={idx}>elem</li>)}</ul>)
      } else if (typeof val === "object") {
        return (<DataTableBase columns={val.columns} data={val.data} />)
      } else {
        return val;
      }
    }
  }));


  const tableOptions = {
    data: processedData,
    columns: columnsOption,
    getCoreRowModel: getCoreRowModel(),
    ...paginated ? ({ getPaginationRowModel: getPaginationRowModel() }) : ({})
  }
  const table = useReactTable(tableOptions)

  const borderClassName = bordered ? "rounded-md border" : ""
  const tableComponent = (
    <Table>
      <TableHeader className={scrollable ? "sticky top-0 bg-secondary" : ""}>
        {table.getHeaderGroups().map((headerGroup) => (
          <TableRow key={headerGroup.id}>
            {headerGroup.headers.map((header) => {
              return (
                <TableHead key={header.id}>
                  {header.isPlaceholder
                    ? null
                    : flexRender(
                      header.column.columnDef.header,
                      header.getContext()
                    )}
                </TableHead>
              )
            })}
          </TableRow>
        ))}
      </TableHeader>
      <TableBody>
        {table.getRowModel().rows?.length ? (
          table.getRowModel().rows.map((row) => (
            <TableRow
              key={row.id}
              data-state={row.getIsSelected() && "selected"}
            >
              {row.getVisibleCells().map((cell) => (
                <TableCell key={cell.id}>
                  {flexRender(cell.column.columnDef.cell, cell.getContext())}
                </TableCell>
              ))}
            </TableRow>
          ))
        ) : (
          <TableRow>
            <TableCell colSpan={columnsOption.length} className="h-24 text-center">
              No results.
            </TableCell>
          </TableRow>
        )}
      </TableBody>
    </Table>
  )

  return (
    <div className={cn("flex flex-col space-y-2", className)} {...otherProps}>
      {paginated && (
        <div className="flex items-center space-x-2">
          <Select
            value={`${table.getState().pagination.pageSize}`}
            onValueChange={(value) => {
              table.setPageSize(Number(value))
            }}
          >
            <SelectTrigger className="h-8 w-[70px]">
              <SelectValue placeholder={table.getState().pagination.pageSize} />
            </SelectTrigger>
            <SelectContent side="top">
              {[5, 10, 15, 20].map((pageSize) => (
                <SelectItem key={pageSize} value={`${pageSize}`}>
                  {pageSize}
                </SelectItem>
              ))}
            </SelectContent>
          </Select>
          <p className="text-sm">entries per page</p>
        </div>
      )}
      {
        scrollable ? (
          <ScrollArea className={["w-full", borderClassName].filter(x => x).join(" ")}>
            {tableComponent}
            <ScrollBar orientation="horizontal" />
          </ScrollArea>
        ) : (
          <div className={borderClassName}>{tableComponent}</div>
        )
      }
      {paginated && (
        <div className="flex justify-between">
          <div className="text-sm text-right">
            Page {table.getState().pagination.pageIndex + 1} of{" "}
            {table.getPageCount()}
          </div>
          <div className="flex items-center justify-end space-x-2">
            <Button
              variant="outline"
              className="hidden h-8 w-8 p-0 lg:flex"
              onClick={() => table.setPageIndex(0)}
              disabled={!table.getCanPreviousPage()}
            >
              <span className="sr-only">Go to first page</span>
              <DoubleArrowLeftIcon className="h-4 w-4" />
            </Button>
            <Button
              variant="outline"
              size="sm"
              onClick={() => table.previousPage()}
              disabled={!table.getCanPreviousPage()}
            >
              Previous
            </Button>
            <Button
              variant="outline"
              size="sm"
              onClick={() => table.nextPage()}
              disabled={!table.getCanNextPage()}
            >
              Next
            </Button>
            <Button
              variant="outline"
              className="hidden h-8 w-8 p-0 lg:flex"
              onClick={() => table.setPageIndex(table.getPageCount() - 1)}
              disabled={!table.getCanNextPage()}
            >
              <span className="sr-only">Go to last page</span>
              <DoubleArrowRightIcon className="h-4 w-4" />
            </Button>
          </div>
        </div>
      )}
    </div>
  )
}


export const DataTable = ({
  columns,
  data,
  ...props
}: DataTableProps) => (
  <DataTableBase
    columns={columns}
    data={data}
    paginated
    bordered
    scrollable
    {...props}
  />
)