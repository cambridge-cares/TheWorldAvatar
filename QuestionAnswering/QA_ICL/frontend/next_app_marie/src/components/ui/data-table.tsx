"use client"

import { useState } from "react"
import {
  Column,
  Row,
  SortingState,
  flexRender,
  getCoreRowModel,
  getPaginationRowModel,
  getSortedRowModel,
  useReactTable,
} from "@tanstack/react-table"
import {
  Table,
  TableBody,
  TableCell,
  TableHead,
  TableHeader,
  TableRow,
} from "@/components/ui/table"
import { Button } from "@/components/ui/button"
import { CaretSortIcon } from "@radix-ui/react-icons"
import { TableDataBase, TableDataRow } from "@/lib/model"


export interface DataTableProps extends TableDataBase {
  paginated: boolean
  isInner: boolean
}


function DataTableBase({
  columns,
  data,
  paginated,
  isInner,
  ...props
}: DataTableProps) {
  console.log("drawing table")
  const cols = columns.map(h => ({
    id: h,
    accessorFn: (row: TableDataRow) => row[h],
    header: ({ column }: { column: Column<TableDataRow> }) => {
      return (
        <Button
          variant="ghost"
          onClick={() => column.toggleSorting(column.getIsSorted() === "asc")}
        >
          {h}
          <CaretSortIcon className="ml-2 h-4 w-4" />
        </Button>
      )
    },
    cell: ({ row }: { row: Row<TableDataRow> }) => {
      const val = row.getValue(h) as TableDataBase;
      if (!(val)) {
        return ""
      } else if (typeof val === "object") {
        return (<DataTableBase columns={val.columns} data={val.data} paginated={false} isInner={true} />)
      } else {
        return val;
      }
    }
  }));

  const [sorting, setSorting] = useState<SortingState>([]);

  const tableOptions = {
    data,
    columns: cols,
    getCoreRowModel: getCoreRowModel(),
    getPaginationRowModel: getPaginationRowModel(),
    onSortingChange: setSorting,
    getSortedRowModel: getSortedRowModel(),
    state: {
      sorting,
    },
  }
  if (paginated) {
    tableOptions["getPaginationRowModel"] = getPaginationRowModel()
  }
  const table = useReactTable(tableOptions)

  return (
    <div {...props}>
      <div className={isInner ? "" : "rounded-md border"}>
        <Table>
          <TableHeader>
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
                <TableCell colSpan={cols.length} className="h-24 text-center">
                  No results.
                </TableCell>
              </TableRow>
            )}
          </TableBody>
        </Table>
      </div>
      {paginated && (
        <div className="flex items-center justify-end space-x-2 py-4">
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
        </div>
      )}
    </div>
  )
}


export const DataTable = ({
  columns,
  data,
  ...props
}: TableDataBase) => DataTableBase({ columns, data, paginated: true, isInner: false, ...props })