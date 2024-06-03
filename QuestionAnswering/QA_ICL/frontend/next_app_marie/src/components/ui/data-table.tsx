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
import { TableDataBase, TableDataRow, TableDataValue } from "@/lib/model"
import { ScrollArea, ScrollBar } from "./scroll-area"


export interface DataTableProps extends TableDataBase {
  paginated?: boolean
  bordered?: boolean
  scrollable: boolean
}


function DataTableBase({
  columns,
  data,
  scrollable,
  ...props
}: DataTableProps) {
  const { paginated = false, bordered = false, ...otherProps } = props

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
      const val = row.getValue(h) as TableDataValue;
      if (!(val)) {
        return ""
      } else if (Array.isArray(val)) {
        return (<ul>{val.map((elem, idx) => <li key={idx}>elem</li>)}</ul>)
      } else if (typeof val === "object") {
        return (<DataTableBase columns={val.columns} data={val.data} scrollable={false} />)
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

  // const scrollableClassName = scrollable ? "h-fit max-h-80 w-fit max-w-full overflow-y-auto overflow-x-auto relative" : null
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
            <TableCell colSpan={cols.length} className="h-24 text-center">
              No results.
            </TableCell>
          </TableRow>
        )}
      </TableBody>
    </Table>
  )

  return (
    <div {...otherProps}>
      {
        scrollable ? (
          <ScrollArea className={["h-96 w-full", borderClassName].filter(x => x).join(" ")}>
            {tableComponent}
            <ScrollBar orientation="horizontal" />
          </ScrollArea>
        ) : (
          <div className={borderClassName}>{tableComponent}</div>
        )
      }
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
}: TableDataBase) => (<DataTableBase columns={columns} data={data} paginated bordered scrollable {...props} />)