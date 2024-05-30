"use client"

import { useState } from "react"
import {
  Column,
  Row,
  SortingState,
  VisibilityState,
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
import { TableDataItemHeader } from "@/lib/model"


const TWA_ABOX_IRI_PREFIXES = ["http://www.theworldavatar.com/kb/", "https://www.theworldavatar.com/kg/"]


interface DataTableProps<TData> {
  headers: TableDataItemHeader[]
  data: TData[]
}

export function DataTable<TData extends { [key: string]: any }>({
  headers,
  data,
  ...props
}: DataTableProps<TData>) {
  const aboxIRIHeaders = headers
    .map(h => h.value)
    .filter(h => data
      .map(datum => datum[h])
      .every(val => typeof val === "string" && TWA_ABOX_IRI_PREFIXES.some(prefix => val.startsWith(prefix)))
    );

  const columns = headers.map(h => ({
    accessorKey: h.value,
    header: ({ column }: { column: Column<TData> }) => {
      return (
        <Button
          variant="ghost"
          onClick={() => column.toggleSorting(column.getIsSorted() === "asc")}
        >
          {h.label}
          <CaretSortIcon className="ml-2 h-4 w-4" />
        </Button>
      )
    },
    cell: ({ row }: { row: Row<TData> }) => {
      const val = row.getValue(h.value);
      return typeof val === 'object' ? JSON.stringify(val) : val;
    }
  }));

  const [sorting, setSorting] = useState<SortingState>([]);
  const [areIRIColsVisible, setAreIRIColsVisible] = useState(false);
  const [columnVisibility, setColumnVisibility] = useState<VisibilityState>(Object.fromEntries(aboxIRIHeaders.map(h => [h, areIRIColsVisible])))

  const table = useReactTable({
    data,
    columns,
    getCoreRowModel: getCoreRowModel(),
    getPaginationRowModel: getPaginationRowModel(),
    onSortingChange: setSorting,
    getSortedRowModel: getSortedRowModel(),
    onColumnVisibilityChange: setColumnVisibility,
    state: {
      sorting,
      columnVisibility,
    },
  })

  const aboxIRICols = table
    .getAllColumns()
    .filter(col => aboxIRIHeaders.some(h => h == col.id));

  return (
    <div {...props}>
      <div className="flex items-center justify-start">
        <Button variant="outline" size="sm" onClick={() => {
          setAreIRIColsVisible(!areIRIColsVisible);
          aboxIRICols.forEach(col => col.toggleVisibility(areIRIColsVisible)
          );
        }}>{areIRIColsVisible ? "Hide IRIs" : "Show IRIs"}</Button>
      </div>
      <div className="rounded-md border">
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
                <TableCell colSpan={columns.length} className="h-24 text-center">
                  No results.
                </TableCell>
              </TableRow>
            )}
          </TableBody>
        </Table>
      </div>
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
    </div>
  )
}
