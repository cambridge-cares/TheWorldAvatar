import * as React from 'react'

import { MatrixComponent } from '@/lib/model/ontozeolite'
import { Table, TableCell, TableRow } from '@/components/ui/table'

export const MatrixTable = ({ data }: { data: MatrixComponent[] }) => {
  const colNum = React.useMemo(
    () => Math.max(...data.map(component => component.col_index)),
    [data]
  )
  const rowNum = React.useMemo(
    () => Math.max(...data.map(component => component.row_index)),
    [data]
  )

  return (
    <Table>
      {Array.from(Array(rowNum).keys()).map(i => (
        <TableRow key={i}>
          {Array.from(Array(colNum).keys()).map(j => (
            <TableCell key={j}>
              {
                data.find(
                  component =>
                    component.row_index === i + 1 &&
                    component.col_index === j + 1
                )?.value
              }
            </TableCell>
          ))}
        </TableRow>
      ))}
    </Table>
  )
}
