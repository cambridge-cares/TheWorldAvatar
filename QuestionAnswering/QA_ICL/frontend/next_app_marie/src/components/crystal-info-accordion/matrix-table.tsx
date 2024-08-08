import * as React from 'react'

import { MatrixComponent } from '@/lib/model/ontozeolite'
import { Table, TableBody, TableCell, TableRow } from '@/components/ui/table'

export const MatrixTable = ({ data }: { data: MatrixComponent[] }) => {
  const colNum = React.useMemo(
    () => Math.max(...data.map(component => component.col)),
    [data]
  )
  const rowNum = React.useMemo(
    () => Math.max(...data.map(component => component.row)),
    [data]
  )

  return (
    <Table>
      <TableBody>
        {Array.from(Array(rowNum).keys()).map(i => (
          <TableRow key={i}>
            {Array.from(Array(colNum).keys()).map(j => (
              <TableCell key={j}>
                {
                  data.find(
                    component =>
                      component.row === i + 1 && component.col === j + 1
                  )?.value
                }
              </TableCell>
            ))}
          </TableRow>
        ))}
      </TableBody>
    </Table>
  )
}
