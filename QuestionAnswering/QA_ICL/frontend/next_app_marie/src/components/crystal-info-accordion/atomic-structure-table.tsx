'use client'

import { createColumnHelper } from '@tanstack/react-table'

import { DataTable } from '@/components/ui/data-table'
import { AtomSite, MeasureVector } from '@/lib/model/ontozeolite'

const COL_HELPER = createColumnHelper<AtomSite>()
const COLS = [
  COL_HELPER.accessor('label', { header: 'Label' }),
  COL_HELPER.group({
    header: 'Cartesian position (Å)',
    columns: ['x', 'y', 'z'].map(axis =>
      COL_HELPER.accessor('CartesianPosition', {
        id: `cart-${axis}`,
        header: axis,
        cell: cell =>
          cell
            .getValue<MeasureVector>()
            .component.find(component => component.label === axis)?.value,
      })
    ),
  }),
  COL_HELPER.group({
    header: 'Fractional position',
    columns: ['x', 'y', 'z'].map(axis =>
      COL_HELPER.accessor('FractionalPosition', {
        id: `fract-${axis}`,
        header: axis,
        cell: cell =>
          cell
            .getValue<MeasureVector>()
            .component.find(component => component.label === axis)?.value,
      })
    ),
  }),
]

export const AtomicStructureTable = ({
  atomicStructure,
}: {
  atomicStructure: AtomSite[]
}) => (
  <DataTable
    columns={COLS}
    data={atomicStructure}
    bordered
    numbered
    paginated
  />
)
