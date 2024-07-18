'use client'

import * as React from 'react'
import { createColumnHelper } from '@tanstack/react-table'

import { MeasureVector, XRDPeak } from '@/lib/model/ontozeolite'
import { DataTable } from '@/components/ui/data-table'

const COL_HELPER = createColumnHelper<XRDPeak>()
const COLS = [
  COL_HELPER.accessor('two_theta_position', { header: '2θ' }),
  COL_HELPER.accessor('relative_intensity', { header: 'Relative intensity' }),
  COL_HELPER.accessor('is_simulated', { header: 'Simulated' }),
  COL_HELPER.accessor('miller_indices', {
    header: 'Miller indices',
    cell: cell => {
      const components = cell.getValue<MeasureVector>().vector_component
      const sortedComponents = ['h', 'k', 'l'].map(label =>
        components.find(component => component.label === label)
      )
      return `(${sortedComponents.map(x => (x ? x.value : '')).join(', ')})`
    },
  }),
]

export const XRDPeakTable = ({ data }: { data: XRDPeak[] }) => {
  const sortedData = React.useMemo(
    () => data.toSorted((a, b) => a.two_theta_position - b.two_theta_position),
    [data]
  )
  return (
    <DataTable columns={COLS} data={sortedData} numbered paginated bordered />
  )
}
