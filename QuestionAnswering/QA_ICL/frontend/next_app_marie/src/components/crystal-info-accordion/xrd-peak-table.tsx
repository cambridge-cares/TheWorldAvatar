'use client'

import * as React from 'react'
import { createColumnHelper } from '@tanstack/react-table'

import { MeasureVector, XRDPeak } from '@/lib/model/ontozeolite'
import { DataTable } from '@/components/ui/data-table'

const COL_HELPER = createColumnHelper<XRDPeak>()
const COLS = [
  COL_HELPER.accessor('TwoThetaPosition', { header: '2θ' }),
  COL_HELPER.accessor('RelativeIntensity', { header: 'Relative intensity' }),
  COL_HELPER.accessor('isSimulated', { header: 'Simulated' }),
  COL_HELPER.accessor('MillerIndices', {
    header: 'Miller indices',
    cell: cell => {
      const components = cell.getValue<MeasureVector>().component
      const sortedComponents = ['h', 'k', 'l'].map(label =>
        components.find(component => component.label === label)
      )
      return `(${sortedComponents.map(x => (x ? x.value : '')).join(', ')})`
    },
  }),
]

export const XRDPeakTable = ({ data }: { data: XRDPeak[] }) => {
  return <DataTable columns={COLS} data={data} numbered paginated bordered />
}
