'use client'

import { createColumnHelper } from '@tanstack/react-table'

import { DataTable } from '@/components/ui/data-table'
import { OntospeciesProperty } from '@/lib/model/ontospecies'

const PROPERTY_TABLE_COL_HELPER = createColumnHelper<OntospeciesProperty>()
const PROPERTY_TABLE_COLS = [
  PROPERTY_TABLE_COL_HELPER.accessor('value', { header: 'Value' }),
  PROPERTY_TABLE_COL_HELPER.accessor('unit', { header: 'Unit' }),
  PROPERTY_TABLE_COL_HELPER.group({
    header: 'Reference state',
    columns: [
      PROPERTY_TABLE_COL_HELPER.accessor(row => row.reference_state?.value, {
        id: 'ref_value',
        header: 'Value',
      }),
      PROPERTY_TABLE_COL_HELPER.accessor(row => row.reference_state?.unit, {
        id: 'ref_unit',
        header: 'Unit',
      }),
    ],
  }),
  PROPERTY_TABLE_COL_HELPER.accessor('provenance', { header: 'Provenance' }),
]

export const PropertyTable = ({ rows }: { rows: OntospeciesProperty[] }) => (
  <DataTable columns={PROPERTY_TABLE_COLS} data={rows} bordered />
)
