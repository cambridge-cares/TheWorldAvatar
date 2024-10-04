'use client'

import { createColumnHelper } from '@tanstack/react-table'

import { DataTable } from '@/components/ui/data-table'
import { OntospeciesProperty } from '@/lib/model/ontospecies'

const PROPERTY_TABLE_COL_HELPER = createColumnHelper<OntospeciesProperty>()

const PROPERTY_TABLE_COLS_NO_REFSTATE = [
  PROPERTY_TABLE_COL_HELPER.accessor('value', { header: 'Value' }),
  PROPERTY_TABLE_COL_HELPER.accessor('unit', { header: 'Unit' }),
  PROPERTY_TABLE_COL_HELPER.accessor('Provenance', { header: 'Provenance' }),
]

const PROPERTY_TABLE_COLS_WITH_REFSTATE = [
  PROPERTY_TABLE_COL_HELPER.accessor('value', { header: 'Value' }),
  PROPERTY_TABLE_COL_HELPER.accessor('unit', { header: 'Unit' }),
  PROPERTY_TABLE_COL_HELPER.group({
    header: 'Reference state',
    columns: [
      PROPERTY_TABLE_COL_HELPER.accessor(row => row.ReferenceState?.value, {
        id: 'ref_value',
        header: 'Value',
      }),
      PROPERTY_TABLE_COL_HELPER.accessor(row => row.ReferenceState?.unit, {
        id: 'ref_unit',
        header: 'Unit',
      }),
    ],
  }),
  PROPERTY_TABLE_COL_HELPER.accessor('Provenance', { header: 'Provenance' }),
]

export const PropertyTable = ({ rows }: { rows: OntospeciesProperty[] }) => (
  <DataTable
    columns={
      rows.every(row => row.ReferenceState !== undefined)
        ? PROPERTY_TABLE_COLS_WITH_REFSTATE
        : PROPERTY_TABLE_COLS_NO_REFSTATE
    }
    data={rows}
    bordered
  />
)
