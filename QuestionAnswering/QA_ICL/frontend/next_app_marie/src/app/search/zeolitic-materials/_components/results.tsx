'use client'

import * as React from 'react'
import Link from 'next/link'
import { useSearchParams } from 'next/navigation'
import { createColumnHelper } from '@tanstack/react-table'

import { Button } from '@/components/ui/button'
import { getZeoliticMaterialsMany } from '@/lib/api/ontozeolite'
import { ZeoliticMaterialBase } from '@/lib/model/ontozeolite'
import { DataTable } from '@/components/ui/data-table'

const COL_HELPER = createColumnHelper<ZeoliticMaterialBase>()
const COLS = [
  COL_HELPER.accessor('IRI', {
    header: '',
    cell: cell => {
      const val = cell.getValue()
      return (
        <Link
          href={`/zeolitic-materials/${encodeURIComponent(val)}`}
          className='hover:underline'
        >
          <Button variant='secondary'>View</Button>
        </Link>
      )
    },
  }),
  COL_HELPER.accessor('ChemicalFormula', {
    header: 'Formula'
  })
]

export function ZeoliticMaterialResults() {
  const searchParams = useSearchParams()

  const [isLoading, setIsLoading] = React.useState(false)
  const [data, setData] = React.useState<ZeoliticMaterialBase[] | undefined>(
    undefined
  )

  React.useEffect(() => {
    async function retrieveData() {
      setIsLoading(true)
      try {
        setData(await getZeoliticMaterialsMany(searchParams))
      } catch {
      } finally {
        setIsLoading(false)
      }
    }

    setData(undefined)
    if (searchParams.size > 0) {
      retrieveData()
    }
  }, [searchParams])

  return data !== undefined ? (
    <DataTable
      columns={COLS}
      data={data}
      numbered
      paginated
      bordered
      scrollable
    />
  ) : isLoading ? (
    <div>Getting search results...</div>
  ) : (
    <></>
  )
}
