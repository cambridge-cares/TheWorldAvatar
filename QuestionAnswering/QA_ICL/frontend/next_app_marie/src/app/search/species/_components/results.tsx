'use client'

import * as React from 'react'
import { useSearchParams } from 'next/navigation'
import { createColumnHelper } from '@tanstack/react-table'

import { getSpeciesMany } from '@/lib/api/ontospecies'
import { SpeciesBase } from '@/lib/model/ontospecies'
import { DataTable } from '@/components/ui/data-table'
import Link from 'next/link'
import { Button } from '@/components/ui/button'

const COL_HELPER = createColumnHelper<SpeciesBase>()
const COLS = [
  COL_HELPER.accessor('IRI', {
    header: '',
    cell: cell => {
      const val = cell.getValue()
      return (
        <Link
          href={`/species/${encodeURIComponent(val)}`}
          className='hover:underline'
        >
          <Button variant='secondary'>View</Button>
        </Link>
      )
    },
  }),
  COL_HELPER.accessor('label', { header: 'Label' }),
  COL_HELPER.accessor('IUPAC_name', { header: 'IUPAC name' }),
  COL_HELPER.accessor('InChI', { header: 'InChI' }),
]

export function SpeciesSearchResults() {
  const searchParams = useSearchParams()

  const [isLoading, setIsLoading] = React.useState(false)
  const [data, setData] = React.useState<SpeciesBase[] | undefined>(undefined)

  React.useEffect(() => {
    async function retrieveData() {
      setIsLoading(true)
      try {
        setData(await getSpeciesMany(searchParams))
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
