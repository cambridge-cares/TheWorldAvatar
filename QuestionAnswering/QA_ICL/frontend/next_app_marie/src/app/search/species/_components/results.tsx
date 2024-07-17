'use client'

import { DataTable } from '@/components/ui/data-table'
import { getSpeciesMany } from '@/lib/api/ontospecies'
import { SpeciesBase } from '@/lib/model/ontospecies'
import { useSearchParams } from 'next/navigation'
import * as React from 'react'

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
      columns={[
        {
          accessorKey: 'IRI',
          header: 'IRI',
        },
        {
          accessorKey: 'label',
          header: 'Label',
        },
        {
          accessorKey: 'IUPAC_name',
          header: 'IUPAC name',
        },
        {
          accessorKey: 'InChI',
          header: 'InChI',
        },
      ]}
      data={data}
      numbered
      paginated
      bordered
      className='w-full md:max-w-screen-md lg:max-w-screen-lg mb-12'
    />
  ) : isLoading ? (
    <div>Getting search results...</div>
  ) : (
    <></>
  )
}
