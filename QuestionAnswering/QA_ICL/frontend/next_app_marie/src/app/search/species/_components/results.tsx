'use client'

import { DataTable } from '@/components/ui/data-table'
import { getSpecies } from '@/lib/api/ontospecies'
import { SpeciesBase } from '@/lib/model/ontospecies'
import { useSearchParams } from 'next/navigation'
import * as React from 'react'

export interface SpeciesSearchResultsParams {
  searchParams: [string, string][]
}

export function SpeciesSearchResults() {
  const searchParams = useSearchParams()

  const [isLoading, setIsLoading] = React.useState(false)
  const [speciesLst, setSpeciesLst] = React.useState<SpeciesBase[] | undefined>(
    undefined
  )

  React.useEffect(() => {
    async function retrieveSpecies() {
      setIsLoading(true)
      try {
        setSpeciesLst(await getSpecies(searchParams))
      } catch {
      } finally {
        setIsLoading(false)
      }
    }

    setSpeciesLst(undefined)
    if (searchParams.size > 0) {
      retrieveSpecies()
    }
  }, [searchParams])

  return speciesLst === undefined ? (
    isLoading ? (
      <div>Getting search results...</div>
    ) : (
      <></>
    )
  ) : (
    <DataTable
      columns={[
        {
          value: 'IRI',
          label: 'IRI',
        },
        {
          value: 'label',
          label: 'label',
        },
        {
          value: 'IUPAC_name',
          label: 'IUPAC name',
        },
        {
          value: 'InChI',
          label: 'InChI',
        },
      ]}
      data={speciesLst}
      className='w-full md:max-w-screen-md lg:max-w-screen-lg mb-12'
    />
  )
}
