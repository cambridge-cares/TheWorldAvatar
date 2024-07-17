'use client'

import * as React from 'react'

import { useSearchParams } from 'next/navigation'
import { ZeoliteFrameworkBase } from '@/lib/model/ontozeolite'
import { getZeoliteFrameworks } from '@/lib/api/ontozeolite'
import { DataTable } from '@/components/ui/data-table'

export function ZeoliteFrameworkResults() {
  const searchParams = useSearchParams()

  const [isLoading, setIsLoading] = React.useState(false)
  const [data, setData] = React.useState<ZeoliteFrameworkBase[] | undefined>(
    undefined
  )

  React.useEffect(() => {
    async function retreiveData() {
      setIsLoading(true)
      try {
        setData(await getZeoliteFrameworks(searchParams))
      } catch {
      } finally {
        setIsLoading(false)
      }
    }

    setData(undefined)
    if (searchParams.size > 0) {
      retreiveData()
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
          accessorKey: 'code',
          header: 'Code',
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
