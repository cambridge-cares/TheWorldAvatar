'use client'

import * as React from 'react'
import { useSearchParams } from 'next/navigation'
import { createColumnHelper } from '@tanstack/react-table'

import { ZeoliteFrameworkBase } from '@/lib/model/ontozeolite'
import { getZeoliteFrameworks } from '@/lib/api/ontozeolite'
import { DataTable } from '@/components/ui/data-table'

const COL_HELPER = createColumnHelper<ZeoliteFrameworkBase>()
const COLS = [
  COL_HELPER.accessor('IRI', { header: 'IRI' }),
  COL_HELPER.accessor('code', { header: 'Code' }),
]

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
