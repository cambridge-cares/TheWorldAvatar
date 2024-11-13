'use client'

import * as React from 'react'
import { useSearchParams } from 'next/navigation'

import { DataTable } from '@/components/ui/data-table'
import { ColumnDef } from '@tanstack/react-table'
import Link from 'next/link'
import { Button } from '@/components/ui/button'

export interface LinkButtonToIRIPageProps {
  IRI: string
  prefixPath: string
}

export const LinkButtonToIRIPage = ({
  IRI,
  prefixPath,
}: LinkButtonToIRIPageProps) => (
  <Link
    href={`${prefixPath}?iri=${encodeURIComponent(IRI)}`}
    className='hover:underline'
  >
    <Button variant='secondary'>View</Button>
  </Link>
)

export interface SearchResultProps<T> {
  dataGetter: (searchParams: URLSearchParams) => Promise<T[]>
  columns: ColumnDef<T, any>[]
}

export function SearchResults<T>({
  dataGetter,
  columns,
}: SearchResultProps<T>) {
  const searchParams = useSearchParams()

  const [isLoading, setIsLoading] = React.useState(false)
  const [data, setData] = React.useState<T[] | undefined>(undefined)

  React.useEffect(() => {
    async function retrieveData() {
      setIsLoading(true)
      try {
        setData(await dataGetter(searchParams))
      } catch {
      } finally {
        setIsLoading(false)
      }
    }

    setData(undefined)
    if (searchParams.size > 0) {
      retrieveData()
    }
  }, [dataGetter, searchParams])

  return data !== undefined ? (
    <DataTable
      columns={columns}
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
