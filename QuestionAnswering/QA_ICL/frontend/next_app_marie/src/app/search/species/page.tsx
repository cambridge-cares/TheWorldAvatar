import { getChemicalClasses, getSpecies, getUses } from '@/lib/api/ontospecies'
import { SpeciesForm } from './_components/form'
import { Main } from '@/components/ui/main'
import { DataTable } from '@/components/ui/data-table'
import {
  OSpeciesIdentifierKey,
  OSpeciesPropertyKey,
} from '@/lib/model/ontospecies'

function getFirst(
  params: { [key: string]: string | string[] | undefined },
  key: string
) {
  let val = params[key]
  if (typeof val === 'string') return val
  if (typeof val === 'object' && val.length > 0) return val[0]
  return undefined
}

function getAll(
  params: { [key: string]: string | string[] | undefined },
  key: string
) {
  let val = params[key]
  if (typeof val === 'string') return [val]
  if (typeof val === 'object') return val
  return []
}

export default async function SpeciesPage({
  searchParams,
}: {
  searchParams: { [key: string]: string | string[] | undefined }
}) {
  const [chemicalClasses, uses] = await Promise.all([
    getChemicalClasses(),
    getUses(),
  ])

  const chemicalClass = getFirst(searchParams, 'chemicalClass') || ''
  const use = getFirst(searchParams, 'use') || ''
  const identifier = Object.fromEntries(
    Object.values(OSpeciesIdentifierKey).map(key => [
      key,
      getFirst(searchParams, key) || '',
    ])
  )
  const property = Object.fromEntries(
    Object.values(OSpeciesPropertyKey).map(key => {
      const vals = getAll(searchParams, key)
      const lower =
        vals.find(val => val.startsWith('gte:'))?.substring('gte:'.length) || ''
      const upper =
        vals.find(val => val.startsWith('lte:'))?.substring('lte:'.length) || ''
      return [key, { lower, upper }]
    })
  )
  const defaultValues = {
    chemicalClass,
    use,
    identifier,
    property,
  }

  const speciesLst = await getSpecies(searchParams)

  return (
    <Main className='flex flex-col items-center'>
      <div className='w-full mt-8 px-4 md:max-w-screen-md lg:max-w-screen-lg'>
        <h1 className='mb-4'>Species search</h1>
        <SpeciesForm
          initValues={defaultValues}
          allChemicalClasses={chemicalClasses}
          allUses={uses}
          className='mb-12'
        />
        {speciesLst && (
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
        )}
      </div>
    </Main>
  )
}
