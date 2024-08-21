import * as React from 'react'

import { getChemicalClasses, getUses } from '@/lib/api/ontospecies'
import { Main } from '@/components/layout'
import { SpeciesForm } from './_components/form'
import { SpeciesSearchResults } from './_components/results'

export const dynamic = 'force-dynamic'

export default async function SpeciesPage() {
  const [chemicalClasses, uses] = await Promise.all([
    getChemicalClasses(),
    getUses(),
  ])

  return (
    <Main className='flex flex-col items-center'>
      <div className='w-full mt-8 px-4 md:max-w-screen-md lg:max-w-screen-lg'>
        <h1 className='mb-4'>Species search</h1>
        <React.Suspense>
          <SpeciesForm
            chemicalClassOptions={chemicalClasses}
            useOptions={uses}
          />
        </React.Suspense>
        <React.Suspense>
          <SpeciesSearchResults />
        </React.Suspense>
      </div>
    </Main>
  )
}
