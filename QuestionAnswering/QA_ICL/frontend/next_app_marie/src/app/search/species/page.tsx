import { getChemicalClasses, getUses } from '@/lib/api/ontospecies'
import { SpeciesForm } from './_components/form'
import { Main } from '@/components/ui/main'
import { SpeciesSearchResults } from './_components/results'
import { Suspense } from 'react'

export default async function SpeciesPage() {
  const [chemicalClasses, uses] = await Promise.all([
    getChemicalClasses(),
    getUses(),
  ])

  return (
    <Main className='flex flex-col items-center'>
      <div className='w-full mt-8 px-4 md:max-w-screen-md lg:max-w-screen-lg'>
        <h1 className='mb-4'>Species search</h1>
        <Suspense>
          <SpeciesForm
            allChemicalClasses={chemicalClasses}
            allUses={uses}
            className='mb-12'
          />
        </Suspense>
        <Suspense>
          <SpeciesSearchResults />
        </Suspense>
      </div>
    </Main>
  )
}
