import * as React from 'react'

import { getChemicalClasses } from '@/lib/api/ontospecies'
import { Main } from '@/components/layout'
import { SpeciesPropertiesPlotCtx } from './_components/plot-ctx'

export const dynamic = 'force-dynamic'

export default async function SpeciesPropertiesPlotPage() {
  const chemicalClasses = await getChemicalClasses()

  return (
    <Main className='flex flex-col items-center'>
      <div className='w-full mt-8 px-4 md:max-w-screen-md lg:max-w-screen-lg'>
        <h1 className='mb-4'>SpeciesExplorer</h1>
        <SpeciesPropertiesPlotCtx chemicalClassOptions={chemicalClasses} />
      </div>
    </Main>
  )
}
