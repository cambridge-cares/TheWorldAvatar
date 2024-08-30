import * as React from 'react'

import { getCBUs, getSBUs } from '@/lib/api/ontozeolite'
import { Main } from '@/components/layout'
import { ZeoFrameworkForm } from './_components/form'
import { ZeoFrameworkResults } from './_components/results'

export const dynamic = 'force-dynamic'

export default async function ZeoFrameworkSearchPage() {
  const [allCBUs, allSBUs] = await Promise.all([getCBUs(), getSBUs()])

  return (
    <Main className='flex flex-col items-center'>
      <div className='w-full mt-8 px-4 md:max-w-screen-md lg:max-w-screen-lg'>
        <h1 className='mb-4'>Zeolite framework search</h1>
        <React.Suspense>
          <ZeoFrameworkForm CBUOptions={allCBUs} SBUOptions={allSBUs} />
        </React.Suspense>
        <React.Suspense>
          <ZeoFrameworkResults />
        </React.Suspense>
      </div>
    </Main>
  )
}
