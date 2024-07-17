import * as React from 'react'

import { getCBUs, getSBUs } from '@/lib/api/ontozeolite'
import { Main } from '@/components/ui/main'
import { ZeoliteFrameworkForm } from './_components/form'
import { ZeoliteFrameworkResults } from './_components/results'

export default async function ZeoliteFrameworksPage() {
  const [allCBUs, allSBUs] = await Promise.all([getCBUs(), getSBUs()])

  return (
    <Main className='flex flex-col items-center'>
      <div className='w-full mt-8 px-4 md:max-w-screen-md lg:max-w-screen-lg'>
        <h1 className='mb-4'>Zeolite framework search</h1>
        <React.Suspense>
          <ZeoliteFrameworkForm
            allCBUs={allCBUs}
            allSBUs={allSBUs}
            className='mb-12'
          />
        </React.Suspense>
        <React.Suspense>
          <ZeoliteFrameworkResults />
        </React.Suspense>
      </div>
    </Main>
  )
}
