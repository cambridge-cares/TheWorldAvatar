import * as React from 'react'

import {
  getFrameworkComponents,
  getGuestComponents,
  getJournals,
  getZeoliteFrameworksMany,
} from '@/lib/api/ontozeolite'
import { Main } from '@/components/ui/main'
import { ZeoliticMaterialForm } from './_components/form'
import { ZeoliticMaterialResults } from './_components/results'

export default async function ZeoliticMaterialsPage() {
  const [frameworks, framweorkComponents, guests, journals] = await Promise.all(
    [
      getZeoliteFrameworksMany(),
      getFrameworkComponents(),
      getGuestComponents(),
      getJournals(),
    ]
  )

  return (
    <Main className='flex flex-col items-center'>
      <div className='w-full mt-8 px-4 mb-12 md:max-w-screen-md lg:max-w-screen-lg'>
        <h1 className='mb-4'>Zeolitic material search</h1>
        <React.Suspense>
          <ZeoliticMaterialForm
            className='mb-12'
            frameworkOptions={frameworks}
            frameworkComponentOptions={framweorkComponents}
            guestOptions={guests}
            journalOptions={journals}
          />
        </React.Suspense>
        <React.Suspense>
          <ZeoliticMaterialResults />
        </React.Suspense>
      </div>
    </Main>
  )
}
