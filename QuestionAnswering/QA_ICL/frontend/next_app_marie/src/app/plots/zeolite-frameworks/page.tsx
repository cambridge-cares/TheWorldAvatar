import * as React from 'react'

import { Main } from '@/components/layout'
import { ZeolitePropertiesPlotCtx } from './_components/plot-ctx'

export default async function ZeolitePropertiesPlotPage() {
  return (
    <Main className='flex flex-col items-center'>
      <div className='w-full mt-8 px-4 md:max-w-screen-md lg:max-w-screen-lg'>
        <h1 className='mb-4'>ZeoliteExplorer</h1>
        <ZeolitePropertiesPlotCtx />
      </div>
    </Main>
  )
}
