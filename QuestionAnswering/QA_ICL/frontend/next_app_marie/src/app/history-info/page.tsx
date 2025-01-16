import path from 'path'
import { promises as fs } from 'fs'

import { PATH_TO_RESOURCES } from '@/lib/fs'
import { Main } from '@/components/layout'
import { Prose } from '@/components/ui/prose'

const PATH_TO_HISTORY_INFO = path.join(PATH_TO_RESOURCES, 'history.md')
export default async function HistoryInfo() {
  const mdContent = await fs.readFile(PATH_TO_HISTORY_INFO, 'utf-8')

  return (
    <Main className='flex flex-col items-center'>
      <div className='w-full md:max-w-screen-md lg:max-w-screen-lg mb-12 px-4 pt-8'>
        <h1 className='mb-8'>History of Marie</h1>
        <Prose>{mdContent}</Prose>
      </div>
    </Main>
  )
}
