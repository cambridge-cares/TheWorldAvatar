import path from 'path'
import { promises as fs } from 'fs'

import matter from 'gray-matter'

import { PATH_TO_RESOURCES } from '@/lib/fs'
import { Main } from '@/components/layout'
import { Prose } from '@/components/ui/prose'

const PATH_TO_TBOX_INFO = path.join(PATH_TO_RESOURCES, 'tbox-info')

export default async function OntologyInfo() {
  const data = await fs
    .readdir(PATH_TO_TBOX_INFO, {
      withFileTypes: true,
    })
    .then(dirents =>
      Promise.all(
        dirents
          .filter(dirent => dirent.isFile() && dirent.name.endsWith('.md'))
          .map(f =>
            fs
              .readFile(path.join(PATH_TO_TBOX_INFO, f.name), 'utf-8')
              .then(content => matter(content))
              .then(({ data, content }) => ({
                sortKey: data.sortKey as number,
                id: data.id as string,
                heading: data.title as string,
                mdContent: content,
              }))
          )
      )
    )
    .then(data =>
      data.sort(
        ({ sortKey: keyLeft }, { sortKey: keyRight }) => keyLeft - keyRight
      )
    )

  return (
    <Main className='flex flex-col items-center'>
      <div className='w-full md:max-w-screen-md lg:max-w-screen-lg pt-8 mb-12 px-4 [&_section]:mb-12 last:mb-0'>
        <h1 className='mb-8'>Information on Chemistry Ontologies</h1>
        <section>
          <h2 className='mb-2'>Table of Contents</h2>
          <ol className='list-decimal list-inside'>
            {data.map(({ id, heading }, i) => (
              <li key={i} className='text-xl'>
                <a href={`#${id}`} className='hover:underline'>
                  {heading}
                </a>
              </li>
            ))}
          </ol>
        </section>
        {data.map(({ id, heading, mdContent }, i) => (
          <section key={i}>
            <h2 id={id} className='mb-2'>
              <a href={`#${id}`} className='hover:underline'>
                {heading}
              </a>
            </h2>
            <Prose>{mdContent}</Prose>
          </section>
        ))}
      </div>
    </Main>
  )
}
