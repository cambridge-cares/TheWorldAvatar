import { promises as fs } from 'fs'
import path from 'path'

import * as React from 'react'
import matter from 'gray-matter'

import { QAFragment } from './_components/qa'
import { IntroSection } from './_components/intro-section'
import { ExampleQuestionGroup } from './_components/example-question-accordion'
import { TBoxInfoAccordion } from './_components/tbox-info-accordion'

export default async function Home() {
  const introText = await fs.readFile(
    path.join(process.cwd(), 'resources/intro-text.md'),
    'utf8'
  )
  const exampleQuestionGroups: ExampleQuestionGroup[] = await fs
    .readFile(
      path.join(process.cwd(), 'resources/example-questions.json'),
      'utf-8'
    )
    .then(content => JSON.parse(content))

  const tboxInfo = await fs
    .readdir(path.join(process.cwd(), 'resources/ontologies'), {
      withFileTypes: true,
    })
    .then(dirents =>
      Promise.all(
        dirents
          .filter(dirent => dirent.isFile() && dirent.name.endsWith('.md'))
          .map(f =>
            fs
              .readFile(path.join('resources/ontologies', f.name), 'utf-8')
              .then(content => matter(content))
              .then(({ data, content }) => ({
                key: data.key as number,
                heading: data.title as string,
                mdContent: content,
              }))
          )
      )
    )
    .then(data =>
      data.sort(({ key: keyLeft }, { key: keyRight }) => keyLeft - keyRight)
    )

  return (
    <main className='p-4 min-h-screen flex flex-col justify-center items-center'>
      <IntroSection
        imgSrc='/images/marie-thumbnail.jpg'
        imgAlt="Marie's thumbnail"
        introTextMd={introText}
        className='w-full md:max-w-screen-md lg:max-w-screen-lg mb-8'
      />
      <QAFragment exampleQuestionGroups={exampleQuestionGroups} />
      <section className='w-full md:max-w-screen-sm lg:max-w-screen-md'>
        <TBoxInfoAccordion data={tboxInfo} type='multiple' className='w-full' />
      </section>
    </main>
  )
}
