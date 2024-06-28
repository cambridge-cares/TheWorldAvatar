import { promises as fs } from 'fs'
import path from 'path'
import matter from 'gray-matter'

import * as React from 'react'

import { QAFragment } from './_components/qa'
import { IntroSection } from './_components/intro-section'
import { ExampleQuestionGroup } from './_components/example-question-tabs'
import { AdditionalInfoSection } from './_components/additional-info-section'
import { PATH_TO_RESOURCES } from '@/lib/fs'

const PATH_TO_EXAMPLE_QUESTIONS = path.join(
  PATH_TO_RESOURCES,
  'example-questions.json'
)
const PATH_TO_HISTORY_INFO = path.join(PATH_TO_RESOURCES, 'history.md')

export default async function Home() {
  const introText = await fs.readFile(
    path.join(process.cwd(), 'resources/intro-text.md'),
    'utf8'
  ).then(content => matter(content)).then(
    ({ data, content }) => ({
      heading: data.title as string,
      mdContent: content
    })
  )
  const exampleQuestionGroups: ExampleQuestionGroup[] = await fs
    .readFile(PATH_TO_EXAMPLE_QUESTIONS, 'utf-8')
    .then(content => JSON.parse(content))

  const historyInfo = await fs.readFile(PATH_TO_HISTORY_INFO, 'utf-8')

  return (
    <main className='p-4 min-h-screen flex flex-col justify-center items-center'>
      <IntroSection
        imgSrc='/images/marie-thumbnail.jpg'
        imgAlt="Marie's thumbnail"
        {...introText}
        className='w-full md:max-w-screen-md lg:max-w-screen-lg mb-8'
      />
      <QAFragment exampleQuestionGroups={exampleQuestionGroups} />
      <AdditionalInfoSection
        historyInfoMdContent={historyInfo}
        className='w-full md:max-w-screen-sm lg:max-w-screen-md'
      />
    </main>
  )
}
