import { promises as fs } from 'fs'
import path from 'path'
import matter from 'gray-matter'

import * as React from 'react'

import { QAFragment } from './_components/qa'
import { IntroSection } from './_components/intro-section'
import { ExampleQuestionGroup } from './_components/example-question-tabs'
import { PATH_TO_RESOURCES } from '@/lib/fs'
import { Main } from '@/components/layout'

const PATH_TO_EXAMPLE_QUESTIONS = path.join(
  PATH_TO_RESOURCES,
  'example-questions.json'
)

export default async function Home() {
  const introText = await fs
    .readFile(path.join(process.cwd(), 'resources/intro-text.md'), 'utf8')
    .then(content => matter(content))
    .then(({ data, content }) => ({
      heading: data.title as string,
      mdContent: content,
    }))
  const exampleQuestionGroups: ExampleQuestionGroup[] = await fs
    .readFile(PATH_TO_EXAMPLE_QUESTIONS, 'utf-8')
    .then(content => JSON.parse(content))

  return (
    <Main className='p-4 flex flex-col justify-center items-center'>
      <IntroSection
        imgSrc='/images/marie-thumbnail.jpg'
        imgAlt="Marie's thumbnail"
        {...introText}
        className='w-full md:max-w-screen-md lg:max-w-screen-lg mb-8'
      />
      <QAFragment exampleQuestionGroups={exampleQuestionGroups} />
    </Main>
  )
}
