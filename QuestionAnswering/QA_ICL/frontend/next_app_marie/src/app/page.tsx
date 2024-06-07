import { promises as fs } from 'fs'
import path from 'path'

import * as React from 'react'
import { default as matter } from 'gray-matter'

import { QAFragment } from './_components/qa'
import { IntroSection } from './_components/intro-section'
import { ExampleQuestionGroup } from './_components/example-question-accordion'
import { AdditionalInfoSection } from './_components/additional-info-section'

const PATH_TO_RESOURCES = path.join(process.cwd(), 'resources')
const PATH_TO_EXAMPLE_QUESTIONS = path.join(
  PATH_TO_RESOURCES,
  'example-questions.json'
)
const PATH_TO_TBOX_INFO = path.join(PATH_TO_RESOURCES, 'tbox-info')
const PATH_TO_HISTORY_INFO = path.join(PATH_TO_RESOURCES, 'history.md')

export default async function Home() {
  const introText = await fs.readFile(
    path.join(process.cwd(), 'resources/intro-text.md'),
    'utf8'
  )
  const exampleQuestionGroups: ExampleQuestionGroup[] = await fs
    .readFile(PATH_TO_EXAMPLE_QUESTIONS, 'utf-8')
    .then(content => JSON.parse(content))
  const tboxInfo = await fs
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
  const historyInfo = await fs.readFile(PATH_TO_HISTORY_INFO, 'utf-8')

  return (
    <main className='p-4 min-h-screen flex flex-col justify-center items-center'>
      <IntroSection
        imgSrc='/images/marie-thumbnail.jpg'
        imgAlt="Marie's thumbnail"
        introTextMd={introText}
        className='w-full md:max-w-screen-md lg:max-w-screen-lg mb-8'
      />
      <QAFragment exampleQuestionGroups={exampleQuestionGroups} />
      <AdditionalInfoSection
        tboxInfoData={tboxInfo}
        historyInfoMdContent={historyInfo}
        className='w-full md:max-w-screen-sm lg:max-w-screen-md'
      />
    </main>
  )
}
