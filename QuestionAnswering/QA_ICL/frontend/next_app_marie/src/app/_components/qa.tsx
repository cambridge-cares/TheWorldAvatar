'use client'

import * as React from 'react'

import { QAResponse } from '@/lib/model'
import { queryChat, queryQa } from '@/lib/api'
import { NLPSearchForm } from './nlp-search-form'
import {
  ExampleQuestionTabs,
  ExampleQuestionGroup,
} from './example-question-tabs'
import { QAResponseDiv } from './qa-response-div'

export interface QAFragmentProps {
  exampleQuestionGroups: ExampleQuestionGroup[]
}

export function QAFragment({ exampleQuestionGroups }: QAFragmentProps) {
  const nlpFormRef = React.useRef<null | HTMLFormElement>(null)

  const [question, setQuestion] = React.useState('')
  const [isProcessing, setIsProcessing] = React.useState(false)
  const [qaResponse, setQaResponse] = React.useState<QAResponse | undefined>(
    undefined
  )
  const [chatAnswer, setChatAnswer] = React.useState<string | undefined>(
    undefined
  )

  const queryDataThenDisplay = async (question: string) => {
    setIsProcessing(true)
    setQaResponse(undefined)
    setChatAnswer(undefined)
    try {
      const qaRes = await queryQa(question)
      setQaResponse(qaRes)

      const textStream = await queryChat(qaRes.request_id)

      const pump = ({
        done,
        value,
      }: {
        done: boolean
        value?: string
      }): Promise<void> => {
        if (done) {
          return Promise.resolve()
        }

        // TODO: Use TransformerStream to do the parsing in API call code
        if (value) {
          value.split('\n').forEach(line => {
            const trimmedLine = line.trim()
            if (trimmedLine.startsWith('data: ')) {
              const msg = trimmedLine.substring('data: '.length)
              try {
                const dataChunk = JSON.parse(msg)
                const content = dataChunk['content']
                if (typeof content === 'string') {
                  setChatAnswer(oldValue => (oldValue || '') + content)
                }
              } catch (err) {
                console.log(
                  'Unexpected data received from server: '.concat(msg)
                )
              }
            }
          })
        }
        return textStream.read().then(pump)
      }
      await textStream.read().then(pump)
    } catch (err) {
    } finally {
      setIsProcessing(false)
    }
  }

  const handleExampleQuestionClick = async (
    qn: string,
    e: React.MouseEvent<HTMLLIElement>
  ) => {
    e.preventDefault()
    setQuestion(qn)
    nlpFormRef.current?.scrollIntoView()
    await queryDataThenDisplay(qn)
  }

  const handleNLPSearchFormSubmit = async (
    e: React.FormEvent<HTMLFormElement>
  ) => {
    e.preventDefault()
    nlpFormRef.current?.scrollIntoView()
    await queryDataThenDisplay(question)
  }

  return (
    <>
      <section className='w-full md:max-w-screen-sm lg:max-w-screen-md mb-8'>
        <h2 className='text-lg font-medium mb-2'>Example Questions</h2>
        <ExampleQuestionTabs
          data={exampleQuestionGroups}
          questionOnClick={handleExampleQuestionClick}
        />
      </section>
      <section className='w-full md:max-w-screen-sm lg:max-w-screen-md mb-12'>
        <NLPSearchForm
          ref={nlpFormRef}
          onSubmit={handleNLPSearchFormSubmit}
          inputValue={question}
          onInputChange={e => setQuestion(e.target.value)}
          isProcessing={isProcessing}
        />
      </section>
      <section className='w-full md:max-w-screen-md lg:max-w-screen-lg mb-12'>
        {(qaResponse || chatAnswer) && (
          <QAResponseDiv
            qaResponse={qaResponse}
            chatAnswer={chatAnswer}
            className='w-full'
          />
        )}
      </section>
    </>
  )
}
