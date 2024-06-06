'use client'

import * as React from 'react'

import { QAResponse } from '@/lib/model'
import { queryChat, queryQa } from '@/lib/api'
import {
  Accordion,
  AccordionContent,
  AccordionItem,
  AccordionTrigger,
} from '@/components/ui/accordion'
import { NLPSearchForm } from './nlp-search-form'
import {
  ExampleQuestionAccordion,
  ExampleQuestionGroup,
} from './example-question-accordion'
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
      <section className='max-w-3xl w-full'>
        <Accordion type='single' collapsible className='w-full mb-8'>
          <AccordionItem value='0'>
            <AccordionTrigger>Example Questions</AccordionTrigger>
            <AccordionContent className='px-4'>
              <ExampleQuestionAccordion
                type='single'
                collapsible
                data={exampleQuestionGroups}
                questionOnClick={handleExampleQuestionClick}
              />
            </AccordionContent>
          </AccordionItem>
        </Accordion>
      </section>
      <section className='max-w-3xl mb-12 w-full'>
        <NLPSearchForm
          ref={nlpFormRef}
          onSubmit={handleNLPSearchFormSubmit}
          inputValue={question}
          onInputChange={e => setQuestion(e.target.value)}
          isProcessing={isProcessing}
        />
      </section>
      <section className='mb-12 w-full flex flex-col space-y-4 justify-center'>
        {(qaResponse || chatAnswer) && (
          <div className='flex justify-center'>
            <QAResponseDiv
              qaResponse={qaResponse}
              chatAnswer={chatAnswer}
              className='p-4 w-full md:w-3/4 lg:w-2/4'
            />
          </div>
        )}
      </section>
    </>
  )
}
