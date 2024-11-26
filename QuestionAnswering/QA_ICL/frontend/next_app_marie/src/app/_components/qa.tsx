'use client'

import * as React from 'react'

import { QAResponse } from '@/lib/model/qa'
import { queryQa } from '@/lib/api/qa'
import { queryChat } from '@/lib/api/chat'
import { NLPSearchForm } from './nlp-search-form'
import {
  ExampleQuestionTabs,
  ExampleQuestionGroup,
} from './example-question-tabs'
import { QAResponseFragment } from './qa-response'
import { InternalServerError } from '@/lib/api'
import { useToast } from '@/components/ui/use-toast'

export interface QAFragmentProps {
  exampleQuestionGroups: ExampleQuestionGroup[]
}

export function QAFragment({ exampleQuestionGroups }: QAFragmentProps) {
  const { toast } = useToast()
  const nlpFormRef = React.useRef<null | HTMLFormElement>(null)

  const [question, setQuestion] = React.useState('')
  const [isProcessing, setIsProcessing] = React.useState(false)
  const [qaResponse, setQaResponse] = React.useState<QAResponse | undefined>(
    undefined
  )
  const [chatAbortController, setChatAbortController] = React.useState<
    AbortController | undefined
  >(undefined)
  const [chatStream, setChatStream] = React.useState<
    ReadableStreamDefaultReader<string> | undefined
  >(undefined)

  const queryDataThenDisplay = async (question: string) => {
    if (isProcessing) return

    setIsProcessing(true)
    setQaResponse(undefined)
    setChatAbortController(undefined)
    setChatStream(undefined)
    try {
      const qaRes = await queryQa(question)
      setQaResponse(qaRes)

      const [abortController, textStreamPromise] = queryChat(qaRes.request_id)
      setChatAbortController(abortController)
      const textStream = await textStreamPromise
      setChatStream(textStream)
    } catch (err) {
      if (err instanceof Error)
        toast({
          variant: 'destructive',
          title: err.constructor.name,
          description:
            err instanceof InternalServerError
              ? 'Please try again'
              : err.message,
        })
      else console.log('Unexpected error: ', err)
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
          disabled={isProcessing}
          questionOnClick={handleExampleQuestionClick}
        />
      </section>
      <section className='w-full md:max-w-screen-sm lg:max-w-screen-md'>
        <NLPSearchForm
          ref={nlpFormRef}
          onSubmit={handleNLPSearchFormSubmit}
          inputValue={question}
          onInputChange={e => setQuestion(e.target.value)}
          isProcessing={isProcessing}
        />
      </section>
      {(qaResponse || chatStream) && (
        <section className='w-full md:max-w-screen-md lg:max-w-screen-lg mt-12 flex flex-col space-y-6'>
          <QAResponseFragment
            qaResponse={qaResponse}
            chatAbortController={chatAbortController}
            chatStream={chatStream}
          />
        </section>
      )}
    </>
  )
}
