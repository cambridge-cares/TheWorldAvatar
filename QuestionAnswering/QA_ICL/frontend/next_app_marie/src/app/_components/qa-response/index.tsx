'use client'

import * as React from 'react'
import Markdown from 'react-markdown'

import { QAResponse } from '@/lib/model/qa'

import { StopIcon } from '@radix-ui/react-icons'
import { QAResponseMetadataDiv } from './metadata'
import { QAResponseVisualisationDiv } from './vis'
import { QAResponseDataDiv } from './data'

export interface QAResponseDivProps {
  qaResponse?: QAResponse
  chatAbortController?: AbortController
  chatStream?: ReadableStreamDefaultReader<string>
}

export function QAResponseFragment({
  qaResponse,
  chatAbortController,
  chatStream,
}: QAResponseDivProps) {
  const chatRef = React.useRef<null | HTMLDivElement>(null)
  const [isGeneratingChat, setIsGeneratingChat] = React.useState<boolean>(false)
  const [chatAnswer, setChatAnswer] = React.useState<string | undefined>(
    undefined
  )

  React.useEffect(() => {
    if (!chatStream) return

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
              console.log('Unexpected data received from server: '.concat(msg))
            }
          }
        })
      }
      return chatStream.read().then(pump)
    }

    const readStream = async () => {
      setIsGeneratingChat(true)
      try {
        await chatStream.read().then(pump)
      } catch (err) {
      } finally {
        setIsGeneratingChat(false)
      }
    }

    readStream()
  }, [chatStream])

  React.useEffect(() => {
    chatRef.current?.scrollIntoView(false)
  }, [chatAnswer])

  const handleAbort = () => {
    chatAbortController?.abort()
    setIsGeneratingChat(false)
  }

  return (
    <>
      {qaResponse && (
        <>
          <QAResponseMetadataDiv qaResponseMetadata={qaResponse.metadata} />
          <QAResponseVisualisationDiv visData={qaResponse.visualisation} />
          <QAResponseDataDiv qaResponseData={qaResponse.data} />
        </>
      )}
      {chatAnswer && (
        <div ref={chatRef}>
          <h2 className='text-xl font-semibold text-blue-500 mb-2'>
            Marie&apos;s response
          </h2>
          <Markdown className='prose max-w-none prose-sm prose-slate'>
            {chatAnswer}
          </Markdown>
          {isGeneratingChat && (
            <div
              onClick={handleAbort}
              className='mb-4 flex items-center justify-center space-x-2 hover:cursor-pointer'
            >
              <StopIcon className='h-4 w-4' />
              <p>Stop generating</p>
            </div>
          )}
        </div>
      )}
    </>
  )
}
