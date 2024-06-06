'use client'

import * as React from 'react'

import Markdown from 'react-markdown'

import { QAResponse } from '@/lib/model'
import { cn } from '@/lib/utils'
import { QAResponseMetadataDiv } from './qa-response-metadata-div'
import { QAResponseDataDiv } from './qa-response-data-div'

export interface QAResponseDivProps
  extends React.HTMLAttributes<HTMLDivElement> {
  qaResponse?: QAResponse
  chatAnswer?: string
}

export function QAResponseDiv({
  qaResponse,
  chatAnswer,
  className,
  ...props
}: QAResponseDivProps) {
  const chatRef = React.useRef<null | HTMLDivElement>(null)

  React.useEffect(() => {
    chatRef.current?.scrollIntoView()
  }, [chatAnswer])

  return (
    <div className={cn('flex flex-col space-y-6', className)} {...props}>
      {qaResponse && (
        <>
          <QAResponseMetadataDiv qaResponseMetadata={qaResponse.metadata} />
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
        </div>
      )}
    </div>
  )
}
