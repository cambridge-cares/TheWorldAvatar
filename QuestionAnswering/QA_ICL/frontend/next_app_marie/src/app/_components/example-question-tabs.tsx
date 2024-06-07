'use client'

import React from 'react'

import { Tabs, TabsList, TabsContent, TabsTrigger } from '@/components/ui/tabs'
import { cn } from '@/lib/utils'

export interface ExampleQuestionGroup {
  heading: string
  questions: string[]
}

export type ExampleQuestionTabsProps = React.ComponentPropsWithoutRef<
  typeof Tabs
> & {
  data: ExampleQuestionGroup[]
  questionOnClick: (
    question: string,
    event: React.MouseEvent<HTMLLIElement>
  ) => void
}

export const ExampleQuestionTabs = ({
  data,
  questionOnClick,
  className,
  ...props
}: ExampleQuestionTabsProps) => (
  <Tabs
    orientation='vertical'
    defaultValue='0'
    className={cn('grid lg:grid-cols-4 gap-4', className)}
    {...props}
  >
    <div>
      <TabsList className='flex lg:flex-col space-y-1'>
        {data.map(({ heading }, i) => (
          <TabsTrigger key={i} value={i.toString()}>
            {heading}
          </TabsTrigger>
        ))}
      </TabsList>
    </div>
    <div className='lg:col-span-3'>
      {data.map(({ questions }, i) => (
        <TabsContent key={i} value={i.toString()}>
          <ul className='list-disc list-inside'>
            {questions.map((qn, i) => (
              <li
                key={i}
                onClick={e => questionOnClick(qn, e)}
                className='hover:underline hover:cursor-pointer'
              >
                {qn}
              </li>
            ))}
          </ul>
        </TabsContent>
      ))}
    </div>
  </Tabs>
)
