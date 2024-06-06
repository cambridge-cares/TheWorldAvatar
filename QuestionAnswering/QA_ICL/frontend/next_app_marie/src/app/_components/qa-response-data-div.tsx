'use client'

import * as React from 'react'

import { DataItem } from '@/lib/model'
import {
  Accordion,
  AccordionContent,
  AccordionItem,
  AccordionTrigger,
} from '@/components/ui/accordion'
import { DataTable } from '@/components/ui/data-table'
import { JSONTree } from '@/components/ui/json-tree'

export interface QAResponseDataDivProps
  extends React.HTMLAttributes<HTMLDivElement> {
  qaResponseData: DataItem[]
}

export function QAResponseDataDiv({
  qaResponseData,
  ...props
}: QAResponseDataDivProps) {
  return (
    <div {...props}>
      <h2 className='text-xl font-semibold text-blue-500'>Retrieved data</h2>
      <Accordion type='multiple'>
        {qaResponseData
          .filter(
            item => item.type === 'document_collection' || item.type === 'table'
          )
          .map((item, idx) => {
            let headerText, component
            if (item.type === 'document_collection') {
              headerText = 'JSON data'
              component = <JSONTree data={item.data} />
            } else if (item.type === 'table') {
              headerText = 'Tabular data'
              component = <DataTable columns={item.columns} data={item.data} />
            }
            return headerText && component ? (
              <AccordionItem key={idx} value={idx.toString()}>
                <AccordionTrigger>{headerText}</AccordionTrigger>
                <AccordionContent className='py-2'>
                  {component}
                </AccordionContent>
              </AccordionItem>
            ) : (
              <></>
            )
          })}
      </Accordion>
    </div>
  )
}
