'use client'

import * as React from 'react'

import {
  Accordion,
  AccordionContent,
  AccordionItem,
  AccordionTrigger,
} from '@/components/ui/accordion'
import Markdown from 'react-markdown'
import { Tabs, TabsContent, TabsList, TabsTrigger } from '@/components/ui/tabs'

export type TBoxInfoTabsProps = React.ComponentPropsWithoutRef<typeof Tabs> & {
  data: { heading: string; mdContent: string }[]
}

export const TBoxInfoTabs = ({ data, ...props }: TBoxInfoTabsProps) => (
  <Tabs defaultValue='0' {...props}>
    <TabsList className='inline-flex mb-2'>
      {data.map(({ heading }, i) => (
        <TabsTrigger key={i} value={i.toString()}>
          {heading}
        </TabsTrigger>
      ))}
    </TabsList>
    {data.map(({ mdContent }, i) => (
      <TabsContent key={i} value={i.toString()}>
        <Markdown className='prose prose-sm max-w-none'>{mdContent}</Markdown>
      </TabsContent>
    ))}
  </Tabs>
)

export interface AdditionalInfoProps extends React.HTMLAttributes<HTMLElement> {
  tboxInfoData: { heading: string; mdContent: string }[]
  historyInfoMdContent: string
}

export const AdditionalInfoSection = ({
  tboxInfoData,
  historyInfoMdContent,
  ...props
}: AdditionalInfoProps) => (
  <section {...props}>
    <Accordion type='single' collapsible>
      <AccordionItem value='tbox_info'>
        <AccordionTrigger>Information on Chemistry Ontologies</AccordionTrigger>
        <AccordionContent className='px-4'>
          <TBoxInfoTabs data={tboxInfoData} />
        </AccordionContent>
      </AccordionItem>
      <AccordionItem value='history_info'>
        <AccordionTrigger>History of Marie</AccordionTrigger>
        <AccordionContent className='px-6'>
          <Markdown className='prose prose-sm max-w-none'>
            {historyInfoMdContent}
          </Markdown>
        </AccordionContent>
      </AccordionItem>
    </Accordion>
  </section>
)
