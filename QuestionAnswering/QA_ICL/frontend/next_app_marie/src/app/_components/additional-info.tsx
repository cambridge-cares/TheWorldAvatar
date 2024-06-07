'use client'

import * as React from 'react'

import {
  Accordion,
  AccordionContent,
  AccordionItem,
  AccordionTrigger,
} from '@/components/ui/accordion'
import Markdown from 'react-markdown'

export type TBoxInfoAccordionProps = React.ComponentPropsWithoutRef<
  typeof Accordion
> & {
  data: { heading: string; mdContent: string }[]
}

export const TBoxInfoAccordion = ({
  data,
  ...props
}: TBoxInfoAccordionProps) => (
  <Accordion {...props}>
    {data.map(({ heading, mdContent }, i) => (
      <AccordionItem key={i} value={i.toString()}>
        <AccordionTrigger>{heading}</AccordionTrigger>
        <AccordionContent className='px-6'>
          <Markdown className='prose prose-sm max-w-none'>{mdContent}</Markdown>
        </AccordionContent>
      </AccordionItem>
    ))}
  </Accordion>
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
          <TBoxInfoAccordion type='single' collapsible data={tboxInfoData} />
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
