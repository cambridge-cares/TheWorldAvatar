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


export interface AdditionalInfoProps extends React.HTMLAttributes<HTMLElement> {
  historyInfoMdContent: string
}

export const AdditionalInfoSection = ({
  historyInfoMdContent,
  ...props
}: AdditionalInfoProps) => (
  <section {...props}>
    <Accordion type='single' collapsible>
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
