'use client'

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
