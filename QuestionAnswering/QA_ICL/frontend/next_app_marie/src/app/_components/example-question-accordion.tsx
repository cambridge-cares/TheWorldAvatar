"use client"

import React from "react";
import { Accordion, AccordionContent, AccordionItem, AccordionTrigger } from "@/components/ui/accordion";


export interface ExampleQuestionGroup {
  heading: string
  questions: string[]
}

export type ExampleQuestionAccordionProps = React.ComponentPropsWithoutRef<typeof Accordion> & {
  data: ExampleQuestionGroup[]
  questionOnClick: (question: string, event: React.MouseEvent<HTMLLIElement>) => void
}

export function ExampleQuestionAccordion({ data, questionOnClick, ...props }: ExampleQuestionAccordionProps) {
  return (
    <Accordion {...props}>
      {data.map((grp, idx) => (
        <AccordionItem key={idx} value={idx.toString()}>
          <AccordionTrigger>{grp.heading}</AccordionTrigger>
          <AccordionContent className='px-8'>
            <ul className='list-disc list-outside'>
              {grp.questions.map((qn, i) => (
                <li
                  key={i}
                  onClick={e => questionOnClick(qn, e)}
                  className="hover:underline hover:cursor-pointer"
                >
                  {qn}
                </li>
              ))}
            </ul>
          </AccordionContent>
        </AccordionItem>
      ))}
    </Accordion>
  )
}