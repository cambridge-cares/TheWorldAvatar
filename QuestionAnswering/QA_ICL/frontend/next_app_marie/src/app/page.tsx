import { promises as fs } from 'fs';
import path from "path";

import React from "react";
import { Accordion, AccordionContent, AccordionItem, AccordionTrigger } from "@/components/ui/accordion";
import QAFragment from './_components/qa';
import IntroSection from "../components/intro-section";

interface ExampleQuestionGroup {
  heading: string
  questions: string[]
}

export default async function Home() {
  const introText = await fs.readFile(path.join(process.cwd(), "resources/intro-text.md"), "utf8")
  const exampleQuestionGroups: ExampleQuestionGroup[] = JSON.parse(await fs.readFile(path.join(process.cwd(), "resources/example-questions.json"), "utf-8"))

  return (
    <main className="p-4 min-h-screen flex flex-col justify-center items-center">
      <IntroSection imgSrc='/images/marie-thumbnail.jpg' imgAlt="Marie's thumbnail" introTextMd={introText} />
      <section className="max-w-3xl w-full">
        <Accordion type="single" collapsible className='w-full mb-8'>
          <AccordionItem value="0">
            <AccordionTrigger>Example Questions</AccordionTrigger>
            <AccordionContent className='px-4'>
              <Accordion type="single" collapsible className="w-full">
                {exampleQuestionGroups.map((grp, idx) => (
                  <AccordionItem key={idx} value={idx.toString()}>
                    <AccordionTrigger>{grp.heading}</AccordionTrigger>
                    <AccordionContent className='px-8'>
                      <ul className='list-disc list-outside'>
                        {grp.questions.map((qn, i) => (<li key={i} >{qn}</li>))}
                      </ul>
                    </AccordionContent>
                  </AccordionItem>
                ))}
              </Accordion>
            </AccordionContent>
          </AccordionItem>
        </Accordion>
      </section>
      <QAFragment />
    </main>
  );
}
