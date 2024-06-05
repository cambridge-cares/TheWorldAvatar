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
      <QAFragment />
      <div className="max-w-2xl w-full">
        <h2>Example Questions</h2>
        <Accordion type="single" collapsible className="w-full">
          {exampleQuestionGroups.map((grp, idx) => (
            <AccordionItem key={idx} value={idx.toString()}>
              <AccordionTrigger>{grp.heading}</AccordionTrigger>
              <AccordionContent>
                <ul className='list-disc list-inside'>
                  {grp.questions.map((qn, i) => (<li key={i} >{qn}</li>))}
                </ul>
              </AccordionContent>
            </AccordionItem>
          ))}
        </Accordion>
      </div>
    </main>
  );
}
