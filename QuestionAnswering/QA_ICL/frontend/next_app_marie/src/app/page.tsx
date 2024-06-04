import React from "react";
import { Accordion, AccordionContent, AccordionItem, AccordionTrigger } from "@/components/ui/accordion";
import QAFragment from './components/qa';
import IntroSection from "./components/intro-section";


export default async function Home() {
  return (
    <main className="p-4 min-h-screen flex flex-col justify-center items-center">
      <IntroSection />
      <QAFragment />
      <div className="max-w-2xl w-full">
        <h2>Example Questions</h2>
        <Accordion type="single" collapsible className="w-full">
          <AccordionItem value="item-1">
            <AccordionTrigger>Chemical Species Properties</AccordionTrigger>
            <AccordionContent>
              <ul>
                <li>What is the charge of benzene?</li>
                <li>Provide a roster of chemical entities used as pH regulating agent</li>
              </ul>
            </AccordionContent>
          </AccordionItem>

          <AccordionItem value="item-2">
            <AccordionTrigger>Gas-Phase Reaction Mechanisms</AccordionTrigger>
            <AccordionContent>
              <ul>
                <li>List all mechanisms that involve O, H, and AR</li>
                <li>What are reactions in which O2 is a reactant and OH is a product</li>
              </ul>
            </AccordionContent>
          </AccordionItem>
        </Accordion>
      </div>
    </main>
  );
}
