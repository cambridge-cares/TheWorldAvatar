"use client";

import * as React from "react";
import Markdown from "react-markdown";
import { DataItem } from "@/lib/model";
import { queryChat, queryQa } from "@/lib/api";
import { DataTable } from "@/components/ui/data-table";
import { ReloadIcon } from "@radix-ui/react-icons";
import { JSONTree } from "@/components/ui/json-tree";
import { ScrollArea } from "@/components/ui/scroll-area";
import { NLPSearchForm } from "./nlp-search-form";
import { Accordion, AccordionContent, AccordionItem, AccordionTrigger } from "@/components/ui/accordion";
import { ExampleQuestionAccordion, ExampleQuestionGroup } from "./example-question-accordion";


const MemoDataTable = React.memo(DataTable)


interface DataItemComponentInterface {
  dataItem: DataItem
}

function DataItemComponent({ dataItem, ...props }: DataItemComponentInterface) {
  let headerText, component
  if (dataItem.type === "document_collection") {
    headerText = "JSON data"
    component = (<JSONTree data={dataItem.data} />)
  } else if (dataItem.type === "table") {
    headerText = "Tabular data"
    component = (<MemoDataTable columns={dataItem.columns} data={dataItem.data} />)
  }
  return headerText && component
    ? (
      <div {...props}>
        <p className="text-lg font-semibold text-blue-500">{headerText}</p>
        {component}
      </div>
    ) : <></>
}

export interface QAFragmentProps {
  exampleQuestionGroups: ExampleQuestionGroup[]
}

export default function QAFragment({ exampleQuestionGroups }: QAFragmentProps) {
  const [question, setQuestion] = React.useState("")
  const [isQueryingQA, setIsQueryingQA] = React.useState(false)
  const [qaData, setQaData] = React.useState<DataItem[] | null>(null)
  const [chatAnswer, setChatAnswer] = React.useState<string | null>(null)

  const queryDataThenDisplay = async () => {
    setIsQueryingQA(true)
    setQaData(null)
    setChatAnswer(null)
    try {
      const qaRes = await queryQa(question)
      setIsQueryingQA(false)
      setQaData(qaRes.data)

      const textStream = await queryChat(qaRes.request_id)

      const pump = ({ done, value }: { done: boolean, value?: string }): Promise<void> => {
        if (done) {
          return Promise.resolve()
        }

        // TODO: Use TransformerStream to do the parsing in API call code
        if (value) {
          value.split("\n").forEach(line => {
            const trimmedLine = line.trim()
            if (trimmedLine.startsWith("data: ")) {
              const msg = trimmedLine.substring("data: ".length)
              try {
                const dataChunk = JSON.parse(msg)
                const content = dataChunk["content"]
                if (typeof content === "string") {
                  setChatAnswer(oldValue => (oldValue || "") + content)
                }
              } catch (err) {
                console.log("Unexpected data received from server: ".concat(msg))
              }
            }
          })
        }
        return textStream.read().then(pump)
      }
      await textStream.read().then(pump)
    } catch (err) {

    } finally {
      setIsQueryingQA(false)
    }
  }

  const handleExampleQuestionClick = async (qn: string, e: React.MouseEvent<HTMLLIElement>) => {
    e.preventDefault()
    setQuestion(qn)
    await queryDataThenDisplay()
  }

  const handleNLPSearchFormSubmit = async (e: React.FormEvent<HTMLFormElement>) => {
    e.preventDefault()
    await queryDataThenDisplay()
  }

  return (
    <>
      <section className="max-w-3xl w-full">
        <Accordion type="single" collapsible className='w-full mb-8'>
          <AccordionItem value="0">
            <AccordionTrigger>Example Questions</AccordionTrigger>
            <AccordionContent className='px-4'>
              <ExampleQuestionAccordion
                type="single"
                collapsible
                data={exampleQuestionGroups}
                questionOnClick={handleExampleQuestionClick}
              />
            </AccordionContent>
          </AccordionItem>
        </Accordion>
      </section>
      <section className="max-w-3xl mb-12 w-full">
        <NLPSearchForm onSubmit={handleNLPSearchFormSubmit} inputValue={question} onInputChange={e => setQuestion(e.target.value)} disabled={isQueryingQA} />
      </section>
      <section className="max-w-5xl mb-12 w-full flex flex-col space-y-4 justify-center">
        {isQueryingQA && (<ReloadIcon className="self-center mr-2 h-4 w-4 animate-spin" />)}
        {qaData && qaData.map((item, idx) => <DataItemComponent key={idx} dataItem={item} />)}
        {chatAnswer && (
          <div className="w-full rounded-md border p-4">
            <p className="text-xl font-semibold text-blue-500">Marie&apos;s response</p>
            <ScrollArea className="h-96 w-full"><Markdown className="prose max-w-none prose-sm prose-slate">{chatAnswer}</Markdown></ScrollArea>
          </div>
        )}
      </section>
    </>
  )
}