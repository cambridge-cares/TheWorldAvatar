"use client"

import * as React from "react"

import Markdown from "react-markdown"

import { QAResponse } from "@/lib/model"
import { JSONTree } from "@/components/ui/json-tree"
import { DataTable } from "@/components/ui/data-table"
import { cn, makePrefixedIRI } from "@/lib/utils"
import { Accordion, AccordionContent, AccordionItem, AccordionTrigger } from "@/components/ui/accordion"

export interface QAResponseDivProps extends React.HTMLAttributes<HTMLDivElement> {
  qaResponse?: QAResponse
  chatAnswer?: string
}


export function QAResponseDiv({ qaResponse, chatAnswer, className, ...props }: QAResponseDivProps) {
  const chatRef = React.useRef<null | HTMLDivElement>(null)

  React.useEffect(() => {
    chatRef.current?.scrollIntoView()
  }, [chatAnswer])

  return (
    <div className={cn("flex flex-col space-y-6", className)} {...props}>
      {qaResponse && (
        <>
          <div>
            <h2 className="text-xl font-semibold text-blue-500">Reasoning steps</h2>
            <Accordion type="multiple">
              <AccordionItem value="schema_relations">
                <AccordionTrigger>Relations retrieved from knowledge base with highest relevance</AccordionTrigger>
                <AccordionContent>
                  <DataTable
                    columns={[{ value: "s", label: "Subject" }, { value: "p", label: "Predicate" }, { value: "o", label: "Object" }]}
                    data={qaResponse.metadata.translation_context.schema_relations.map(obj => Object.fromEntries(Object.entries(obj).map(([k, v]) => [k, makePrefixedIRI(v)])))}
                  />
                </AccordionContent>
              </AccordionItem>
              <AccordionItem value="examples">
                <AccordionTrigger>Semantic parsing examples retrieved from database with highest relevance</AccordionTrigger>
                <AccordionContent>
                  <DataTable
                    columns={[
                      { value: "nlq", label: "Natural language query" },
                      { value: "entity_bindings", label: "Entity recognition" },
                      { value: "req_form", label: "Structured query" },
                    ]}
                    data={
                      qaResponse.metadata.translation_context.examples.map(example => ({
                        "nlq": example.nlq,
                        "entity_bindings": {
                          columns: [{ value: "var", label: "Variable" }, { value: "cls", label: "Class" }, { value: "values", label: "Values" }],
                          data: Object.entries(example.data_req.entity_bindings).map(([k, v]) => ({
                            var: k,
                            cls: v.cls,
                            values: v.values.flatMap(val => (val.text ? [val.text] : []).concat(Object.entries(val.identifier).map(([k, v]) => `${k}: ${v}`)))
                          }))
                        },
                        "req_form": JSON.stringify(example.data_req.req_form)
                      }))
                    }
                  />
                </AccordionContent>
              </AccordionItem>
              <AccordionItem value="prediction">
                <AccordionTrigger>Predicted structured query</AccordionTrigger>
                <AccordionContent>
                  {JSON.stringify(qaResponse.metadata.data_request)}
                </AccordionContent>
              </AccordionItem>
              <AccordionItem value="variables">
                <AccordionTrigger>Entity linking</AccordionTrigger>
                <AccordionContent>
                  {JSON.stringify(qaResponse.metadata.linked_variables)}
                </AccordionContent>
              </AccordionItem>
            </Accordion>
          </div>
          <div>
            <h2 className="text-xl font-semibold text-blue-500">Retrieved data</h2>
            <Accordion type="multiple">
              {
                qaResponse.data
                  .filter(item => item.type === "document_collection" || item.type === "table")
                  .map((item, idx) => {
                    let headerText, component
                    if (item.type === "document_collection") {
                      headerText = "JSON data"
                      component = (<JSONTree data={item.data} />)
                    } else if (item.type === "table") {
                      headerText = "Tabular data"
                      component = (<DataTable columns={item.columns} data={item.data} />)
                    }
                    return (
                      (headerText && component) ? (
                        <AccordionItem key={idx} value={idx.toString()}>
                          <AccordionTrigger className="text-lg font-semibold text-blue-500">{headerText}</AccordionTrigger>
                          <AccordionContent className="py-2">
                            {component}
                          </AccordionContent>
                        </AccordionItem>
                      ) : (<></>)
                    )
                  })
              }
            </Accordion>
          </div>
        </>
      )}
      {
        chatAnswer && (
          <div ref={chatRef}>
            <h2 className="text-xl font-semibold text-blue-500 mb-2">Marie&apos;s response</h2>
            <Markdown className="prose max-w-none prose-sm prose-slate">{chatAnswer}</Markdown>
          </div>
        )
      }
    </div >
  )
}