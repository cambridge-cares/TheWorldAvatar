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
            <h2 className="text-xl font-semibold text-blue-500">Processing steps</h2>
            <Accordion type="multiple">
              <AccordionItem value="translation_context">
                <AccordionTrigger>Translation context</AccordionTrigger>
                <AccordionContent className="px-4">
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
                        <JSONTree data={qaResponse.metadata.translation_context.examples} />
                      </AccordionContent>
                    </AccordionItem>
                  </Accordion>
                </AccordionContent>
              </AccordionItem>
              <AccordionItem value="prediction">
                <AccordionTrigger>Predicted structured query</AccordionTrigger>
                <AccordionContent className="px-4">
                  <Accordion type="multiple">
                    <AccordionItem value="entity_bindings">
                      <AccordionTrigger>Entity recognition and linking</AccordionTrigger>
                      <AccordionContent>
                        <DataTable
                          columns={[
                            { value: "var", label: "Variable" },
                            { value: "cls", label: "Class" },
                            { value: "mention", label: "Mention" },
                            { value: "linked_iris", label: "Linked IRIs" }
                          ]}
                          data={Object
                            .entries(qaResponse.metadata.data_request.entity_bindings)
                            .map(([varname, binding]) => ({
                              var: varname,
                              cls: binding.cls,
                              mention: binding.values.flatMap(val => (val.text ? [val.text] : []).concat(Object.entries(val.identifier).map(([k, v]) => `${k}: ${v}`))),
                              linked_iris: qaResponse.metadata.linked_variables[varname]
                            }))
                          }
                        />
                      </AccordionContent>
                    </AccordionItem>
                    <AccordionItem value="data_req_form">
                      <AccordionTrigger>Structured query form</AccordionTrigger>
                      <AccordionContent className="px-6">
                        {qaResponse.metadata.data_request.req_form.type === "sparql" ? (
                          <>
                            <h4 className="font-medium">Namespace</h4>
                            <p className="mb-2">{qaResponse.metadata.data_request.req_form.namespace}</p>
                            <h4 className="font-medium">SPARQL query</h4>
                            <p className="font-mono whitespace-pre bg-slate-50 p-4">{qaResponse.metadata.data_request.req_form.query}</p>
                          </>
                        ) : (<></>)}
                      </AccordionContent>
                    </AccordionItem>
                  </Accordion>
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
                          <AccordionTrigger>{headerText}</AccordionTrigger>
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