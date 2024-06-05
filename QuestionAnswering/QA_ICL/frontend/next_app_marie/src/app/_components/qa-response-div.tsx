"use client"

import * as React from "react"

import Markdown from "react-markdown"

import { DataItem } from "@/lib/model"
import { JSONTree } from "@/components/ui/json-tree"
import { DataTable } from "@/components/ui/data-table"
import { cn } from "@/lib/utils"
import { Accordion, AccordionContent, AccordionItem, AccordionTrigger } from "@/components/ui/accordion"

export interface QAResponseDivProps extends React.HTMLAttributes<HTMLDivElement> {
  structuredData?: DataItem[]
  chatAnswer?: string
}


export function QAResponseDiv({ structuredData, chatAnswer, className, ...props }: QAResponseDivProps) {
  const chatRef = React.useRef<null | HTMLDivElement>(null)

  React.useEffect(() => {
    chatRef.current?.scrollIntoView()
  }, [chatAnswer])

  return (
    <div className={cn("flex flex-col space-y-6", className)} {...props}>
      {structuredData && (
        <div>
          <p className="text-xl font-semibold text-blue-500">Retrieved data</p>
          <Accordion type="multiple">
            {
              structuredData
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
      )}
      {
        chatAnswer && (
          <div ref={chatRef}>
            <p className="text-xl font-semibold text-blue-500 mb-2">Marie&apos;s response</p>
            <Markdown className="prose max-w-none prose-sm prose-slate">{chatAnswer}</Markdown>
          </div>
        )
      }
    </div >
  )
}