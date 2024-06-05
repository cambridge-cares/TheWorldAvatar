"use client";

import React from "react";
import { DataItem } from "@/lib/model";
import { queryChat, queryQa } from "@/lib/api";
import { DataTable } from "@/components/ui/data-table";
import { Button } from "@/components/ui/button";
import { Input } from "@/components/ui/input"
import { MagnifyingGlassIcon, ReloadIcon } from "@radix-ui/react-icons";
import { JSONTree } from "@/components/ui/json-tree";
import { ScrollArea } from "@/components/ui/scroll-area";
import Markdown from "react-markdown";


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

export default function QAFragment() {
  const [question, setQuestion] = React.useState("")
  const [isQueryingQA, setIsQueryingQA] = React.useState(false)
  const [qaData, setQaData] = React.useState<DataItem[] | null>(null)
  const [chatAnswer, setChatAnswer] = React.useState<string | null>(null)

  const handleFormSubmit = async (e: React.FormEvent<HTMLFormElement>) => {
    try {
      e.preventDefault()
      setIsQueryingQA(true)
      setQaData(null)
      setChatAnswer(null)

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


  return (
    <>
      <div className="max-w-3xl mb-12 w-full">
        <form className="flex justify-center items-center" onSubmit={handleFormSubmit}>
          <Input placeholder="Type your query..." required className="mr-2" onChange={e => setQuestion(e.target.value)} />
          <Button type="submit" variant="outline" size="icon" disabled={isQueryingQA}><MagnifyingGlassIcon /></Button>
        </form>
      </div>
      <div className="max-w-5xl mb-12 w-full flex flex-col space-y-4 justify-center">
        {isQueryingQA && (<ReloadIcon className="self-center mr-2 h-4 w-4 animate-spin" />)}
        {qaData && qaData.map((item, idx) => <DataItemComponent key={idx} dataItem={item} />)}
        {chatAnswer && (
          <div className="w-full rounded-md border p-4">
            <p className="text-xl font-semibold text-blue-500">Marie&apos;s response</p>
            <ScrollArea className="h-96 w-full"><Markdown className="prose max-w-none prose-sm prose-slate">{chatAnswer}</Markdown></ScrollArea>
          </div>
        )}
      </div>
    </>
  )
}