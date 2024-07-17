'use client'

import * as React from 'react'

import Markdown from 'react-markdown'

import {
  ChemStructData,
  DataItem,
  QAResponse,
  QAResponseMetadata,
} from '@/lib/model/qa'
import { cn, isObjectEmtpy, makePrefixedIRI } from '@/lib/utils'
import {
  Accordion,
  AccordionContent,
  AccordionItem,
  AccordionTrigger,
} from '@/components/ui/accordion'
import { DataTable } from '@/components/ui/data-table'
import { DataTableRecursive } from '@/components/ui/data-table-recursive'
import { JSONTree } from '@/components/ui/json-tree'
import { MolViewer } from '@/components/ui/mol-viewer'
import { StopIcon } from '@radix-ui/react-icons'
import { ToggleGroup, ToggleGroupItem } from '@/components/ui/toggle-group'
import {
  Select,
  SelectContent,
  SelectItem,
  SelectTrigger,
  SelectValue,
} from '@/components/ui/select'

export interface QAResponseMetadataDivProps
  extends React.HTMLAttributes<HTMLDivElement> {
  qaResponseMetadata: QAResponseMetadata
}

export const QAResponseMetadataDiv = ({
  qaResponseMetadata,
  ...props
}: QAResponseMetadataDivProps) => (
  <div {...props}>
    <h2 className='text-xl font-semibold text-blue-500'>Processing steps</h2>
    <Accordion type='multiple'>
      {qaResponseMetadata.rewritten_question && (
        <AccordionItem value='rewritten_question'>
          <AccordionTrigger>Rewritten input question</AccordionTrigger>
          <AccordionContent>
            <p>The input query has been rewritten into the following:</p>
            <p>&quot;{qaResponseMetadata.rewritten_question}&quot;</p>
          </AccordionContent>
        </AccordionItem>
      )}
      <AccordionItem value='translation_context'>
        <AccordionTrigger>Translation context</AccordionTrigger>
        <AccordionContent className='px-4'>
          <Accordion type='multiple'>
            <AccordionItem value='schema_relations'>
              <AccordionTrigger>
                Relations retrieved from knowledge base with highest relevance
              </AccordionTrigger>
              <AccordionContent>
                <DataTable
                  columns={[
                    { accessorKey: 'iri', header: 'IRI' },
                    { accessorKey: 'label', header: 'Label' },
                    { accessorKey: 'comment', header: 'Comment' },
                  ]}
                  data={qaResponseMetadata.translation_context.properties.map(
                    ([obj, _]) => ({
                      iri: makePrefixedIRI(obj.iri),
                      label: obj.label,
                      comment: obj.comment,
                    })
                  )}
                />
              </AccordionContent>
            </AccordionItem>
            <AccordionItem value='examples'>
              <AccordionTrigger>
                Semantic parsing examples retrieved from database with highest
                relevance
              </AccordionTrigger>
              <AccordionContent>
                <DataTable
                  columns={[
                    { accessorKey: 'nlq', header: 'Natural language question' },
                    { accessorKey: 'var2cls', header: 'Class assignment' },
                    {
                      accessorKey: 'entity_bindings',
                      header: 'Entity bindings',
                    },
                    {
                      accessorKey: 'req_form',
                      header: 'Structured query form',
                    },
                  ]}
                  data={qaResponseMetadata.translation_context.examples.map(
                    ([example, _]) => ({
                      nlq: example.nlq,
                      var2cls: <JSONTree data={example.data_req.var2cls} />,
                      entity_bindings: (
                        <JSONTree data={example.data_req.entity_bindings} />
                      ),
                      req_form: <JSONTree data={example.data_req.req_form} />,
                    })
                  )}
                />
              </AccordionContent>
            </AccordionItem>
          </Accordion>
        </AccordionContent>
      </AccordionItem>
      <AccordionItem value='prediction'>
        <AccordionTrigger>Predicted structured query</AccordionTrigger>
        <AccordionContent className='px-4'>
          <Accordion type='multiple'>
            <AccordionItem value='class_assignment'>
              <AccordionTrigger>Class assignment</AccordionTrigger>
              <AccordionContent>
                <DataTable
                  columns={[
                    { accessorKey: 'var', header: 'Variable' },
                    { accessorKey: 'cls', header: 'Class' },
                  ]}
                  data={Object.entries(
                    qaResponseMetadata.data_request.var2cls
                  ).map(([varname, cls]) => ({ var: varname, cls }))}
                />
              </AccordionContent>
            </AccordionItem>
            <AccordionItem value='entity_bindings'>
              <AccordionTrigger>
                Entity recognition and linking
              </AccordionTrigger>
              <AccordionContent>
                <DataTable
                  columns={[
                    { accessorKey: 'var', header: 'Variable' },
                    { accessorKey: 'cls', header: 'Class' },
                    { accessorKey: 'mention', header: 'Mention' },
                    { accessorKey: 'linked_iris', header: 'Linked IRIs' },
                  ]}
                  data={Object.entries(
                    qaResponseMetadata.data_request.entity_bindings
                  ).map(([varname, values]) => ({
                    var: varname,
                    cls: qaResponseMetadata.data_request.var2cls[varname],
                    mention: values.map(val =>
                      typeof val === 'string'
                        ? val
                        : Object.entries(val)
                            .map(([k, v]) => `${k}: ${v}`)
                            .join('\n')
                    ),
                    linked_iris: qaResponseMetadata.linked_variables[varname],
                  }))}
                />
              </AccordionContent>
            </AccordionItem>
            {qaResponseMetadata.data_request.req_form && (
              <AccordionItem value='data_req_form'>
                <AccordionTrigger>Structured query form</AccordionTrigger>
                <AccordionContent className='px-6'>
                  {qaResponseMetadata.data_request.req_form.type ===
                  'sparql' ? (
                    <>
                      <h4 className='font-medium'>Triplestore</h4>
                      <p className='mb-2'>
                        {qaResponseMetadata.data_request.req_form.triplestore}
                      </p>
                      <h4 className='font-medium'>SPARQL query</h4>
                      <p className='font-mono whitespace-pre bg-slate-50 p-4'>
                        {qaResponseMetadata.data_request.req_form.query}
                      </p>
                    </>
                  ) : (
                    <></>
                  )}
                </AccordionContent>
              </AccordionItem>
            )}
          </Accordion>
        </AccordionContent>
      </AccordionItem>
    </Accordion>
  </div>
)

export interface QAResponseVisualisationDivProps
  extends React.HTMLAttributes<HTMLDivElement> {
  visData: { [key: string]: ChemStructData[] }
}

export function QAResponseVisualisationDiv({
  visData,
  ...props
}: QAResponseVisualisationDivProps) {
  const iri2struct = React.useMemo(
    () =>
      Object.fromEntries(
        Object.values(visData).flatMap(structs =>
          structs.map(struct => [struct.iri, struct])
        )
      ),
    [visData]
  )

  const [curVar, setCurVar] = React.useState<string | undefined>(undefined)
  const [curIri, setCurIri] = React.useState<string | undefined>(undefined)

  React.useEffect(() => {
    for (const key in visData) {
      setCurVar(key)
      break
    }
  }, [visData])
  React.useEffect(() => {
    if (
      curVar === undefined ||
      !(curVar in visData) ||
      visData[curVar].length === 0
    )
      return
    setCurIri(visData[curVar][0].iri)
  }, [visData, curVar])

  if (isObjectEmtpy(visData)) return <></>
  return (
    <div {...props}>
      <h2 className='text-xl font-semibold text-blue-500 mb-2'>
        Chemical Structure Visualisation
      </h2>
      <div className='grid lg:grid-cols-3'>
        <div className='flex flex-col space-y-2'>
          {curVar && (
            <ToggleGroup
              type='single'
              defaultValue={curVar}
              onValueChange={val => {
                setCurIri(undefined)
                setCurVar(val)
              }}
              className='flex justify-start'
            >
              {Object.keys(visData).map((varname, i) => (
                <ToggleGroupItem key={i} value={varname}>
                  {varname}
                </ToggleGroupItem>
              ))}
            </ToggleGroup>
          )}
          {curVar && visData[curVar] && curIri && (
            <Select defaultValue={curIri} onValueChange={val => setCurIri(val)}>
              <SelectTrigger className='w-[280px]'>
                <SelectValue placeholder='Select structure to visualise' />
              </SelectTrigger>
              <SelectContent>
                {visData[curVar].map((chemStruct, i) => (
                  <SelectItem key={i} value={chemStruct.iri}>
                    {chemStruct.label}
                  </SelectItem>
                ))}
              </SelectContent>
            </Select>
          )}
        </div>
        <div className='lg:col-span-2'>
          {curIri && iri2struct[curIri] && (
            <MolViewer
              type={iri2struct[curIri].type}
              data={iri2struct[curIri].data}
            />
          )}
        </div>
      </div>
    </div>
  )
}

export interface QAResponseDataDivProps
  extends React.HTMLAttributes<HTMLDivElement> {
  qaResponseData: DataItem[]
}

export const QAResponseDataDiv = ({
  qaResponseData,
  ...props
}: QAResponseDataDivProps) => (
  <div {...props}>
    <h2 className='text-xl font-semibold text-blue-500'>Retrieved data</h2>
    <Accordion
      type='multiple'
      defaultValue={qaResponseData.map((_, idx) => idx.toString())}
    >
      {qaResponseData.map((item, idx) => {
        let headerText, component
        if (item.type === 'document_collection') {
          headerText = 'JSON data'
          component = (
            <JSONTree
              data={item.data}
              shouldExpandNodeInitially={() => false}
            />
          )
        } else if (item.type === 'table') {
          headerText = 'Tabular data'
          component = (
            <DataTableRecursive
              columns={item.columns}
              data={item.data}
              numbered
              paginated
              bordered
              scrollable
            />
          )
        }
        return headerText && component ? (
          <AccordionItem key={idx} value={idx.toString()}>
            <AccordionTrigger>{headerText}</AccordionTrigger>
            <AccordionContent className='py-2'>{component}</AccordionContent>
          </AccordionItem>
        ) : (
          <></>
        )
      })}
    </Accordion>
  </div>
)

export interface QAResponseDivProps
  extends React.HTMLAttributes<HTMLDivElement> {
  qaResponse?: QAResponse
  chatAbortController?: AbortController
  chatStream?: ReadableStreamDefaultReader<string>
}

export function QAResponseDiv({
  qaResponse,
  chatAbortController,
  chatStream,
  className,
  ...props
}: QAResponseDivProps) {
  const chatRef = React.useRef<null | HTMLDivElement>(null)
  const [isGeneratingChat, setIsGeneratingChat] = React.useState<boolean>(false)
  const [chatAnswer, setChatAnswer] = React.useState<string | undefined>(
    undefined
  )

  React.useEffect(() => {
    if (!chatStream) return

    const pump = ({
      done,
      value,
    }: {
      done: boolean
      value?: string
    }): Promise<void> => {
      if (done) {
        return Promise.resolve()
      }

      // TODO: Use TransformerStream to do the parsing in API call code
      if (value) {
        value.split('\n').forEach(line => {
          const trimmedLine = line.trim()
          if (trimmedLine.startsWith('data: ')) {
            const msg = trimmedLine.substring('data: '.length)
            try {
              const dataChunk = JSON.parse(msg)
              const content = dataChunk['content']
              if (typeof content === 'string') {
                setChatAnswer(oldValue => (oldValue || '') + content)
              }
            } catch (err) {
              console.log('Unexpected data received from server: '.concat(msg))
            }
          }
        })
      }
      return chatStream.read().then(pump)
    }

    const readStream = async () => {
      setIsGeneratingChat(true)
      try {
        await chatStream.read().then(pump)
      } catch (err) {
      } finally {
        setIsGeneratingChat(false)
      }
    }

    readStream()
  }, [chatStream])

  React.useEffect(() => {
    chatRef.current?.scrollIntoView(false)
  }, [chatAnswer])

  const handleAbort = () => {
    chatAbortController?.abort()
    setIsGeneratingChat(false)
  }

  return (
    <div className={cn('flex flex-col space-y-6', className)} {...props}>
      {qaResponse && (
        <>
          <QAResponseMetadataDiv qaResponseMetadata={qaResponse.metadata} />
          <QAResponseVisualisationDiv visData={qaResponse.visualisation} />
          <QAResponseDataDiv qaResponseData={qaResponse.data} />
        </>
      )}
      {chatAnswer && (
        <div ref={chatRef}>
          <h2 className='text-xl font-semibold text-blue-500 mb-2'>
            Marie&apos;s response
          </h2>
          <Markdown className='prose max-w-none prose-sm prose-slate'>
            {chatAnswer}
          </Markdown>
          {isGeneratingChat && (
            <div
              onClick={handleAbort}
              className='mb-4 flex items-center justify-center space-x-2 hover:cursor-pointer'
            >
              <StopIcon className='h-4 w-4' />
              <p>Stop generating</p>
            </div>
          )}
        </div>
      )}
    </div>
  )
}
