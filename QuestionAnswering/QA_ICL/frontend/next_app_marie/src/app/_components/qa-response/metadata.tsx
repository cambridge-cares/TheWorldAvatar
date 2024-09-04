import {
  DataRequestForm,
  Nlq2DataReqExample,
  QAResponseMetadata,
  RDFProperty,
} from '@/lib/model/qa'
import { makePrefixedIRI } from '@/lib/utils'

import {
  Accordion,
  AccordionContent,
  AccordionItem,
  AccordionTrigger,
} from '@/components/ui/accordion'
import { DataTable } from '@/components/ui/data-table'
import { JSONTree } from '@/components/ui/json-tree'
import { createColumnHelper } from '@tanstack/react-table'
import React from 'react'

const SEM_PARSE_EXAMPLE_TABLE_COL_HELPER =
  createColumnHelper<Nlq2DataReqExample>()
const SEM_PARSE_EXAMPLE_TABLE_COLS = [
  SEM_PARSE_EXAMPLE_TABLE_COL_HELPER.accessor('nlq', {
    header: 'Natural language question',
  }),
  SEM_PARSE_EXAMPLE_TABLE_COL_HELPER.accessor('data_req.var2cls', {
    header: 'Class assignment',
    cell: props => (
      <div className='flex flex-col space-y-2'>
        {Object.entries(props.getValue()).map(([k, v], i) => (
          <div key={i}>
            <p className='font-medium'>{k}</p>
            <p>{v}</p>
          </div>
        ))}
      </div>
    ),
  }),
  SEM_PARSE_EXAMPLE_TABLE_COL_HELPER.accessor('data_req.entity_bindings', {
    header: 'Entity recognition',
    cell: props => (
      <div className='flex flex-col space-y-2'>
        {Object.entries(props.getValue()).map(([k, v], i) => (
          <div key={i}>
            <p className='font-medium'>{k}</p>
            {v
              .map(x =>
                typeof x === 'object'
                  ? Object.entries(x)
                      .map(([k, v]) => `${k}: ${v}`)
                      .join('\n')
                  : x
              )
              .map((x, i) => (
                <p key={i}>{x}</p>
              ))}
          </div>
        ))}
      </div>
    ),
  }),
  SEM_PARSE_EXAMPLE_TABLE_COL_HELPER.accessor('data_req.req_form', {
    header: 'Structured query form',
    cell: props => {
      const val = props.getValue<DataRequestForm | undefined>()
      if (val)
        return (
          <div className='w-[42rem]'>
            <JSONTree data={val} />
          </div>
        )
      return ''
    },
  }),
]
const SemParseExampleTable = ({
  examples,
}: {
  examples: Nlq2DataReqExample[]
}) => (
  <DataTable
    columns={SEM_PARSE_EXAMPLE_TABLE_COLS}
    data={examples}
    numbered
    paginated
    initialPageSize={5}
    bordered
    scrollable
  />
)

const RELATION_TABLE_COL_HELPER = createColumnHelper<RDFProperty>()
const RELATION_TABLE_COLS = [
  RELATION_TABLE_COL_HELPER.accessor('iri', {
    header: 'IRI',
    cell: cell => makePrefixedIRI(cell.getValue()),
  }),
  RELATION_TABLE_COL_HELPER.accessor('label', {
    header: 'Label',
  }),
  RELATION_TABLE_COL_HELPER.accessor('comment', {
    header: 'Comment',
  }),
]
const RelationTable = ({ properties }: { properties: RDFProperty[] }) => (
  <DataTable
    columns={RELATION_TABLE_COLS}
    data={properties}
    numbered
    paginated
    bordered
    scrollable
  />
)

const VAR2CLS_TABLE_COL_HELPER = createColumnHelper<{
  var: string
  cls: string
}>()
const VAR2CLS_TABLE_COLS = [
  VAR2CLS_TABLE_COL_HELPER.accessor('var', { header: 'Variable' }),
  VAR2CLS_TABLE_COL_HELPER.accessor('cls', { header: 'Class' }),
]
const ClassAssignmentTable = ({
  var2cls,
}: {
  var2cls: { [key: string]: string }
}) => {
  const data = React.useMemo(
    () =>
      Object.entries(var2cls).map(([varname, cls]) => ({ var: varname, cls })),
    [var2cls]
  )

  return (
    <DataTable
      columns={VAR2CLS_TABLE_COLS}
      data={data}
      numbered
      paginated
      bordered
      scrollable
    />
  )
}

const EntityRecognitionTable = ({
  entity_bindings,
  linked_variables,
}: {
  entity_bindings: { [key: string]: (string | { [key: string]: string })[] }
  linked_variables: { [key: string]: string[] }
}) => {
  const data = React.useMemo(
    () =>
      Object.entries(entity_bindings).map(([varname, mentions]) => ({
        var: varname,
        mentions,
        linked_iris: linked_variables[varname],
      })),
    [entity_bindings, linked_variables]
  )

  const helper = createColumnHelper<{
    var: string
    mentions: (string | { [key: string]: string })[]
    linked_iris: string[]
  }>()
  const cols = [
    helper.accessor('var', { header: 'Variable' }),
    helper.accessor('mentions', {
      header: 'Mentions',
      cell: cell =>
        cell
          .getValue()
          .map(x =>
            typeof x === 'string'
              ? x
              : Object.entries(x)
                  .map(([k, v]) => `${k}: ${v}`)
                  .join(', ')
          )
          .join('\n'),
    }),
    helper.accessor('linked_iris', {
      header: 'Linked IRIs',
      cell: cell => cell.getValue().join('\n'),
    }),
  ]

  return (
    <DataTable
      columns={cols}
      data={data}
      numbered
      paginated
      bordered
      scrollable
    />
  )
}

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
                <RelationTable
                  properties={qaResponseMetadata.translation_context.properties.map(
                    ([prop, _]) => prop
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
                <SemParseExampleTable
                  examples={qaResponseMetadata.translation_context.examples.map(
                    ([example, _]) => example
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
                <ClassAssignmentTable
                  var2cls={qaResponseMetadata.data_request.var2cls}
                />
              </AccordionContent>
            </AccordionItem>
            <AccordionItem value='entity_bindings'>
              <AccordionTrigger>
                Entity recognition and linking
              </AccordionTrigger>
              <AccordionContent>
                <EntityRecognitionTable
                  entity_bindings={
                    qaResponseMetadata.data_request.entity_bindings
                  }
                  linked_variables={qaResponseMetadata.linked_variables}
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
