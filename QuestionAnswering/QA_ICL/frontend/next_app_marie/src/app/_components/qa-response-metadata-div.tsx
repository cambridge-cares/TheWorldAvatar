"use client"

import * as React from "react";

import { QAResponseMetadata } from "@/lib/model";
import { makePrefixedIRI } from "@/lib/utils";
import { Accordion, AccordionContent, AccordionItem, AccordionTrigger } from "@/components/ui/accordion";
import { DataTable } from "@/components/ui/data-table";
import { JSONTree } from "@/components/ui/json-tree";


export interface QAResponseMetadataDivProps extends React.HTMLAttributes<HTMLDivElement> {
  qaResponseMetadata: QAResponseMetadata
}

export function QAResponseMetadataDiv({ qaResponseMetadata, ...props }: QAResponseMetadataDivProps) {
  return (
    <div {...props}>
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
                    data={qaResponseMetadata.translation_context.schema_relations.map(obj => Object.fromEntries(Object.entries(obj).map(([k, v]) => [k, makePrefixedIRI(v)])))}
                  />
                </AccordionContent>
              </AccordionItem>
              <AccordionItem value="examples">
                <AccordionTrigger>Semantic parsing examples retrieved from database with highest relevance</AccordionTrigger>
                <AccordionContent>
                  <DataTable
                    columns={[
                      { value: "nlq", label: "Natural language question" },
                      { value: "entity_bindings", label: "Entity bindings" },
                      { value: "req_form", label: "Structured query form" }
                    ]}
                    data={qaResponseMetadata.translation_context.examples.map(example => ({
                      nlq: example.nlq,
                      entity_bindings: (<JSONTree data={example.data_req.entity_bindings} />),
                      req_form: (<JSONTree data={example.data_req.req_form} />)
                    }))}
                  />
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
                      .entries(qaResponseMetadata.data_request.entity_bindings)
                      .map(([varname, binding]) => ({
                        var: varname,
                        cls: binding.cls,
                        mention: binding.values.flatMap(val => (val.text ? [val.text] : []).concat(Object.entries(val.identifier).map(([k, v]) => `${k}: ${v}`))),
                        linked_iris: qaResponseMetadata.linked_variables[varname]
                      }))
                    }
                  />
                </AccordionContent>
              </AccordionItem>
              <AccordionItem value="data_req_form">
                <AccordionTrigger>Structured query form</AccordionTrigger>
                <AccordionContent className="px-6">
                  {qaResponseMetadata.data_request.req_form.type === "sparql" ? (
                    <>
                      <h4 className="font-medium">Namespace</h4>
                      <p className="mb-2">{qaResponseMetadata.data_request.req_form.namespace}</p>
                      <h4 className="font-medium">SPARQL query</h4>
                      <p className="font-mono whitespace-pre bg-slate-50 p-4">{qaResponseMetadata.data_request.req_form.query}</p>
                    </>
                  ) : (<></>)}
                </AccordionContent>
              </AccordionItem>
            </Accordion>
          </AccordionContent>
        </AccordionItem>
      </Accordion>
    </div>
  )
}