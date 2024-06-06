export interface DocumentCollection {
  type: "document_collection"
  data: { [key: string]: any }[]
}

export type TableDataValue = undefined | null | string | number | string[] | number[] | TableDataBase

export interface TableDataBase {
  columns: { value: string, label: string }[]
  data: { [key: string]: TableDataValue }[]
}

export interface TableData extends TableDataBase {
  type: "table"
}

export interface TypedSeries {
  data: any[]
  type: string
}

export interface ScatterPlotTrace {
  name?: string
  x: TypedSeries
  y: TypedSeries
}

export interface ScatterPlotData {
  type: "scatter_plot"
  title: string
  traces: ScatterPlotTrace[]
}

export interface WKTGeometryData {
  type: "wkt_geometry"
  srs: "crs84"
  title?: string
  literal: string
}

export type DataItem = DocumentCollection | TableData | ScatterPlotData | WKTGeometryData

export interface QARequest {
  question: string
}

export interface LexicalBindingValue {
  text?: string
  identifier: { [key: string]: string }
}

export interface LexicalEntityBinding {
  cls: string
  values: LexicalBindingValue[]
}

export interface SparqlDataReqForm {
  type: "sparql"
  namespace: string
  query: string
  res_map: { [key: string]: { pkey: boolean, cls?: string } }
}

export interface FuncDataReqForm {
  type: "func"
  name: string
}

export type DataRequestForm = SparqlDataReqForm | FuncDataReqForm

export interface DataRequest {
  entity_bindings: { [key: string]: LexicalEntityBinding }
  const_bindings: { [key: string]: any }
  req_form: DataRequestForm
}

export interface Nlq2DataReqExample {
  nlq: string
  data_req: DataRequest
}

export interface RDFRelation {
  s: string
  p: string
  o: string
}

export interface TranslationContext {
  examples: Nlq2DataReqExample[]
  schema_relations: RDFRelation[]
}

export interface QAResponseMetadata {
  rewritten_question?: string
  translation_context: TranslationContext
  data_request: DataRequest
  linked_variables: { [key: string]: string[] }
}

export interface QAResponse {
  request_id: string
  metadata: QAResponseMetadata,
  data: DataItem[]
}

export interface ChatRequest {
  qa_request_id: string
}
