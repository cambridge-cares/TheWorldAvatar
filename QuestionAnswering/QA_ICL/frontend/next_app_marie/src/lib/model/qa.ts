export interface DocumentCollection {
  type: 'document_collection'
  data: { [key: string]: any }[]
}

export type TableDataValue =
  | undefined
  | null
  | string
  | number
  | string[]
  | number[]
  | TableDataBase

export interface TableDataBase {
  columns: string[]
  data: { [key: string]: TableDataValue }[]
}

export interface TableData extends TableDataBase {
  type: 'table'
}

export interface ChemStructData {
  type: 'xyz' | 'cif'
  iri: string
  label: string
  data: string
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
  type: 'scatter_plot'
  title: string
  traces: ScatterPlotTrace[]
}

export interface WKTGeometryData {
  type: 'wkt_geometry'
  srs: 'crs84'
  title?: string
  literal: string
}

export type DataItem =
  | DocumentCollection
  | TableData
  | ScatterPlotData
  | WKTGeometryData

export interface QARequest {
  question: string
}

export interface SparqlDataReqForm {
  type: 'sparql'
  triplestore: string
  query: string
  pkeys: string[]
}

export interface FuncDataReqForm {
  type: 'func'
  name: string
}

export type DataRequestForm = SparqlDataReqForm | FuncDataReqForm

export interface DataRequest {
  var2cls: { [key: string]: string }
  entity_bindings: { [key: string]: (string | { [key: string]: string })[] }
  const_bindings: { [key: string]: any }
  req_form?: DataRequestForm
  visualise: string[]
}

export interface Nlq2DataReqExample {
  nlq: string
  data_req: DataRequest
}

export interface RDFProperty {
  iri: string
  label?: string
  comment?: string
}

export interface TranslationContext {
  examples: [Nlq2DataReqExample, number][]
  properties: [RDFProperty, number][]
}

export interface QAResponseMetadata {
  rewritten_question?: string
  translation_context: TranslationContext
  data_request: DataRequest
  linked_variables: { [key: string]: string[] }
}

export interface QAResponse {
  request_id: string
  metadata: QAResponseMetadata
  data: DataItem[]
  visualisation: { [key: string]: ChemStructData[] }
}
