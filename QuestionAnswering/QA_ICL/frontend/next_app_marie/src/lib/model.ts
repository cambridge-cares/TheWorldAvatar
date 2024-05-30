export interface TableDataItemHeader {
  value: string
  label: string
  dtype: "string" | "number" | "object"
}

export interface TableDataItem {
  type: "table"
  headers: TableDataItemHeader[]
  data: { [key: string]: string | number | object }
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

export interface ScatterPlotDataItem {
  type: "scatter_plot"
  title: string
  traces: ScatterPlotTrace[]
}

export interface MapDataItem {
  type: "map"
  title?: string
  wkt_crs84: string
}

export type DataItem = TableDataItem | ScatterPlotDataItem | MapDataItem 
