/**
 * Interface of default settings for landing page.
 */
export interface DefaultSettings {
  branding: {
    [key: string]: string | number | boolean;
  },
  modules: {
    [key: string]: boolean;
  },
  scenario?: {
    url: string;
    data: string;
  }
}

// Interface of map settings for visualisation page
export interface MapSettings {
  type: string,
  camera: CameraSettings,
  imagery: ImagerySettings,
  legend?: LegendSettings,
  icons: IconSettings,
}

// Icon settings object
export interface IconSettings {
  [key: string]: string
}

// Imagery settings object
export interface ImagerySettings {
  default: string,
  options: ImageryOption[]
}

// Imagery option object
export interface ImageryOption {
  name: string,
  url: string,
  time?: string
}

// Camera settings object
export interface CameraSettings {
  default: string,
  positions: CameraPosition[]
}

// Camera position object
export interface CameraPosition {
  name: string,
  center: [number, number],
  zoom: number,
  bearing: number,
  pitch: number
}

// Legend settings object
export interface LegendSettings {
  [groupName: string]: LegendGroup;
}

export interface LegendGroup {
  [itemName: string]: FillLegend | SymbolLegend;
}

interface SymbolLegend {
  type: "symbol";
  icon: string;
}

interface FillLegend {
  type: "fill";
  fill: string;
}