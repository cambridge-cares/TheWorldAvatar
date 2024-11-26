import { DefaultPageThumbnailProps } from "ui/pages/page-thumbnail";

/**
 * Interface of default settings for landing page.
 */
export type UISettings = {
  branding: {
    'navbarLogo': string | string[]; // Backwards compatibility but do not use this going forward
    'navbar': string | string[];
    'landing': string;
    'landingDark': string;
  },
  modules: {
    [key: string]: boolean;
  },
  links?: DefaultPageThumbnailProps[],
  resources?: {
    [key: string]: {
      url: string;
      data?: string;
    };
  }
}

// Interface of map settings for visualisation page
export type MapSettings = {
  type: string,
  camera: CameraSettings,
  imagery: ImagerySettings,
  legend?: LegendSettings,
  icons?: IconSettings,
}

// Icon settings object
export type IconSettings = {
  [key: string]: string
}

// Imagery settings object
export type ImagerySettings = {
  default: string,
  options: ImageryOption[]
}

// Imagery option object
export type ImageryOption = {
  name: string,
  url: string,
  time?: string
}

// Camera settings object
export type CameraSettings = {
  default: string,
  positions: CameraPosition[]
}

// Camera position object
export type CameraPosition = {
  name: string,
  center: [number, number],
  zoom: number,
  bearing: number,
  pitch: number
}

// Legend settings object
export type LegendSettings = {
  [groupName: string]: LegendGroup;
}

export type LegendGroup =  FillLegend[] | SymbolLegend[];

type SymbolLegend = {
  heading: string;
  content: string;
  type: "symbol";
  icon: string;
}

type FillLegend = {
  heading: string;
  content: string; 
  type: "fill";
  fill: string;
}

export type MapboxCredentials = { username: string, token: string }