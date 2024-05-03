import { ScenarioDimensionsData } from "./timeseries";

export type ScenarioDefinition = {
  id: string;
  name: string;
  description: string;
  url?: string;
  dataset?: string;
  dimensions: ScenarioDimensionsData
};