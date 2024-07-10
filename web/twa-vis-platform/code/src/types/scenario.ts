import { ScenarioDimensionsData } from "./timeseries";

export type ScenarioDefinition = {
  name: string;
  description: string;
  id: string;
  url?: string;
  dataset?: string;
  type: 'Heat Event' | 'Flood Event' | 'Probabilistic Heat Event';
  dimensions: ScenarioDimensionsData
};