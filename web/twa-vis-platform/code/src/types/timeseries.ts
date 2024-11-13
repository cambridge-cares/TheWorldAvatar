import moment from 'moment';

export type TimeSeriesGroup = {
  id: number;
  timeClass: string;
  momentTimes: moment.Moment[];
  times: number[];
  data: TimeSeries[];
};

export type TimeSeries = {
  name: string;
  unit: string;
  values: number[];
  valuesClass: string;
};

export type ScenarioDimensionStep = {
  value: number;
  label: string;
};

export type ScenarioDimensionsData = {
  [key: string]: ScenarioDimensionStep[];
};
