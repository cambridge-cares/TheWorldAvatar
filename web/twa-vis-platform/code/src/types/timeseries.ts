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