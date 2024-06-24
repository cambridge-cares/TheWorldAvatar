import styles from './info-tree.module.css';

import React, { useState } from 'react';

import Chart from 'ui/graphic/chart/chart';
import Table from 'ui/graphic/table/table';
import DropdownField, { DropdownFieldOption } from 'ui/interaction/dropdown/dropdown';
import { TimeSeriesGroup } from 'types/timeseries';

interface TimeSeriesPanelProps {
  data: TimeSeriesGroup;
}

/**
 * This component is responsible for displaying time series information as a panel.
 * 
 * @param {TimeSeriesGroup} data The queried time series data.
 */
export default function TimeSeriesPanel(props: Readonly<TimeSeriesPanelProps>) {
  const tsData: TimeSeriesGroup = props.data;
  const [selectedTimeSeriesOption, setSelectedTimeSeriesOption] = useState(0);

  const parseTimeSeriesIntoOptions = (timeSeries: TimeSeriesGroup): DropdownFieldOption[] => {
    const options: DropdownFieldOption[] = [];
    timeSeries.data.map((timeSeries, index) => {
      const label: string = timeSeries.unit === "-" ? timeSeries.name : timeSeries.name + " [" + timeSeries.unit + "]";
      options.push({ index: index, label: label });
    });
    return options;
  };

  return (
    <div className={styles["time-series-panel"]}>
      <DropdownField options={parseTimeSeriesIntoOptions(tsData)} selectedIndex={selectedTimeSeriesOption} setSelectedIndex={setSelectedTimeSeriesOption} />
      <Chart data={tsData} selectedIndex={selectedTimeSeriesOption} />
      <Table group={tsData} selectedIndex={selectedTimeSeriesOption} />
    </div>
  );
}