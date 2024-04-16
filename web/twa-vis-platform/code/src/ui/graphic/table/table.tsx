import styles from './table.module.css';

import React from 'react';

import { TimeSeries, TimeSeriesGroup } from 'types/timeseries';

type TableProps = {
  group: TimeSeriesGroup;
  selectedIndex: number;
};

/**
 * This component renders a table populated with the time series data.
 * 
 * @param {TimeSeriesGroup} group The group to render.
 * @param {number} selectedIndex The currently selected index.
 */
export default function Table(props: TableProps) {
  const currentTimeSeries: TimeSeries = props.group.data[props.selectedIndex];
  return (
    <div className={styles["table"]}>
      <p>Data series</p>
      <div className={styles["content"]}>
        {currentTimeSeries.values.map((value, index) => (
          <div key={index} className={styles["entry"]}>
            <span className={styles["entry-header"]}>{value}</span>
            <span>at date/time:</span>
            <span>{props.group.times[index]}</span>
          </div>
        ))}
      </div>
    </div>
  );
}