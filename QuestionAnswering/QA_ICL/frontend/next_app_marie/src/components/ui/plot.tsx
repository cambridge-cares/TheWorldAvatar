'use client'

import * as React from 'react'
import Chart from 'chart.js/auto'
import { cn } from '@/lib/utils'

export interface ScatterPlotDataPoint {
  x: number
  y: number
  label: string
}

export interface ScatterPlotProps extends React.HTMLAttributes<HTMLDivElement> {
  data: ScatterPlotDataPoint[]
  xAxisLabel: string
  yAxisLabel: string
}

export const ScatterPlot = ({
  data,
  xAxisLabel,
  yAxisLabel,
  className,
  ...props
}: ScatterPlotProps) => {
  const ref = React.useRef<null | HTMLCanvasElement>(null)

  React.useEffect(() => {
    if (ref.current === null) return

    const chart = new Chart(ref.current, {
      type: 'scatter',
      data: {
        datasets: [
          {
            data,
          },
        ],
      },
      options: {
        scales: {
          x: {
            title: { display: true, text: xAxisLabel },
          },
          y: {
            title: { display: true, text: yAxisLabel },
          },
        },
        plugins: {
          legend: { display: false },
          tooltip: {
            displayColors: false,
            callbacks: {
              label: ({ parsed: { x, y }, raw }) => [
                `${(raw as ScatterPlotDataPoint).label}`,
                `(${x}, ${y})`,
              ],
            },
          },
        },
      },
    })

    return () => chart.destroy()
  }, [data, xAxisLabel, yAxisLabel])

  return (
    <div className={cn('flex items-center', className)} {...props}>
      <canvas ref={ref} />
    </div>
  )
}
