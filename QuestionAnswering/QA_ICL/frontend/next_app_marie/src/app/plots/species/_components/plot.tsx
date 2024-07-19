'use client'

import * as React from 'react'
import Chart from 'chart.js/auto'
import { cn } from '@/lib/utils'

export interface SpeciesPropertiesPlotProps extends React.HTMLAttributes<HTMLDivElement> {
  data: {
    x: number
    y: number
  }[]
  xAxisLabel: string
  yAxisLabel: string
}

export const SpeciesPropertiesPlot = ({
  data,
  xAxisLabel,
  yAxisLabel,
  className,
  ...props
}: SpeciesPropertiesPlotProps) => {
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
