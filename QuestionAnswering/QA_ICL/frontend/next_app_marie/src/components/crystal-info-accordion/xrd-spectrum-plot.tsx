'use client'

import * as React from 'react'
import Chart from 'chart.js/auto'

import { XRDPeak } from '@/lib/model/ontozeolite'

export const XRDSpectrumPlot = ({ data }: { data: XRDPeak[] }) => {
  const ref = React.useRef<null | HTMLCanvasElement>(null)

  React.useEffect(() => {
    if (ref.current === null) return

    const chart = new Chart(ref.current, {
      type: 'bar',
      data: {
        datasets: [
          {
            barThickness: 1,
            data: data.map(datum => ({
              x: datum.TwoThetaPosition,
              y: datum.RelativeIntensity,
            })),
          },
        ],
      },
      options: {
        scales: {
          x: {
            type: 'linear',
            offset: false,
            title: { display: true, text: '2θ' },
          },
          y: {
            title: { display: true, text: 'Relative intensity' },
          },
        },
        plugins: {
          legend: { display: false },
        },
      },
    })

    return () => chart.destroy()
  }, [data])

  return (
    <div className='flex items-center'>
      <canvas ref={ref} />
    </div>
  )
}
