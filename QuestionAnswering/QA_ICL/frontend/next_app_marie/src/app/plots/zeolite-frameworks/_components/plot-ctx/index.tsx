'use client'

import * as React from 'react'

import { RETURN_FIELD_KEY } from '@/lib/model/ontospecies'
import { Combobox } from '@/components/ui/combobox'
import { ZeoliteFrameworkPartial } from '@/lib/model/ontozeolite'
import { ScatterPlot, ScatterPlotDataPoint } from '@/components/ui/plot'
import { ZeoFrameworkPlotAttrKey } from './model'
import {
  ZEOFRAMEWORK_PLOT_ATTR_LABELS,
  ZEOFRAMEWORK_PLOT_ATTR_QUERY_KEYS,
} from './constants'
import { getZeoliteFrameworksPartialMany } from '@/lib/api/ontozeolite'
import { getZeoliteFrameworkAttrValue } from './helpers'

export const ZeolitePropertiesPlotCtx = () => {
  const [xProp, setXProp] = React.useState<ZeoFrameworkPlotAttrKey>(
    ZeoFrameworkPlotAttrKey.INCLUDED_SPHERE_DIAMETER
  )
  const [yProp, setYProp] = React.useState<ZeoFrameworkPlotAttrKey>(
    ZeoFrameworkPlotAttrKey.TD10
  )

  const [data, setData] = React.useState<ZeoliteFrameworkPartial[] | undefined>(
    undefined
  )
  const [plotData, setPlotData] = React.useState<
    ScatterPlotDataPoint[] | undefined
  >(undefined)
  const [isProcessing, setIsProcessing] = React.useState<boolean>(false)

  React.useEffect(() => {
    async function getData() {
      setIsProcessing(true)

      try {
        const params = new URLSearchParams([
          [RETURN_FIELD_KEY, ZEOFRAMEWORK_PLOT_ATTR_QUERY_KEYS[xProp]],
          [RETURN_FIELD_KEY, ZEOFRAMEWORK_PLOT_ATTR_QUERY_KEYS[yProp]],
        ])
        const res = await getZeoliteFrameworksPartialMany(params)
        setData(res)
      } catch {
      } finally {
        setIsProcessing(false)
      }
    }
    getData()
  }, [xProp, yProp])

  React.useEffect(() => {
    if (data === undefined) return

    setPlotData(
      data
        .map(datum => ({
          x: getZeoliteFrameworkAttrValue(datum, xProp),
          y: getZeoliteFrameworkAttrValue(datum, yProp),
          label: datum.code,
        }))
        .filter(
          (
            datum
          ): datum is {
            x: number
            y: number
            label: string
          } => datum.x !== undefined && datum.y !== undefined
        )
    )
  }, [data, xProp, yProp])

  return (
    <div className='flex flex-col space-y-12'>
      <div className='grid gap-x-4 gap-y-2 md:grid-cols-2'>
        <div>
          <div>X-axis</div>
          <Combobox
            itemCls='zeolite property'
            items={Object.entries(ZEOFRAMEWORK_PLOT_ATTR_LABELS).map(
              ([value, label]) => ({
                value,
                label,
              })
            )}
            value={xProp.toString()}
            onCmdItemSelect={value => setXProp(Number(value))}
          />
        </div>
        <div>
          <div>Y-axis</div>
          <Combobox
            itemCls='species property'
            items={Object.entries(ZEOFRAMEWORK_PLOT_ATTR_LABELS).map(
              ([value, label]) => ({
                value,
                label,
              })
            )}
            value={yProp.toString()}
            onCmdItemSelect={value => setYProp(Number(value))}
            className='col-span-3'
          />
        </div>
      </div>
      {isProcessing && <div>Retrieving data...</div>}
      {plotData && plotData.length ? (
        <ScatterPlot
          data={plotData}
          xAxisLabel={ZEOFRAMEWORK_PLOT_ATTR_LABELS[xProp]}
          yAxisLabel={ZEOFRAMEWORK_PLOT_ATTR_LABELS[yProp]}
        />
      ) : (
        <></>
      )}
    </div>
  )
}
