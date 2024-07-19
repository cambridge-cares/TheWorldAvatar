'use client'

import * as React from 'react'

import {
  CHEMICAL_CLASS_KEY,
  ChemicalClass,
  OntospeciesProperty,
  OSpeciesPropertyKey,
  RETURN_FIELD_KEY,
  Species,
  SpeciesPropertyKey,
} from '@/lib/model/ontospecies'
import { getSpeciesMany } from '@/lib/api/ontospecies'
import { Combobox } from '@/components/ui/combobox'
import { SpeciesPropertiesPlot } from './plot'


function selectSpeciesPropertyNode(nodes: OntospeciesProperty[]) {
  return nodes.find(node => node.provenance === 'PubChem agent') || nodes[0]
}

export interface SpeciesPropertiesPlotCtxProps {
  chemicalClassOptions: ChemicalClass[]
}

export const SpeciesPropertiesPlotCtx = ({
  chemicalClassOptions,
}: SpeciesPropertiesPlotCtxProps) => {
  const [xProp, setXProp] =
    React.useState<SpeciesPropertyKey>('MolecularWeight')
  const [yProp, setYProp] = React.useState<SpeciesPropertyKey>('MeltingPoint')
  const [chemClass, setChemClass] = React.useState<string>(
    (
      chemicalClassOptions.find(node => node.label === 'alkane') ||
      chemicalClassOptions[0]
    ).IRI
  )
  const [plotData, setPlotData] = React.useState<{ x: number, y: number }[] | undefined>(undefined)

  React.useEffect(() => {
    async function getData() {
      const params = new URLSearchParams([
        [CHEMICAL_CLASS_KEY, chemClass],
        [RETURN_FIELD_KEY, xProp],
        [RETURN_FIELD_KEY, yProp],
      ])
      const res = await getSpeciesMany(params)
      const data = res
        .filter(datum => datum.properties[xProp] !== undefined && datum.properties[yProp] !== undefined)
        .map(datum => ({
          x: xProp in datum.properties ? Number(selectSpeciesPropertyNode(datum.properties[xProp]).value) : undefined,
          y: yProp in datum.properties ? Number(selectSpeciesPropertyNode(datum.properties[yProp]).value) : undefined,
        }))
        .filter(
          (datum): datum is { x: number; y: number } =>
            typeof datum.x === 'number' && typeof datum.y === 'number'
        )
      setPlotData(data)
    }
    getData()
  }, [xProp, yProp, chemClass])

  return (
    <div className='flex flex-col space-y-12'>
      <div className='grid gap-x-4 gap-y-2 md:grid-cols-2'>
        <div className='md:col-span-2'>
          <div>Chemical class</div>
          <Combobox
            itemCls='chemical class'
            items={chemicalClassOptions.map(({ IRI, label }) => ({
              value: IRI,
              label,
            }))}
            value={chemClass}
            onCmdItemSelect={value => setChemClass(value)}
            closePopoverOnCmdItemSelect
          />
        </div>
        <div>
          <div>X-axis</div>
          <Combobox
            itemCls='species property'
            items={Object.values(OSpeciesPropertyKey).map(val => ({
              value: val,
              label: val,
            }))}
            value={xProp}
            onCmdItemSelect={value => setXProp(value as SpeciesPropertyKey)}
            closePopoverOnCmdItemSelect
          />
        </div>
        <div>
          <div>Y-axis</div>
          <Combobox
            itemCls='species property'
            items={Object.values(OSpeciesPropertyKey).map(val => ({
              value: val,
              label: val,
            }))}
            value={yProp}
            onCmdItemSelect={value => setYProp(value as SpeciesPropertyKey)}
            closePopoverOnCmdItemSelect
          />
        </div>
      </div>
      {plotData && plotData.length > 0 && (
        <SpeciesPropertiesPlot
          data={plotData}
          xAxisLabel={xProp}
          yAxisLabel={yProp}
          className='w-full'
        />
      )}
    </div>
  )
}
