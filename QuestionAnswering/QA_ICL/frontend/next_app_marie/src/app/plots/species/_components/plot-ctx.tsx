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
import { getSpeciesPartialMany } from '@/lib/api/ontospecies'
import { Combobox } from '@/components/ui/combobox'
import { ScatterPlot, ScatterPlotDataPoint } from '@/components/ui/plot'

function selectSpeciesPropertyNode(nodes: OntospeciesProperty[]) {
  return nodes.find(node => node.Provenance === 'PubChem agent') || nodes[0]
}

function deriveUnitOptions(data: Partial<Species>[], key: SpeciesPropertyKey) {
  return Object.entries(
    data
      .map(datum => (datum.Property ? datum.Property[key] : undefined))
      .filter((x): x is OntospeciesProperty[] => x !== undefined)
      .flatMap(nodes => nodes.map(node => node.unit))
      .filter((x): x is string => typeof x === 'string')
      .reduce(
        (acc, x) => {
          acc[x] = (acc[x] || 0) + 1
          return acc
        },
        {} as { [key: string]: number }
      )
  )
    .toSorted(([_unitA, freqA], [_unitB, freqB]) => freqB - freqA) // sort by descending frequency
    .map(([unit, _]) => unit)
}

export interface SpeciesPropertiesPlotCtxProps {
  chemicalClassOptions: ChemicalClass[]
}

export const SpeciesPropertiesPlotCtx = ({
  chemicalClassOptions,
}: SpeciesPropertiesPlotCtxProps) => {
  const [xProp, setXProp] =
    React.useState<SpeciesPropertyKey>('MolecularWeight')
  const [xUnitOptions, setXUnitOptions] = React.useState<string[] | undefined>(
    undefined
  )
  const [xUnit, setXUnit] = React.useState<string | undefined>(undefined)

  const [yProp, setYProp] = React.useState<SpeciesPropertyKey>('MeltingPoint')
  const [yUnitOptions, setYUnitOptions] = React.useState<string[] | undefined>(
    undefined
  )
  const [yUnit, setYUnit] = React.useState<string | undefined>(undefined)

  const [chemClass, setChemClass] = React.useState<string>(
    (
      chemicalClassOptions.find(node => node.label === 'alkane') ||
      chemicalClassOptions[0]
    ).IRI
  )
  const [data, setData] = React.useState<Partial<Species>[] | undefined>(
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
          [CHEMICAL_CLASS_KEY, chemClass],
          [RETURN_FIELD_KEY, xProp],
          [RETURN_FIELD_KEY, yProp],
        ])
        const res = await getSpeciesPartialMany(params)
        setData(res)
        setXUnitOptions(deriveUnitOptions(res, xProp))
        setYUnitOptions(deriveUnitOptions(res, yProp))
      } catch {
      } finally {
        setIsProcessing(false)
      }
    }
    getData()
  }, [xProp, yProp, chemClass])

  React.useEffect(() => {
    if (xUnitOptions === undefined) return
    setXUnit(xUnitOptions.find(x => x !== ''))
  }, [xUnitOptions])

  React.useEffect(() => {
    if (yUnitOptions === undefined) return
    setYUnit(yUnitOptions.find(x => x !== ''))
  }, [yUnitOptions])

  React.useEffect(() => {
    if (data === undefined) return

    setPlotData(
      data
        .map(({ Property, IUPACName, InChI }) => ({
          x:
            Property && Property[xProp]
              ? Property[xProp].filter(
                  node => xUnit === undefined || node.unit === xUnit
                )
              : undefined,
          y:
            Property && Property[yProp]
              ? Property[yProp].filter(
                  node => yUnit === undefined || node.unit === yUnit
                )
              : undefined,
          label: IUPACName || InChI,
        }))
        .filter(
          (
            datum
          ): datum is {
            x: OntospeciesProperty[]
            y: OntospeciesProperty[]
            label: string
          } =>
            datum.x !== undefined &&
            datum.x.length > 0 &&
            datum.y !== undefined &&
            datum.y.length > 0
        )
        .map(({ x, y, label }) => ({
          x: Number(selectSpeciesPropertyNode(x).value),
          y: Number(selectSpeciesPropertyNode(y).value),
          label,
        }))
    )
  }, [data, xProp, yProp, xUnit, yUnit])

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
          />
        </div>
        <div>
          <div>X-axis</div>
          <div
            className={
              xUnitOptions && xUnit !== undefined
                ? 'grid grid-cols-5 gap-2'
                : ''
            }
          >
            <Combobox
              itemCls='species property'
              items={Object.values(OSpeciesPropertyKey).map(val => ({
                value: val,
                label: val,
              }))}
              value={xProp}
              onCmdItemSelect={value => setXProp(value as SpeciesPropertyKey)}
              className='col-span-3'
            />
            {xUnitOptions && xUnit !== undefined && (
              <Combobox
                itemCls='unit'
                items={xUnitOptions.map(val => ({ value: val, label: val }))}
                value={xUnit}
                onCmdItemSelect={value => setXUnit(value)}
                className='col-span-2'
              />
            )}
          </div>
        </div>
        <div>
          <div>Y-axis</div>
          <div
            className={
              yUnitOptions && yUnit !== undefined
                ? 'grid grid-cols-5 gap-2'
                : ''
            }
          >
            <Combobox
              itemCls='species property'
              items={Object.values(OSpeciesPropertyKey).map(val => ({
                value: val,
                label: val,
              }))}
              value={yProp}
              onCmdItemSelect={value => setYProp(value as SpeciesPropertyKey)}
              className='col-span-3'
            />
            {yUnitOptions && yUnit !== undefined && (
              <Combobox
                itemCls='unit'
                items={yUnitOptions.map(val => ({ value: val, label: val }))}
                value={yUnit}
                onCmdItemSelect={value => setYUnit(value)}
                className='col-span-2'
              />
            )}
          </div>
        </div>
      </div>
      {isProcessing && <div>Retrieving data...</div>}
      {plotData && plotData.length > 0 ? (
        <ScatterPlot
          data={plotData}
          xAxisLabel={`${xProp} (${xUnit})`}
          yAxisLabel={`${yProp} (${yUnit})`}
          className='w-full'
        />
      ) : (
        <></>
      )}
    </div>
  )
}
