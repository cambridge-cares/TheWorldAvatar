'use client'

import * as React from 'react'

import { ChemStructData } from '@/lib/model/qa'
import { isObjectEmtpy } from '@/lib/utils'

import { MolViewer } from '@/components/ui/mol-viewer'
import {
  Select,
  SelectContent,
  SelectItem,
  SelectTrigger,
  SelectValue,
} from '@/components/ui/select'
import { ToggleGroup, ToggleGroupItem } from '@/components/ui/toggle-group'

export interface QAResponseVisualisationDivProps
  extends React.HTMLAttributes<HTMLDivElement> {
  visData: { [key: string]: ChemStructData[] }
}

export function QAResponseVisualisationDiv({
  visData,
  ...props
}: QAResponseVisualisationDivProps) {
  const iri2struct = React.useMemo(
    () =>
      Object.fromEntries(
        Object.values(visData).flatMap(structs =>
          structs.map(struct => [struct.iri, struct])
        )
      ),
    [visData]
  )

  const [curVar, setCurVar] = React.useState<string | undefined>(undefined)
  const [curIri, setCurIri] = React.useState<string | undefined>(undefined)

  React.useEffect(() => {
    for (const key in visData) {
      setCurVar(key)
      break
    }
  }, [visData])
  React.useEffect(() => {
    if (
      curVar === undefined ||
      !(curVar in visData) ||
      visData[curVar].length === 0
    )
      return
    setCurIri(visData[curVar][0].iri)
  }, [visData, curVar])

  if (isObjectEmtpy(visData)) return <></>
  return (
    <div {...props}>
      <h2 className='text-xl font-semibold text-blue-500 mb-2'>
        Chemical Structure Visualisation
      </h2>
      <div className='grid lg:grid-cols-3'>
        <div className='flex flex-col space-y-2'>
          {curVar && (
            <ToggleGroup
              type='single'
              defaultValue={curVar}
              onValueChange={val => {
                setCurIri(undefined)
                setCurVar(val)
              }}
              className='flex justify-start'
            >
              {Object.keys(visData).map((varname, i) => (
                <ToggleGroupItem key={i} value={varname}>
                  {varname}
                </ToggleGroupItem>
              ))}
            </ToggleGroup>
          )}
          {curVar && visData[curVar] && curIri && (
            <Select defaultValue={curIri} onValueChange={val => setCurIri(val)}>
              <SelectTrigger className='w-[280px]'>
                <SelectValue placeholder='Select structure to visualise' />
              </SelectTrigger>
              <SelectContent>
                {visData[curVar].map((chemStruct, i) => (
                  <SelectItem key={i} value={chemStruct.iri}>
                    {chemStruct.label}
                  </SelectItem>
                ))}
              </SelectContent>
            </Select>
          )}
        </div>
        <div className='lg:col-span-2'>
          {curIri && iri2struct[curIri] && (
            <MolViewer
              type={iri2struct[curIri].type}
              data={iri2struct[curIri].data}
            />
          )}
        </div>
      </div>
    </div>
  )
}
