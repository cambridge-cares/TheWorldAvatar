import { z } from 'zod'
import { UseFormReturn } from 'react-hook-form'
import { ReadonlyURLSearchParams } from 'next/navigation'

import { ZEOFRAMEWORK_FORM_SCHEMA } from './model'
import {
  OTopoPropKey,
  OUnitCellAngleKey,
  OUnitCellLengthKey,
  SCALAR_TOPO_PROP_KEYS,
  UNIT_CELL_KEY_PREFIX,
  XRD_PEAK_KEY,
} from '@/lib/model/ontozeolite'
import { extractLowerUpperParams, isObjectEmtpy } from '@/lib/utils'

export function populateZeoFrameworkFormFields(
  form: UseFormReturn<z.infer<typeof ZEOFRAMEWORK_FORM_SCHEMA>>,
  searchParams: ReadonlyURLSearchParams
) {
  const xrdPeaks = searchParams
    .getAll(XRD_PEAK_KEY)
    .map(serialized => JSON.parse(decodeURI(serialized)))
    .map(peak => ({
      position: peak.position || '',
      width: peak.width || '',
      threshold: peak.threshold || '',
    }))
  if (xrdPeaks.length > 0) form.setValue('xrdPeaks', xrdPeaks)

  const unitCellLengths = extractLowerUpperParams(
    searchParams,
    Object.values(OUnitCellLengthKey),
    UNIT_CELL_KEY_PREFIX
  )
  const unitCellAngles = extractLowerUpperParams(
    searchParams,
    Object.values(OUnitCellAngleKey),
    UNIT_CELL_KEY_PREFIX
  )
  form.setValue('unitCell', {
    lengths: unitCellLengths,
    angles: unitCellAngles,
  })

  const scalarTopoProps = extractLowerUpperParams(
    searchParams,
    SCALAR_TOPO_PROP_KEYS
  )
  form.setValue('scalarTopoProps', scalarTopoProps)

  const compositeBUs = searchParams.getAll(OTopoPropKey.COMPOSITE_BU)
  if (compositeBUs.length > 0) form.setValue('compositeBUs', compositeBUs)

  const secondaryBU = searchParams.get(OTopoPropKey.SECONDARY_BU)
  if (secondaryBU) form.setValue('secondaryBU', secondaryBU)
}

export function convertZeoFrameworkFormToSearchParams(
  values: z.infer<typeof ZEOFRAMEWORK_FORM_SCHEMA>
) {
  const xrdPeakParams = values.xrdPeaks
    .map(peak => Object.fromEntries(Object.entries(peak).filter(([_, v]) => v)))
    .filter(x => !isObjectEmtpy(x))
    .map(x => encodeURI(JSON.stringify(x)))
    .map(peak => [XRD_PEAK_KEY, peak] as [string, string])
  const unitCellParams = Object.values(values.unitCell).flatMap(params =>
    Object.entries(params).flatMap(([key, { lower, upper }]) =>
      [
        ['gte', lower],
        ['lte', upper],
      ]
        .filter(([_, val]) => val.length > 0)
        .map(
          ([op, val]) =>
            [UNIT_CELL_KEY_PREFIX + key, `${op}:${val}`] as [string, string]
        )
    )
  )
  const scalarTopoPropsParams = Object.entries(values.scalarTopoProps).flatMap(
    ([key, { lower, upper }]) =>
      [
        ['gte', lower],
        ['lte', upper],
      ]
        .filter(([_, val]) => val.length > 0)
        .map(([op, val]) => [key, `${op}:${val}`] as [string, string])
  )
  const CBUsParams = values.compositeBUs
    .filter(x => x)
    .map(x => [OTopoPropKey.COMPOSITE_BU, x] as [string, string])
  const SBUParam = values.secondaryBU
    ? ([OTopoPropKey.SECONDARY_BU, values.secondaryBU] as [string, string])
    : undefined

  return new URLSearchParams(
    [
      ...xrdPeakParams,
      ...unitCellParams,
      ...scalarTopoPropsParams,
      ...CBUsParams,
      SBUParam,
    ].filter((x): x is [string, string] => x !== undefined)
  )
}
