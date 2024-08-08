import { z } from 'zod'
import { UseFormReturn } from 'react-hook-form'
import { ReadonlyURLSearchParams } from 'next/navigation'

import {
  CHEMICAL_CLASS_KEY,
  OSpeciesIdentifierKey,
  OSpeciesPropertyKey,
  USE_KEY,
} from '@/lib/model/ontospecies'
import { extractLowerUpperParams } from '@/lib/utils'
import { SPECIES_FORM_SCHEMA } from './model'

export function populateSpeciesFormFields(
  form: UseFormReturn<z.infer<typeof SPECIES_FORM_SCHEMA>>,
  searchParams: ReadonlyURLSearchParams
) {
  const chemicalClass = searchParams.get(CHEMICAL_CLASS_KEY)
  if (chemicalClass) {
    form.setValue('chemicalClass', chemicalClass)
  }

  const use = searchParams.get(USE_KEY)
  if (use) {
    form.setValue('use', use)
  }

  const identifier = Object.fromEntries(
    Object.values(OSpeciesIdentifierKey).map(key => [
      key,
      searchParams.get(key) || '',
    ])
  )
  form.setValue('identifier', identifier)

  const property = extractLowerUpperParams(
    searchParams,
    Object.values(OSpeciesPropertyKey)
  )
  form.setValue('property', property)
}

export function convertSpeciesFormToSearchParams(
  values: z.infer<typeof SPECIES_FORM_SCHEMA>
) {
  const propertyParams = Object.entries(values.property).flatMap(
    ([key, { lower, upper }]) =>
      [
        ['gte', lower],
        ['lte', upper],
      ]
        .filter(([_, val]) => val.length > 0)
        .map(([op, val]) => [key, `${op}:${val}`] as [string, string])
  )
  const identifierParams = Object.entries(values.identifier).filter(
    ([_, value]) => value.length > 0
  )

  return new URLSearchParams(
    [
      values.chemicalClass
        ? ([CHEMICAL_CLASS_KEY, encodeURI(values.chemicalClass)] as [
            string,
            string,
          ])
        : undefined,
      values.use
        ? ([USE_KEY, encodeURI(values.use)] as [string, string])
        : undefined,
      ...propertyParams,
      ...identifierParams,
    ].filter((x): x is [string, string] => x !== undefined)
  )
}
