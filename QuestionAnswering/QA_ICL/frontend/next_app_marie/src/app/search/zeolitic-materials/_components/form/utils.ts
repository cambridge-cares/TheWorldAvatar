import { UseFormReturn } from 'react-hook-form'
import { z } from 'zod'

import { ZEOMATERIAL_FORM_SCHEMA } from './model'
import { ReadonlyURLSearchParams } from 'next/navigation'
import {
  OUnitCellAngleKey,
  OUnitCellLengthKey,
  UNIT_CELL_KEY_PREFIX,
} from '@/lib/model/ontozeolite'
import { extractLowerUpperParams } from '@/lib/utils'

const FRAMEWORK_KEY = 'Framework'
const MATERIAL_NAME_KEY = 'name'
const CHEMICAL_FORMULA_KEY = 'ChemicalFormula'
const FRAMEWORK_COMPONENT_KEY = 'FrameworkComponent'
const GUEST_COMPONENT_KEY = 'GuestComponent'
const AUTHOR_LAST_NAME_KEY = 'Author-family_name'
const CITATION_YEAR_KEY = 'year'
const JOURNAL_KEY = 'Journal'
const DOI_KEY = 'doi'

export function populateZeoMaterialFormFields(
  form: UseFormReturn<z.infer<typeof ZEOMATERIAL_FORM_SCHEMA>>,
  searchParams: ReadonlyURLSearchParams
) {
  const framework = searchParams.get(FRAMEWORK_KEY)
  if (framework) form.setValue('framework', framework)

  const name = searchParams.get(MATERIAL_NAME_KEY)
  if (name) form.setValue('name', name)

  const formula = searchParams.get(CHEMICAL_FORMULA_KEY)
  if (formula) form.setValue('formula', formula)

  const elements = searchParams.getAll(FRAMEWORK_COMPONENT_KEY)
  if (elements.length > 0) form.setValue('elements', elements)

  const guests = searchParams.getAll(GUEST_COMPONENT_KEY)
  if (guests.length > 0) form.setValue('guests', guests)

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

  const authorLastName = searchParams.get(AUTHOR_LAST_NAME_KEY)
  if (authorLastName) form.setValue('citation.authorLastName', authorLastName)

  const year = searchParams.get(CITATION_YEAR_KEY)
  if (year) form.setValue('citation.year', year)

  const journal = searchParams.get(JOURNAL_KEY)
  if (journal) form.setValue('citation.journal', journal)

  const doi = searchParams.get(DOI_KEY)
  if (doi) form.setValue('citation.doi', doi)
}

export function convertZeoMaterialFormToSearchParams(
  values: z.infer<typeof ZEOMATERIAL_FORM_SCHEMA>
) {
  const unitCellParams = Object.values(values.unitCell).flatMap(params =>
    Object.entries(params).flatMap(([key, { lower, upper }]) =>
      [
        ['gte', lower],
        ['lte', upper],
      ]
        .filter(([_, val]) => val.length > 0)
        .map(([op, val]) => [key, `${op}:${val}`] as [string, string])
    )
  )

  return new URLSearchParams(
    [
      values.framework
        ? ([FRAMEWORK_KEY, values.framework] as [string, string])
        : undefined,
      values.name
        ? ([MATERIAL_NAME_KEY, values.name] as [string, string])
        : undefined,
      values.formula
        ? ([CHEMICAL_FORMULA_KEY, values.formula] as [string, string])
        : undefined,
      ...values.elements.map(
        x => [FRAMEWORK_COMPONENT_KEY, x] as [string, string]
      ),
      ...values.guests.map(x => [GUEST_COMPONENT_KEY, x] as [string, string]),
      ...unitCellParams,
      values.citation.authorLastName
        ? ([AUTHOR_LAST_NAME_KEY, values.citation.authorLastName] as [
            string,
            string,
          ])
        : undefined,
      values.citation.year
        ? ([CITATION_YEAR_KEY, values.citation.year] as [string, string])
        : undefined,
      values.citation.journal
        ? ([JOURNAL_KEY, values.citation.journal] as [string, string])
        : undefined,
      values.citation.doi
        ? ([DOI_KEY, values.citation.doi] as [string, string])
        : undefined,
    ].filter((x): x is [string, string] => x !== undefined)
  )
}
