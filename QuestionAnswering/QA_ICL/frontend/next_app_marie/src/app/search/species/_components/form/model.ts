import { z } from 'zod'

import {
  OSpeciesIdentifierKey,
  OSpeciesPropertyKey,
} from '@/lib/model/ontospecies'

export const SPECIES_FORM_SCHEMA = z.object({
  chemicalClass: z.string(),
  use: z.string(),
  property: z.object(
    Object.fromEntries(
      Object.values(OSpeciesPropertyKey).map(key => [
        key,
        z.object({ lower: z.string(), upper: z.string() }),
      ])
    )
  ),
  identifier: z.object(
    Object.fromEntries(
      Object.values(OSpeciesIdentifierKey).map(key => [key, z.string()])
    )
  ),
})

export const SPECIES_FORM_INIT_VALUES = {
  chemicalClass: '',
  use: '',
  property: Object.fromEntries(
    Object.values(OSpeciesPropertyKey).map(key => [
      key,
      { lower: '', upper: '' },
    ])
  ),
  identifier: Object.fromEntries(
    Object.values(OSpeciesIdentifierKey).map(key => [key, ''])
  ),
}
