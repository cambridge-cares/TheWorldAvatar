import { z } from 'zod'

import { OUnitCellAngleKey, OUnitCellLengthKey } from '@/lib/model/ontozeolite'

export const ZEOMATERIAL_FORM_SCHEMA = z.object({
  framework: z.string(),
  name: z.string(),
  formula: z.string(),
  elements: z.array(z.string()),
  guests: z.array(z.string()),
  unitCell: z.object({
    lengths: z.object(
      Object.fromEntries(
        Object.values(OUnitCellLengthKey).map(key => [
          key,
          z.object({ lower: z.string(), upper: z.string() }),
        ])
      )
    ),
    angles: z.object(
      Object.fromEntries(
        Object.values(OUnitCellAngleKey).map(key => [
          key,
          z.object({ lower: z.string(), upper: z.string() }),
        ])
      )
    ),
  }),
  citation: z.object({
    authorLastName: z.string(),
    year: z.string(),
    journal: z.string(),
    doi: z.string(),
  }),
})

export const ZEOMATERIAL_FORM_INIT_VALUES = {
  framework: '',
  name: '',
  formula: '',
  elements: [],
  guests: [],
  unitCell: {
    lengths: Object.fromEntries(
      Object.values(OUnitCellLengthKey).map(key => [
        key,
        { lower: '', upper: '' },
      ])
    ),
    angles: Object.fromEntries(
      Object.values(OUnitCellAngleKey).map(key => [
        key,
        { lower: '', upper: '' },
      ])
    ),
  },
  citation: {
    authorLastName: '',
    year: '',
    journal: '',
    doi: '',
  },
}
