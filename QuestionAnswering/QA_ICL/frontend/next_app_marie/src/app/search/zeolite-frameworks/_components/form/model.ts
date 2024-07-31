import { z } from 'zod'

import {
  OUnitCellAngleKey,
  OUnitCellLengthKey,
  SCALAR_TOPO_PROP_KEYS,
} from '@/lib/model/ontozeolite'

export const ZEOFRAMEWORK_FORM_SCHEMA = z.object({
  xrdPeaks: z.array(
    z.object({
      position: z.string(),
      width: z.string(),
      threshold: z.string(),
    })
  ),
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
  scalarTopoProps: z.object(
    Object.fromEntries(
      SCALAR_TOPO_PROP_KEYS.map(key => [
        key,
        z.object({ lower: z.string(), upper: z.string() }),
      ])
    )
  ),
  compositeBUs: z.array(z.string()),
  secondaryBU: z.string(),
})

export const ZEOFRAMEWORK_FORM_INIT_VALUES = {
  xrdPeaks: [{ position: '', width: '', threshold: '' }],
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
  scalarTopoProps: Object.fromEntries(
    SCALAR_TOPO_PROP_KEYS.map(key => [key, { lower: '', upper: '' }])
  ),
  compositeBUs: [''],
  secondaryBU: '',
}
