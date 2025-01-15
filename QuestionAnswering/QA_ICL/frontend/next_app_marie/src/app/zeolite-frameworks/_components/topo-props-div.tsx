import {
  SCALAR_TOPO_PROP_KEYS,
  TOPO_PROP_UNITS,
  TopologicalProperties,
} from '@/lib/model/ontozeolite'
import { capitalize } from '@/lib/utils'
import React from 'react'

export interface TopoPropsDivProps
  extends React.HTMLAttributes<HTMLDivElement> {
  data: TopologicalProperties
}

export const TopoPropsDiv = ({ data, ...props }: TopoPropsDivProps) => (
  <div {...props}>
    <div className='grid md:grid-cols-2 gap-2'>
      {[
        ...SCALAR_TOPO_PROP_KEYS.map(
          key =>
            [
              capitalize(
                key
                  .split(/(?=[A-Z])/)
                  .join(' ')
                  .toLowerCase()
              ),
              `${data[key].value} ${TOPO_PROP_UNITS[key]}`,
            ] as [string, string]
        ),
        ...([
          ['Topological density (TD)', data.TopologicalDensity.TD],
          ['Topological density (TD10)', data.TopologicalDensity.TD10],
          [
            'Diameter of largest included sphere',
            `${data.SphereDiameter.component.find(x => x.label === 'included')?.value} Å`,
          ],
          [
            'Sphere diameter',
            `(${['a', 'b', 'c'].map(label => data.SphereDiameter.component.find(x => x.label === label)?.value).join(', ')}) Å`,
          ],
          [
            'Ring sizes',
            `(${[1, 2, 3].map(idx => data.RingSizes.component.find(x => x.index === idx)?.value).join(', ')})`,
          ],
          ['RDLS', data.RDLS],
          [
            'Secondary building unit',
            data.SecondaryBU.length > 0
              ? data.SecondaryBU.join(', ')
              : undefined,
          ],
          [
            'Composite building unit',
            data.CompositeBU &&
              [...data.CompositeBU.Cage, ...data.CompositeBU.TCage].join(', '),
          ],
          ['ABC sequence', data.ABCSequence],
        ] as [string, string | number | undefined][]),
      ]
        .filter((arr): arr is [string, number | string] => arr[1] !== undefined)
        .map(([header, value], i) => (
          <div key={i}>
            <h3 className='font-semibold'>{header}</h3>
            <div className='whitespace-pre-wrap'>{value}</div>
          </div>
        ))}
      {/* TODO: Display T atoms */}
    </div>
  </div>
)
