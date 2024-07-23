import * as React from 'react'

import { CrystalInfo, VectorComponent } from '@/lib/model/ontozeolite'
import {
  Accordion,
  AccordionContent,
  AccordionItem,
  AccordionTrigger,
} from '@/components/ui/accordion'
import { AtomicStructureTable } from './atomic-structure-table'
import { MatrixTable } from './matrix-table'
import { TileTable } from './tile-table'
import { XRDPeakTable } from './xrd-peak-table'
import { XRDSpectrumPlot } from './xrd-spectrum-plot'

const UNIT_CELL_DIM_KEY = {
  LENGTHS: 'Lengths',
  RECIPROCAL_LENGTHS: 'ReciprocalLengths',
  ANGLES: 'Angles',
  RECIPROCAL_ANGLES: 'ReciprocalAngles',
} as const

const UNIT_CELL_DIM_KEY_LABEL = {
  [UNIT_CELL_DIM_KEY.LENGTHS]: 'Lengths',
  [UNIT_CELL_DIM_KEY.RECIPROCAL_LENGTHS]: 'Reciprocal lengths',
  [UNIT_CELL_DIM_KEY.ANGLES]: 'Angles',
  [UNIT_CELL_DIM_KEY.RECIPROCAL_ANGLES]: 'Reciprocal angles',
}

const UNIT_CELL_DIM_CONFIGS = [
  {
    vectorLabels: ['a', 'b', 'c'],
    keys: [UNIT_CELL_DIM_KEY.LENGTHS, UNIT_CELL_DIM_KEY.RECIPROCAL_LENGTHS],
  },
  {
    vectorLabels: ['alpha', 'beta', 'gamma'],
    keys: [UNIT_CELL_DIM_KEY.ANGLES, UNIT_CELL_DIM_KEY.RECIPROCAL_ANGLES],
  },
]

export const CrystalInfoAccordion = ({
  AtomicStructure,
  CoordinateTransformation,
  UnitCell,
  TiledStructure,
  XRDSpectrum,
}: CrystalInfo) => {
  const sortedXRDPeaks = React.useMemo(
    () =>
      XRDSpectrum?.Peak.toSorted(
        (a, b) => a.TwoThetaPosition - b.TwoThetaPosition
      ),
    [XRDSpectrum]
  )

  return (
    <Accordion type='multiple'>
      <AccordionItem value='atomic-structure'>
        <AccordionTrigger>
          <h3>Atomic structure</h3>
        </AccordionTrigger>
        <AccordionContent>
          <AtomicStructureTable atomicStructure={AtomicStructure.AtomSite} />
        </AccordionContent>
      </AccordionItem>
      <AccordionItem value='coord-transform'>
        <AccordionTrigger>
          <h3>Coordinate transformation</h3>
        </AccordionTrigger>
        <AccordionContent className='px-6 grid gap-4 md:grid-cols-4'>
          <div className='md:col-span-3'>
            <h4 className='font-medium'>
              Fractional to Cartesian transformation matrix
            </h4>
            <MatrixTable
              data={
                CoordinateTransformation.TransformationMatrixToCartesian
                  .component
              }
            />
          </div>
          <div>
            <h4 className='font-medium'>
              Fractional to Cartesian transformation vector
            </h4>
            (
            {[1, 2, 3]
              .map(index =>
                CoordinateTransformation.TransformationVectorToCartesian.component.find(
                  x => x.index === index
                )
              )
              .map(vector => (vector ? vector.value : ''))
              .join(', ')}
            )
          </div>
          <div className='md:col-span-3'>
            <h4 className='font-medium'>
              Cartesian to fractional transformation matrix
            </h4>
            <MatrixTable
              data={
                CoordinateTransformation.TransformationMatrixToFractional
                  .component
              }
            />
          </div>
          <div>
            <h4 className='font-medium'>
              Cartesian to fractional transformation vector
            </h4>
            (
            {[1, 2, 3]
              .map(index =>
                CoordinateTransformation.TransformationVectorToFractional.component.find(
                  x => x.index === index
                )
              )
              .map(vector => (vector ? vector.value : ''))
              .join(', ')}
            )
          </div>
        </AccordionContent>
      </AccordionItem>
      <AccordionItem value='unit-cell'>
        <AccordionTrigger>
          <h3>Unit cell</h3>
        </AccordionTrigger>
        <AccordionContent className='mx-6 flex flex-col space-y-4'>
          <div className='grid md:grid-cols-2 gap-2'>
            {(
              [
                ['Lattice system', UnitCell.LatticeSystem],
                ['Space group symbol', UnitCell.SpaceGroupSymbol],
                ['Symmetry number', UnitCell.SymmetryNumber],
              ] as [string, string | undefined][]
            )
              .filter(([_, val]) => val)
              .map(([heading, val], i) => (
                <div key={i}>
                  <h4 className='font-semibold'>{heading}</h4>
                  <div>{val}</div>
                </div>
              ))}
          </div>
          <div className='grid md:grid-cols-2 gap-2'>
            {UNIT_CELL_DIM_CONFIGS.flatMap(({ vectorLabels, keys }) =>
              keys.map(key => ({
                key,
                vectorLabels,
                vectorComponents: UnitCell[key].component,
              }))
            )
              .map(({ key, vectorLabels, vectorComponents }) => ({
                key,
                vector: `(${vectorLabels
                  .map(label =>
                    vectorComponents.find(
                      component => component.label === label
                    )
                  )
                  .map(x => (x ? x.value : ''))
                  .join(', ')})`,
              }))
              .map(({ key, vector }, i) => (
                <div key={i}>
                  <h4 className='font-semibold'>
                    {UNIT_CELL_DIM_KEY_LABEL[key]}
                  </h4>
                  <div>{vector}</div>
                </div>
              ))}
          </div>
        </AccordionContent>
      </AccordionItem>
      {TiledStructure && (
        <AccordionItem value='tiled-structure'>
          <AccordionTrigger>
            <h3>Tiled structure</h3>
          </AccordionTrigger>
          <AccordionContent className='px-6'>
            <div className='mb-4'>
              <h4 className='font-semibold'>Signature</h4>
              <div>{TiledStructure.Signature}</div>
            </div>
            <TileTable tileNums={TiledStructure.TileNumber} />
          </AccordionContent>
        </AccordionItem>
      )}
      {sortedXRDPeaks && (
        <AccordionItem value='xrd-spectrum'>
          <AccordionTrigger>
            <h3>XRD spectrum</h3>
          </AccordionTrigger>
          <AccordionContent>
            <XRDSpectrumPlot data={sortedXRDPeaks} />
            <XRDPeakTable data={sortedXRDPeaks} />
          </AccordionContent>
        </AccordionItem>
      )}
    </Accordion>
  )
}
