import { DataTable } from '@/components/ui/data-table'
import { ZeoliteFrameworkForm } from './_components/form'
import { Main } from '@/components/ui/main'
import { getCBUs, getSBUs, getZeoliteFrameworks } from '@/lib/api/ontozeolite'
import {
  OScalarTopoPropKey,
  OUnitCellAngleKey,
  OUnitCellLengthKey,
} from '@/lib/model/ontozeolite'
import { extractLowerUpperParams, getAll, getFirst } from '@/lib/utils'

export default async function ZeoliteFrameworksPage({
  searchParams,
}: {
  searchParams: { [key: string]: string | string[] | undefined }
}) {
  const [allCBUs, allSBUs] = await Promise.all([getCBUs(), getSBUs()])

  const xrdPeaks = getAll(searchParams, 'xrdPeak')
    .map(serialized => JSON.parse(decodeURI(serialized)))
    .map(peak => ({
      position: peak.position || '',
      width: peak.width || '',
      threshold: peak.threshold || '',
    }))
  const unitCellLengths = extractLowerUpperParams(
    searchParams,
    OUnitCellLengthKey,
    'unit-cell-'
  )
  const unitCellAngles = extractLowerUpperParams(
    searchParams,
    OUnitCellAngleKey,
    'unit-cell-'
  )
  const scalarTopoProps = extractLowerUpperParams(
    searchParams,
    OScalarTopoPropKey
  )
  const compositeBUs = getAll(searchParams, 'composite-bu')
  const secondaryBU = getFirst(searchParams, 'secondary-bu') || ''
  const initValues = {
    xrdPeaks:
      xrdPeaks.length > 0
        ? xrdPeaks
        : [{ position: '', width: '', threshold: '' }],
    unitCell: {
      lengths: unitCellLengths,
      angles: unitCellAngles,
    },
    scalarTopoProps,
    compositeBUs,
    secondaryBU,
  }

  const frameworkLst = await getZeoliteFrameworks(searchParams)

  return (
    <Main className='flex flex-col items-center'>
      <div className='w-full mt-8 px-4 md:max-w-screen-md lg:max-w-screen-lg'>
        <h1 className='mb-4'>Zeolite framework search</h1>
        <ZeoliteFrameworkForm
          allCBUs={allCBUs}
          allSBUs={allSBUs}
          initValues={initValues}
          className='mb-12'
        />
        {frameworkLst && (
          <DataTable
            columns={[
              {
                value: 'IRI',
                label: 'IRI',
              },
              {
                value: 'code',
                label: 'code',
              },
            ]}
            data={frameworkLst}
            className='w-full md:max-w-screen-md lg:max-w-screen-lg mb-12'
          />
        )}
      </div>
    </Main>
  )
}
