import * as React from 'react'
import Link from 'next/link'

import {
  getZeoliticMaterialCIF,
  getZeoliticMaterialOne,
} from '@/lib/api/ontozeolite'
import { Main } from '@/components/layout'
import { MolViewer } from '@/components/ui/mol-viewer'
import { CrystalInfoAccordion } from '@/components/crystal-info-accordion'

interface ZeoMaterialPageInterface {
  searchParams: { iri: string }
}

export default async function ZeoMaterialPage({
  searchParams,
}: ZeoMaterialPageInterface) {
  const [data, cif] = await Promise.all([
    getZeoliticMaterialOne(searchParams.iri),
    getZeoliticMaterialCIF(searchParams.iri),
  ])

  return (
    <Main className='flex flex-col items-center'>
      <div className='w-full mt-8 px-4 flex flex-col space-y-2 md:max-w-screen-md lg:max-w-screen-lg'>
        <h1>{data.ChemicalFormula}</h1>
        <div className='flex flex-col space-y-4'>
          <section>
            <MolViewer type='cif' data={cif} />
          </section>
          <section>
            <h2 id='general'>
              <a href='#general' className='hover:underline'>
                General
              </a>
            </h2>
            <div className='grid md:grid-cols-2 gap-2'>
              <div>
                <h3 className='font-semibold'>Name</h3>
                <p>{data.name}</p>
              </div>
              <div>
                <h3 className='font-semibold'>Framework code</h3>
                <p>
                  <Link
                    href={
                      '/zeolite-frameworks?iri=' +
                      encodeURIComponent(data.framework.IRI)
                    }
                    className='hover:underline'
                  >
                    {data.framework.code}
                  </Link>
                </p>
              </div>
            </div>
          </section>
          {data.CrystalInformation && (
            <section>
              <h2 id='crystal-info'>
                <a href='#crystal-info' className='hover:underline'>
                  Crystal information
                </a>
              </h2>
              <CrystalInfoAccordion {...data.CrystalInformation} />
            </section>
          )}
          {data.Citation && (
            <section>
              <h2 id='citation'>
                <a href='#citation' className='hover:underline'>
                  Citation
                </a>
              </h2>
              <div>
                {data.Citation.title && (
                  <p className='font-semibold'>{data.Citation.title}</p>
                )}
                <p>
                  {data.Citation.AuthorIndex.sort((a, b) => a.value - b.value)
                    .map(
                      ({ author: { firstName, family_name } }) =>
                        `${family_name}, ${firstName}`
                    )
                    .join(', ')}
                </p>
                {data.Citation.doi && <p>DOI: {data.Citation.doi}</p>}
                {data.Citation.Url && (
                  <a
                    href={data.Citation.Url}
                    target='_blank'
                    rel='noopener noreferrer'
                    className='hover:underline'
                  >
                    {data.Citation.Url}
                  </a>
                )}
              </div>
            </section>
          )}
        </div>
      </div>
    </Main>
  )
}
