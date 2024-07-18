import * as React from 'react'

import { Main } from '@/components/ui/main'
import { MolViewer } from '@/components/ui/mol-viewer'
import {
  getZeoliteFrameworkCIF,
  getZeoliteFrameworkOne,
} from '@/lib/api/ontozeolite'
import { CrystalInfoAccordion } from './_components/crystal-info-accordion'

interface ZeoliteFrameworkPageInterface {
  params: { iriEncoded: string }
}

export default async function ZeoliteFrameworkPage({
  params,
}: ZeoliteFrameworkPageInterface) {
  const [data, cif] = await Promise.all([
    getZeoliteFrameworkOne(params.iriEncoded),
    getZeoliteFrameworkCIF(params.iriEncoded),
  ])

  return (
    <Main className='flex flex-col items-center'>
      <div className='w-full mt-8 px-4 flex flex-col space-y-2 md:max-w-screen-md lg:max-w-screen-lg'>
        <h1>{data.code}</h1>
        <div className='flex flex-col space-y-4'>
          <section>
            <MolViewer type='cif' data={cif} />
          </section>
          <section>
            <h2 id='crystal-info'>
              <a href='#crystal-info' className='hover:underline'>
                Crystal information
              </a>
            </h2>
            <CrystalInfoAccordion {...data.crystal_information} />
          </section>
          {/* <section>
          <h2 id='topo-props'>
            <a href='#topo-props' className='hover:underline'>
              Topological properties
            </a>
          </h2>
          <div>{JSON.stringify(data.topo_props)}</div>
        </section> */}
          <section className='pb-12'>
            <h2 id='materials'>
              <a href='#materials' className='hover:underline'>
                Zeolitic materials
              </a>
            </h2>
            <ul className='list-disc list-inside'>
              {data.material.map((node, i) => (
                <li key={i}>{node.chemical_formula}</li>
              ))}
            </ul>
          </section>
        </div>
      </div>
    </Main>
  )
}
