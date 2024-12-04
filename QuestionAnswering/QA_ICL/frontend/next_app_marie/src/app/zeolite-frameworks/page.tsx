import * as React from 'react'
import Link from 'next/link'
import { CaretSortIcon } from '@radix-ui/react-icons'

import {
  getZeoliteFrameworkCIF,
  getZeoliteFrameworkOne,
} from '@/lib/api/ontozeolite'
import { Main } from '@/components/layout'
import { MolViewer } from '@/components/ui/mol-viewer'
import { CrystalInfoAccordion } from '@/components/crystal-info-accordion'
import {
  Collapsible,
  CollapsibleContent,
  CollapsibleTrigger,
} from '@/components/ui/collapsible'
import { Button } from '@/components/ui/button'
import { TopoPropsDiv } from './_components/topo-props-div'

interface ZeoliteFrameworkPageInterface {
  searchParams: { iri: string }
}

export default async function ZeoliteFrameworkPage({
  searchParams,
}: ZeoliteFrameworkPageInterface) {
  const [data, cif] = await Promise.all([
    getZeoliteFrameworkOne(searchParams.iri),
    getZeoliteFrameworkCIF(searchParams.iri),
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
            <CrystalInfoAccordion {...data.CrystalInformation} />
          </section>
          <section>
            <Collapsible>
              <h2 id='topo-props' className='inline'>
                <a href='#topo-props' className='hover:underline'>
                  Topological properties
                </a>
              </h2>
              <CollapsibleTrigger asChild>
                <Button variant='ghost' size='sm'>
                  <CaretSortIcon className='h-4 w-4' />
                  <span className='sr-only'>Toggle</span>
                </Button>
              </CollapsibleTrigger>
              <CollapsibleContent>
                <TopoPropsDiv data={data.TopologicalProperties} />
              </CollapsibleContent>
            </Collapsible>
          </section>
          <section className='pb-12'>
            <h2 id='materials'>
              <a href='#materials' className='hover:underline'>
                Zeolitic materials
              </a>
            </h2>
            <ul className='list-disc list-inside'>
              {data.ZeoliticMaterial.map(({ ChemicalFormula, IRI }, i) => (
                <li key={i}>
                  <Link
                    href={'/zeolitic-materials?iri=' + encodeURIComponent(IRI)}
                    className='hover:underline'
                  >
                    {ChemicalFormula}
                  </Link>
                </li>
              ))}
            </ul>
          </section>
        </div>
      </div>
    </Main>
  )
}
