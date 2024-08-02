import { cn } from '@/lib/utils'
import React from 'react'
import Image from 'next/image'

const URL_AND_LOGO_LST = [
  {
    url: 'https://cmclinnovations.com/',
    imgSrc: '/images/cmcl.svg',
    imgAlt: 'CMCL logo',
  },
  {
    url: 'https://como.ceb.cam.ac.uk',
    imgSrc: '/images/como_logo_processed.svg',
    imgAlt: 'CoMo logo',
  },
  {
    url: 'https://www.cares.cam.ac.uk',
    imgSrc: '/images/cares_short_logo_processed.svg',
    imgAlt: 'CARES logo',
  },
]
export default function Header({
  className,
  ...props
}: React.HTMLAttributes<HTMLElement>) {
  return (
    <header
      className={cn('bg-white flex justify-center items-center', className)}
      {...props}
    >
      <div className='max-w-7xl w-full  flex items-center justify-between'>
        <div className='pl-2 pr-12 text-4xl font-semibold bg-white text-primary m-0'>
          Marie
        </div>
        <div className='flex'>
          {URL_AND_LOGO_LST.map(({ url, imgSrc, imgAlt }, i) => (
            <a key={i} href={url} target='_blank'>
              <Image src={imgSrc} width={140} height={80} alt={imgAlt}></Image>
            </a>
          ))}
        </div>
      </div>
    </header>
  )
}
