import React from 'react'
import Image from 'next/image'

import { cn } from '@/lib/utils'

import CMCLLogo from '@/public/images/cmcl.svg'
import COMOLogo from '@/public/images/como_logo_processed.svg'
import CARESLogo from '@/public/images/cares_short_logo_processed.svg'
import MITLogo from '@/public/images/MIT-logo-transparent-cropped.png'

const ORGS = [
  {
    imgSrc: CMCLLogo,
    label: 'CMCL',
    url: 'https://cmcl.io/',
    displayUrl: 'cmcl.io',
  },
  {
    imgSrc: COMOLogo,
    label: 'Computational Modelling Group',
    url: 'https://como.ceb.cam.ac.uk/',
    displayUrl: 'como.ceb.cam.ac.uk',
  },
  {
    imgSrc: CARESLogo,
    label: 'Cambridge CARES',
    url: 'https://cares.cam.ac.uk/',
    displayUrl: 'cares.cam.ac.uk',
  },
  {
    imgSrc: MITLogo,
    label: 'MIT',
    url: 'https://www.mit.edu/',
    displayUrl: 'mit.edu',
  },
]

export default function Footer({
  className,
  ...props
}: React.HTMLAttributes<HTMLElement>) {
  return (
    <footer
      className={cn(
        'border-t mx-2 py-2 flex flex-col items-center gap-y-2',
        className
      )}
      {...props}
    >
      <p className='text-gray-400 text-center max-w-4xl'>
        This content was developed, tested, and documented by the following
        commercial and academic partners. For more information on each
        institution, please follow the links listed below.
      </p>
      <div className='w-full grid md:grid-cols-4'>
        {ORGS.map(({ imgSrc, label, url, displayUrl }, i) => (
          <div key={i} className='flex justify-center'>
            <div className='flex flex-col justify-center items-center h-full'>
              <Image
                src={imgSrc}
                width={140}
                height={80}
                alt={`${label} logo`}
              />
              <div className='text-gray-400'>{label}</div>
              <a
                href={url}
                target='_blank'
                className='text-blue-400 hover:underline'
              >
                {displayUrl}
              </a>
            </div>
          </div>
        ))}
      </div>
      <p className='text-gray-400'>All Rights Reserved.</p>
    </footer>
  )
}
