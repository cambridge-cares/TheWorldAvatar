import React from 'react'
import Image from 'next/image'
import Markdown from 'react-markdown'
import { cn } from '@/lib/utils'

export interface IntroSectionProps extends React.HTMLAttributes<HTMLElement> {
  imgSrc: string
  imgAlt: string
  heading: string
  mdContent: string
}

export const IntroSection = async ({
  imgSrc,
  imgAlt,
  heading,
  mdContent,
  className,
  ...props
}: IntroSectionProps) => (
  <section
    className={cn(
      'grid md:grid-cols-3 lg:grid-cols-4 gap-4 lg:gap-8 place-items-center',
      className
    )}
    {...props}
  >
    <div className='grid place-items-center'>
      <Image src={imgSrc} alt={imgAlt} width={175} height={250} priority />
    </div>
    <div className='w-full content-center space-y-2 md:col-span-2 lg:col-span-3'>
      <h1>{heading}</h1>
      <Markdown className='prose max-w-none prose-sm prose-slate'>
        {mdContent}
      </Markdown>
    </div>
  </section>
)
