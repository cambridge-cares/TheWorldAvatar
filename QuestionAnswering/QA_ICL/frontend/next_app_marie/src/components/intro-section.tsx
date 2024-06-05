import React from "react";
import Image from "next/image";
import Markdown from 'react-markdown';
import { cn } from '@/lib/utils';


export interface IntroSectionProps {
  imgSrc: string
  imgAlt: string
  introTextMd: string
}

export interface IntroSectionProps
  extends React.HTMLAttributes<HTMLElement> {
  imgSrc: string
  imgAlt: string
  introTextMd: string
}

export default async function IntroSection({ imgSrc, imgAlt, introTextMd, className, ...props }: IntroSectionProps) {
  return (
    <section className={cn("w-full md:max-w-screen-md lg:max-w-screen-lg mb-8 grid md:grid-cols-3 lg:grid-cols-4 gap-4 lg:gap-8 place-items-center", className)} {...props}>
      <div className="grid place-items-center">
        <Image
          src={imgSrc}
          alt={imgAlt}
          width={175}
          height={250}
          priority
        />
      </div>
      <div className="w-full content-center space-y-2 md:col-span-2 lg:col-span-3 prose max-w-none prose-sm prose-slate prose-h1:text-3xl prose-h1:font-semibold prose-h1:text-blue-500 prose-h1:m-0">
        <Markdown>{introTextMd}</Markdown>
      </div>
    </section>
  )
}