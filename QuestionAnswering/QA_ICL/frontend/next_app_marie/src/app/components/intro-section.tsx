
import { promises as fs } from 'fs';
import path from "path";

import React from "react";
import Image from "next/image";
import Markdown from 'react-markdown';

export default async function IntroSection() {
  const introText = await fs.readFile(path.join(process.cwd(), "resources/intro-text.md"), "utf8")

  return (
    <div className="w-full max-w-screen-lg mb-8 grid gap-8 place-items-center md:grid-cols-4 lg:w-7/12 lg:grid-cols-3">
      <div className="grid place-items-center">
        <Image
          src="/images/marie-thumbnail.jpg"
          alt="Marie thumbnail"
          width={175}
          height={250}
          priority
        />
      </div>
      <div className="w-full content-center space-y-2 md:col-span-3 lg:col-span-2 prose max-w-none prose-sm prose-slate prose-h1:text-3xl prose-h1:font-semibold prose-h1:text-blue-500 prose-h1:m-0">
        <Markdown>{introText}</Markdown>
      </div>
    </div>
  )
}