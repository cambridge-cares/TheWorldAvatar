import path from "path";
import { promises as fs } from 'fs'

import Markdown from "react-markdown";

import { PATH_TO_RESOURCES } from "@/lib/fs";
import { Main } from "@/components/ui/main";


const PATH_TO_HISTORY_INFO = path.join(PATH_TO_RESOURCES, 'history.md')
export default async function HistoryInfo() {
  const mdContent = await fs.readFile(PATH_TO_HISTORY_INFO, 'utf-8')

  return (
    <Main className='flex flex-col items-center'>
      <div className="w-full md:max-w-screen-md lg:max-w-screen-lg pt-8 mb-12">
        <h1 className="pt-8 mb-8">History of Marie</h1>
        <Markdown
        components={{
          h2(props) {
            const {children, ...rest} = props
            const id = children?.toString().toLowerCase()
            return (
              <h2 {...rest} id={id}><a href={`#${id}`} className="hover:underline">{children}</a></h2>
            )
          }
        }}
         className='prose-lg max-w-none prose-ol:list-decimal'>
          {mdContent}
        </Markdown>
      </div>
    </Main>
  )
}