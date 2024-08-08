import * as React from 'react'
import Markdown from 'react-markdown'
import rehypeSlug from 'rehype-slug'
import rehypeAutolinkHeadings from 'rehype-autolink-headings'

import { cn } from '@/lib/utils'

export const Prose = ({
  className,
  ...props
}: React.ComponentProps<typeof Markdown>) => (
  <Markdown
    rehypePlugins={[rehypeSlug, [rehypeAutolinkHeadings, { behavior: 'wrap' }]]}
    className={cn(
      'prose max-w-none prose-lg prose-h2:text-2xl prose-h3:text-xl prose-ol:list-decimal prose-a:no-underline hover:prose-a:underline',
      className
    )}
    {...props}
  />
)
