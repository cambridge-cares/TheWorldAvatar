import * as React from 'react'

import { cn } from '@/lib/utils'

export const Main = ({
  className,
  ...props
}: React.HTMLAttributes<HTMLElement>) => (
  <main className={cn('w-full', className)} {...props} />
)
