import * as React from 'react'

import { cn } from '@/lib/utils'

export default function Main({
  className,
  ...props
}: React.HTMLAttributes<HTMLElement>) {
  return <main className={cn('w-full mb-20', className)} {...props} />
}
