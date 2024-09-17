'use client'

import * as React from 'react'

import { cn } from '@/lib/utils'

export interface MolViewerProps extends React.HTMLAttributes<HTMLDivElement> {
  type: 'xyz' | 'cif'
  data: string
  backgroundColor?: string
}

export function MolViewer({
  type,
  data,
  className,
  backgroundColor,
  ...props
}: MolViewerProps) {
  const backgroundColorComputed = React.useMemo(
    () => backgroundColor || '#f1f5f9',
    [backgroundColor]
  )
  const ref = React.useRef<null | HTMLDivElement>(null)

  React.useEffect(() => {
    async function mountViewer() {
      if (ref.current !== null) {
        // @ts-ignore
        const $3Dmol = await import('3dmol/build/3Dmol.js')
        const viewer = $3Dmol.createViewer(ref.current, {
          backgroundColor: backgroundColorComputed,
        })
        viewer.addModel(data, type)
        viewer.setStyle({}, { stick: { radius: 0.1 }, sphere: { scale: 0.2 } })
        viewer.zoomTo()
        viewer.render()
        viewer.zoom(1.2, 1000)
      }
    }
    mountViewer()
  }, [backgroundColorComputed, type, data])

  return (
    <div
      ref={ref}
      {...props}
      className={cn('w-full h-96 relative', className)}
    />
  )
}
