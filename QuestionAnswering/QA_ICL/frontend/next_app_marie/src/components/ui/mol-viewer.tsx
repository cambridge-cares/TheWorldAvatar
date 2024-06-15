'use client'

import { cn } from '@/lib/utils'
import * as React from 'react'
export interface MolViewerProps extends React.HTMLAttributes<HTMLDivElement> {
  type: 'xyz' | 'cif'
  data: string
}

export function MolViewer({ type, data, className, ...props }: MolViewerProps) {
  const ref = React.useRef<null | HTMLDivElement>(null)

  React.useEffect(() => {
    async function mountViewer() {
      if (ref.current !== null) {
        // @ts-ignore
        const $3Dmol = await import('3dmol/build/3Dmol.js')
        console.log($3Dmol)
        const viewer = $3Dmol.createViewer(ref.current, {})
        console.log(viewer)
        viewer.addModel(data, type)
        viewer.setStyle({}, { stick: { radius: 0.1 }, sphere: { scale: 0.2 } })
        viewer.zoomTo()
        viewer.render()
        viewer.zoom(1.2, 1000)
      }
    }
    mountViewer()
  }, [type, data])

  return (
    <div
      ref={ref}
      {...props}
      className={cn('w-full h-96 relative', className)}
    />
  )
}
