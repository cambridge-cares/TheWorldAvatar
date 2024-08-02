import { cn } from '@/lib/utils'
import Link from 'next/link'
import * as React from 'react'

interface SidebarItem {
  href: string
  label: string
}
interface SidebarItemGroup {
  label: string
  items: SidebarItem[]
}

const SIDEBAR_ITEMS: (SidebarItem | SidebarItemGroup)[] = [
  {
    href: '/',
    label: 'Natural language search',
  },
  {
    label: 'Advanced search',
    items: [
      {
        href: '/search/species',
        label: 'Species',
      },
      {
        href: '/search/zeolite-frameworks',
        label: 'Zeolite framework',
      },
      {
        href: '/search/zeolitic-materials',
        label: 'Zeolitic material',
      },
    ],
  },
  {
    label: 'Explore',
    items: [
      {
        href: '/plots/species',
        label: 'SpeciesExplorer',
      },
      {
        href: '/plots/zeolite-frameworks',
        label: 'ZeoliteExplorer',
      },
    ],
  },
  {
    label: 'About',
    items: [
      {
        href: '/ontology-info',
        label: 'Ontology Information',
      },
      {
        href: '/history-info',
        label: 'History Information',
      },
    ],
  },
]

export default function Sidebar({
  className,
  ...props
}: React.HTMLAttributes<HTMLElement>) {
  return (
    <nav className={cn('flex flex-col', className)} {...props}>
      <ol className='flex flex-col space-y-6'>
        {SIDEBAR_ITEMS.map((item, i) =>
          'href' in item ? (
            <li key={i}>
              <Link href={item.href} legacyBehavior passHref>
                <div className='font-medium w-full whitespace-nowrap justify-start hover:underline hover:cursor-pointer'>
                  {item.label}
                </div>
              </Link>
            </li>
          ) : (
            <div key={i}>
              <div className='font-medium'>{item.label}</div>
              <ol>
                {item.items.map(({ href, label }, i) => (
                  <li key={i}>
                    <Link href={href} legacyBehavior passHref>
                      <div className='w-full whitespace-nowrap justify-start hover:underline hover:cursor-pointer'>
                        {label}
                      </div>
                    </Link>
                  </li>
                ))}
              </ol>
            </div>
          )
        )}
      </ol>
    </nav>
  )
}
