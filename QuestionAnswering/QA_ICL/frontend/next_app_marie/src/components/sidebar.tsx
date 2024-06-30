import Link from 'next/link'
import { Button } from './ui/button'
import React from 'react'

interface SidebarItem {
  href: string
  label: string
}
interface SidebarItemGroup {
  label: string
  items: SidebarItem[]
}

interface SidebarBlockProps extends React.HTMLAttributes<HTMLDivElement> {
  items: (SidebarItem | SidebarItemGroup)[]
}

const SidebarBlock = ({ items, ...props }: SidebarBlockProps) => (
  <div {...props}>
    <ol className='flex flex-col space-y-2'>
      {items.map((item, i) =>
        'href' in item ? (
          <li key={i}>
            <Link href={item.href} legacyBehavior passHref>
              <div className='text-lg w-full whitespace-nowrap justify-start hover:underline hover:cursor-pointer'>
                {item.label}
              </div>
            </Link>
          </li>
        ) : (
          <div key={i}>
            <div className='text-lg'>{item.label}</div>
            <ol className='ml-4'>
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
  </div>
)

const UPPER_SIDEBAR_ITEMS = [
  {
    href: '/',
    label: 'Natural language search',
  },
  {
    label: 'Search',
    items: [
      {
        href: '/search/species',
        label: 'Species search',
      },
    ],
  },
]

const LOWER_SIDEBAR_ITEMS = [
  {
    href: '/ontology-info',
    label: 'Ontology Information',
  },
  {
    href: '/history-info',
    label: 'History Information',
  },
]

export default function Sidebar() {
  return (
    <nav className='h-screen sticky top-0 py-4 pl-4 pr-8 bg-secondary flex flex-col justify-between'>
      <SidebarBlock items={UPPER_SIDEBAR_ITEMS} />
      <SidebarBlock items={LOWER_SIDEBAR_ITEMS} />
    </nav>
  )
}
