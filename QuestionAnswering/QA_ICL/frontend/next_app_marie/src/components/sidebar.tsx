import Link from "next/link";
import { Button } from "./ui/button";
import React from "react";

interface SidebarItemProps {
  href: string
  label: string
}

const SidebarItem = ({ href, label }: SidebarItemProps) => (
  <li>
    <Link href={href} legacyBehavior passHref>
      <Button variant="link" className="text-xl w-full justify-start">{label}</Button>
    </Link>
  </li>
)

interface SidebarBlockProps extends React.HTMLAttributes<HTMLDivElement> {
  items: SidebarItemProps[]
}

const SidebarBlock = ({ items, ...props }: SidebarBlockProps) => (
  <div {...props}>
    <ul className="flex flex-col space-y-2">
      {items.map(({ href, label }, i) => (
        <SidebarItem key={i} href={href} label={label} />
      ))}
    </ul>
  </div>
)

const UPPER_SIDEBAR_ITEMS = [
  {
    href: "/",
    label: "Natural language search"
  }
]

const LOWER_SIDEBAR_ITEMS = [
  {
    href: "/ontology-info",
    label: "Ontology Information"
  },
  {
    href: "/history-info",
    label: "History Information"
  }
]

export default function Sidebar() {
  return (
    <nav className="h-screen sticky top-0 py-2 pr-4 bg-secondary flex flex-col justify-between">
      <SidebarBlock items={UPPER_SIDEBAR_ITEMS} />
      <SidebarBlock items={LOWER_SIDEBAR_ITEMS} />
    </nav>
  )
}