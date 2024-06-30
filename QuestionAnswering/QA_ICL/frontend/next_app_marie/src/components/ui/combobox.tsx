'use client'

import * as React from 'react'
import { CheckIcon, ChevronDownIcon } from '@radix-ui/react-icons'

import { cn } from '@/lib/utils'
import { Button } from './button'
import { Popover, PopoverContent, PopoverTrigger } from './popover'
import {
  Command,
  CommandEmpty,
  CommandGroup,
  CommandInput,
  CommandItem,
  CommandList,
} from './command'

export interface ComboboxProps {
  itemCls: string
  value: string | undefined
  setValue: (value: string | undefined) => void
  items: { value: string; label: string }[]
}

export function Combobox({ itemCls, value, setValue, items }: ComboboxProps) {
  const [open, setOpen] = React.useState(false)
  const value2label = React.useMemo(
    () => Object.fromEntries(items.map(({ value, label }) => [value, label])),
    [items]
  )
  const value2labelLower = React.useMemo(
    () =>
      Object.fromEntries(
        Object.entries(value2label).map(([value, label]) => [
          value,
          label.toLowerCase(),
        ])
      ),
    [value2label]
  )

  return (
    <Popover open={open} onOpenChange={setOpen}>
      <PopoverTrigger asChild>
        <Button
          variant='outline'
          role='combobox'
          aria-expanded={open}
          className='w-full justify-between truncate'
        >
          {value ? value2label[value] : `Select ${itemCls}...`}
          <ChevronDownIcon className='ml-2 h-4 w-4 shrink-0 opacity-50' />
        </Button>
      </PopoverTrigger>
      <PopoverContent className='w-full p-0'>
        <Command
          filter={(itemValue, search) => {
            return value2labelLower[itemValue].includes(search.toLowerCase())
              ? 1
              : 0
          }}
        >
          <CommandInput placeholder={`Search ${itemCls}`} className='h-9' />
          <CommandList>
            <CommandEmpty>{`No ${itemCls} found`}</CommandEmpty>
            <CommandGroup>
              {items.map(({ value: itemValue, label }, i) => (
                <CommandItem
                  key={i}
                  value={itemValue}
                  onSelect={itemValue => {
                    if (itemValue === value) {
                      setValue(undefined)
                    } else {
                      setValue(itemValue)
                    }
                    setOpen(false)
                  }}
                >
                  {label}
                  <CheckIcon
                    className={cn(
                      'ml-auto h-4 w-4',
                      value === itemValue ? 'opacity-100' : 'opacity-0'
                    )}
                  />
                </CommandItem>
              ))}
            </CommandGroup>
          </CommandList>
        </Command>
      </PopoverContent>
    </Popover>
  )
}
