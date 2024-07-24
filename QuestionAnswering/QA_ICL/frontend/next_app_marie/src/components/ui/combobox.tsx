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

export interface ComboboxProps extends React.HTMLAttributes<HTMLDivElement> {
  itemCls?: string
  items: { value: string; label: string }[]
  value: string | string[]
  onCmdItemSelect: (value: string) => void
  closePopoverOnCmdItemSelect?: boolean
}

export function Combobox({
  itemCls,
  value,
  onCmdItemSelect,
  items,
  closePopoverOnCmdItemSelect,
  className,
  ...props
}: ComboboxProps) {
  const resolvedClosePopoverOnCmdItemSelect =
    typeof closePopoverOnCmdItemSelect === 'undefined'
      ? false
      : closePopoverOnCmdItemSelect

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
    <div className={cn('w-full', className)} {...props}>
      <Popover open={open} onOpenChange={setOpen}>
        <PopoverTrigger asChild>
          <Button
            variant='outline'
            role='combobox'
            aria-expanded={open}
            className='w-full justify-between truncate'
          >
            {typeof value === 'string' ? (
              value2label[value]
            ) : value.length === 0 ? (
              itemCls ? (
                `Select ${itemCls}...`
              ) : (
                'Select...'
              )
            ) : (
              <div className='flex justify-start space-x-2'>
                {value.map((x, i) => (
                  <div key={i}>{value2label[x]}</div>
                ))}
              </div>
            )}
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
                    onSelect={val => {
                      onCmdItemSelect(val)
                      if (resolvedClosePopoverOnCmdItemSelect) setOpen(false)
                    }}
                    className='hover:cursor-pointer'
                  >
                    {label}
                    <CheckIcon
                      className={cn(
                        'ml-auto h-4 w-4',
                        (
                          typeof value === 'string'
                            ? value === itemValue
                            : value.includes(itemValue)
                        )
                          ? 'opacity-100'
                          : 'opacity-0'
                      )}
                    />
                  </CommandItem>
                ))}
              </CommandGroup>
            </CommandList>
          </Command>
        </PopoverContent>
      </Popover>
    </div>
  )
}
