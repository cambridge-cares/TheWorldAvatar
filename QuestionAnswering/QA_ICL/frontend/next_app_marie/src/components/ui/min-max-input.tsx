import { cn } from '@/lib/utils'
import { MinusIcon } from '@radix-ui/react-icons'
import React from 'react'
import { Input } from './input'

export interface MinMaxInput extends React.HTMLAttributes<HTMLDivElement> {
  minValue: string
  onMinChange?: React.ChangeEventHandler<HTMLInputElement>
  maxValue: string
  onMaxChange?: React.ChangeEventHandler<HTMLInputElement>
}

export const MinMaxInput = ({
  minValue,
  onMinChange,
  maxValue,
  onMaxChange,
  className,
  ...props
}: MinMaxInput) => (
  <div className={cn('flex items-center space-x-2', className)}>
    <Input
      type='number'
      value={minValue}
      onChange={onMinChange}
      placeholder='min'
    />
    <MinusIcon className='h-4 w-4' />
    <Input
      type='number'
      value={maxValue}
      onChange={onMaxChange}
      placeholder='max'
    />
  </div>
)
