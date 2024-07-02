'use client'

import * as React from 'react'

import { cn } from '@/lib/utils'
import { MagnifyingGlassIcon, ReloadIcon } from '@radix-ui/react-icons'
import { Button } from '@/components/ui/button'
import { Input } from '@/components/ui/input'

export interface NLPSearchFormProps
  extends React.FormHTMLAttributes<HTMLFormElement> {
  inputValue: string
  onInputChange: React.ChangeEventHandler<HTMLInputElement>
  isProcessing: boolean
}

const NLPSearchForm = React.forwardRef<HTMLFormElement, NLPSearchFormProps>(
  ({ inputValue, onInputChange, isProcessing, className, ...props }, ref) => (
    <form
      ref={ref}
      className={cn('flex justify-center items-center', className)}
      {...props}
    >
      <Input
        placeholder='Type your query...'
        required
        className='mr-2'
        value={inputValue}
        onChange={onInputChange}
        disabled={isProcessing}
      />
      <Button
        type='submit'
        variant='outline'
        size='icon'
        disabled={isProcessing}
      >
        {isProcessing ? (
          <ReloadIcon className='animate-spin' />
        ) : (
          <MagnifyingGlassIcon />
        )}
      </Button>
    </form>
  )
)
NLPSearchForm.displayName = 'NLPSearchForm'

export { NLPSearchForm }
