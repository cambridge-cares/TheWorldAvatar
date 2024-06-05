"use client"

import { Button } from "@/components/ui/button";
import { Input } from "@/components/ui/input";
import { cn } from "@/lib/utils";
import { MagnifyingGlassIcon } from "@radix-ui/react-icons";
import { ChangeEventHandler } from "react";


export interface NLPSearchFormProps extends React.HTMLAttributes<HTMLFormElement> {
  inputValue: string
  onInputChange: ChangeEventHandler<HTMLInputElement>
  disabled: boolean
}

export function NLPSearchForm({ onSubmit, inputValue, onInputChange, disabled, className, ...props }: NLPSearchFormProps) {
  return (
    <form className={cn("flex justify-center items-center", className)} onSubmit={onSubmit} {...props}>
      <Input placeholder="Type your query..." required className="mr-2" value={inputValue} onChange={onInputChange} disabled={disabled} />
      <Button type="submit" variant="outline" size="icon" disabled={disabled}><MagnifyingGlassIcon /></Button>
    </form>
  )
}