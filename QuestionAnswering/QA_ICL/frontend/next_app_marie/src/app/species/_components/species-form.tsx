"use client"

import * as React from "react"
import { z } from "zod"
import { zodResolver } from "@hookform/resolvers/zod"
import { useForm } from "react-hook-form"

import { cn } from "@/lib/utils"
import { Button } from "@/components/ui/button"
import { Command, CommandEmpty, CommandGroup, CommandInput, CommandItem, CommandList } from "@/components/ui/command"
import { Popover, PopoverContent, PopoverTrigger } from "@/components/ui/popover"
import { ChemicalClass, Use } from "@/lib/model/ontospecies"
import { CheckIcon, ChevronDownIcon } from "@radix-ui/react-icons"
import { Form, FormControl, FormField, FormItem, FormLabel } from "@/components/ui/form"


export interface SpeciesFormProps {
  allChemicalClasses: ChemicalClass[]
  allUses: Use[]
}

const formSchema = z.object({
  chemicalClass: z.union([z.string(), z.undefined()])
})

export function SpeciesForm({ allChemicalClasses, allUses }: SpeciesFormProps) {
  const form = useForm<z.infer<typeof formSchema>>({
    resolver: zodResolver(formSchema),
    defaultValues: {
      chemicalClass: undefined
    }
  })

  function onSubmit(values: z.infer<typeof formSchema>) {
    console.log(values)
  }

  const [open, setOpen] = React.useState(false)
  const chemicalClass_iri2label = React.useMemo(() => Object.fromEntries(allChemicalClasses.map(({IRI, label}) => [IRI, label])), [allChemicalClasses])
  const chemicalClass_iri2labelLower = React.useMemo(() => Object.fromEntries(Object.entries(chemicalClass_iri2label).map(([IRI, label]) => [IRI, label.toLowerCase()])), [chemicalClass_iri2label])

  console.log("Form")

  return (
    <Form {...form}>
      <form onSubmit={form.handleSubmit(onSubmit)}>
        <FormField
          control={form.control}
          name="chemicalClass"
          render={({ field }) => (
            <FormItem>
              <FormLabel>Chemical class</FormLabel>
              <FormControl>
                <Popover open={open} onOpenChange={setOpen}>
                  <PopoverTrigger asChild>
                    <Button
                      variant="outline"
                      role="combobox"
                      aria-expanded={open}
                      className="w-[200px] justify-between"
                    >
                      {field.value ? chemicalClass_iri2label[field.value] : "Select chemical class..."}
                      <ChevronDownIcon className="ml-2 h-4 w-4 shrink-0 opacity-50" />
                    </Button>
                  </PopoverTrigger>
                  <PopoverContent className="w-[200px] p-0">
                    <Command
                      filter={(value, search) => {
                        return chemicalClass_iri2labelLower[value].includes(search.toLowerCase()) ? 1 : 0
                      }}
                    >
                      <CommandInput placeholder="Search framework..." className="h-9" />
                      <CommandList>
                        <CommandEmpty>No chemical class found.</CommandEmpty>
                        <CommandGroup>
                          {allChemicalClasses.map(({ IRI, label }, i) => (
                            <CommandItem
                              key={i}
                              value={IRI}
                              onSelect={(val) => {
                                if (field.value === val) {
                                  field.onChange(undefined)
                                } else {
                                  field.onChange(val)
                                }
                                setOpen(false)
                              }}
                            >
                              {label}
                              <CheckIcon
                                className={cn(
                                  "ml-auto h-4 w-4",
                                  field.value === IRI ? "opacity-100" : "opacity-0"
                                )}
                              />
                            </CommandItem>
                          ))}
                        </CommandGroup>
                      </CommandList>
                    </Command>
                  </PopoverContent>
                </Popover>
              </FormControl>
            </FormItem>
          )}
        >
        </FormField>
      </form>
    </Form>
  )
} 