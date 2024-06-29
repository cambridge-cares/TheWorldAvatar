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
  chemicalClasses: z.array(z.string())
})

export function SpeciesForm({ allChemicalClasses, allUses }: SpeciesFormProps) {
  const form = useForm<z.infer<typeof formSchema>>({
    resolver: zodResolver(formSchema),
    defaultValues: {
      chemicalClasses: []
    }
  })

  function onSubmit(values: z.infer<typeof formSchema>) {
    console.log(values)
  }

  const [open, setOpen] = React.useState(false)
  const chemicalClassIRI2labelLowercase = React.useMemo(() => Object.fromEntries(allChemicalClasses.map(({ IRI, label }) => [IRI, label.toLowerCase()])), [allChemicalClasses])

  return (
    <Form {...form}>
      <form onSubmit={form.handleSubmit(onSubmit)}>
        <FormField
          control={form.control}
          name="chemicalClasses"
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
                      Select chemical class...
                      <ChevronDownIcon className="ml-2 h-4 w-4 shrink-0 opacity-50" />
                    </Button>
                  </PopoverTrigger>
                  <PopoverContent className="w-[200px] p-0">
                    <Command
                      filter={(value, search) => {
                        return chemicalClassIRI2labelLowercase[value].includes(search.toLowerCase()) ? 1 : 0
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
                                if (field.value.includes(val)) {
                                  field.onChange(
                                    field.value.filter(iri => iri != val)
                                  )
                                } else {
                                  field.onChange([...field.value, val])
                                }
                              }}
                            >
                              {label}
                              <CheckIcon
                                className={cn(
                                  "ml-auto h-4 w-4",
                                  field.value.includes(IRI) ? "opacity-100" : "opacity-0"
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