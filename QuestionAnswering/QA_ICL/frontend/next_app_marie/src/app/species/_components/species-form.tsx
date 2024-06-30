'use client'

import * as React from 'react'
import { z } from 'zod'
import { zodResolver } from '@hookform/resolvers/zod'
import { useForm } from 'react-hook-form'

import { ChemicalClass, Use } from '@/lib/model/ontospecies'
import {
  Form,
  FormControl,
  FormField,
  FormItem,
  FormLabel,
} from '@/components/ui/form'
import { Combobox } from '@/components/ui/combobox'

export interface SpeciesFormProps {
  allChemicalClasses: ChemicalClass[]
  allUses: Use[]
}

const formSchema = z.object({
  chemicalClass: z.union([z.string(), z.undefined()]),
})

export function SpeciesForm({ allChemicalClasses, allUses }: SpeciesFormProps) {
  const form = useForm<z.infer<typeof formSchema>>({
    resolver: zodResolver(formSchema),
    defaultValues: {
      chemicalClass: undefined,
    },
  })

  function onSubmit(values: z.infer<typeof formSchema>) {
    console.log(values)
  }

  return (
    <Form {...form}>
      <form onSubmit={form.handleSubmit(onSubmit)}>
        <FormField
          control={form.control}
          name='chemicalClass'
          render={({ field }) => (
            <FormItem>
              <FormLabel>Chemical class</FormLabel>
              <FormControl>
                <Combobox
                  itemCls='chemical class'
                  value={field.value}
                  setValue={field.onChange}
                  items={allChemicalClasses.map(({ IRI, label }) => ({
                    value: IRI,
                    label,
                  }))}
                />
              </FormControl>
            </FormItem>
          )}
        ></FormField>
      </form>
    </Form>
  )
}
