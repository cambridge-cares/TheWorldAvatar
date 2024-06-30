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
import { Button } from '@/components/ui/button'

export interface SpeciesFormProps {
  allChemicalClasses: ChemicalClass[]
  allUses: Use[]
}

const FORM_SCHEMA = z.object({
  chemicalClass: z.union([z.string(), z.undefined()]),
  use: z.union([z.string(), z.undefined()]),
})

export function SpeciesForm({ allChemicalClasses, allUses }: SpeciesFormProps) {
  const form = useForm<z.infer<typeof FORM_SCHEMA>>({
    resolver: zodResolver(FORM_SCHEMA),
    defaultValues: {
      chemicalClass: undefined,
      use: undefined,
    },
  })

  function onSubmit(values: z.infer<typeof FORM_SCHEMA>) {
    console.log(values)
  }

  return (
    <Form {...form}>
      <form onSubmit={form.handleSubmit(onSubmit)} className='w-full px-4 flex flex-col items-center'>
        <div className='w-full md:max-w-screen-md lg:max-w-screen-lg grid lg:grid-cols-2 gap-4 mb-4'>
        <FormField
          control={form.control}
          name='chemicalClass'
          render={({ field }) => (
            <FormItem>
              <FormLabel>Chemical class</FormLabel>
              <FormControl>
                <div className='w-full'>
                  <Combobox
                    itemCls='chemical class'
                    value={field.value}
                    setValue={field.onChange}
                    items={allChemicalClasses.map(({ IRI, label }) => ({
                      value: IRI,
                      label,
                    }))}
                  />
                </div>
              </FormControl>
            </FormItem>
          )}
        ></FormField>
        <FormField
          control={form.control}
          name='use'
          render={({ field }) => (
            <FormItem>
              <FormLabel>Use</FormLabel>
              <FormControl>
                <div className='w-full'>
                  <Combobox
                    itemCls='use'
                    value={field.value}
                    setValue={field.onChange}
                    items={allUses.map(({ IRI, label }) => ({
                      value: IRI,
                      label,
                    }))}
                  />
                </div>
              </FormControl>
            </FormItem>
          )}
        ></FormField>
        </div>
        <Button type="submit">Search</Button>
      </form>
    </Form>
  )
}
