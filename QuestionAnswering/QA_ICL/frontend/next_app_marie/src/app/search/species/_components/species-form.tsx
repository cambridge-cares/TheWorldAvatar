'use client'

import * as React from 'react'
import { z } from 'zod'
import { zodResolver } from '@hookform/resolvers/zod'
import { useForm } from 'react-hook-form'

import {
  ChemicalClass,
  OSpeciesIdentifierKey,
  OSpeciesPropertyKey,
  SPECIES_IDENTIFIER_KEY_LABELS,
  Use,
} from '@/lib/model/ontospecies'
import {
  Form,
  FormControl,
  FormField,
  FormItem,
  FormLabel,
} from '@/components/ui/form'
import { Combobox } from '@/components/ui/combobox'
import { Button } from '@/components/ui/button'
import { Input } from '@/components/ui/input'
import { MinusIcon } from '@radix-ui/react-icons'
import {
  Accordion,
  AccordionContent,
  AccordionTrigger,
} from '@/components/ui/accordion'
import { AccordionItem } from '@radix-ui/react-accordion'

export interface SpeciesFormProps {
  allChemicalClasses: ChemicalClass[]
  allUses: Use[]
}

const FORM_SCHEMA = z.object({
  chemicalClass: z.union([z.string(), z.undefined()]),
  use: z.union([z.string(), z.undefined()]),
  property: z.object(
    Object.fromEntries(
      Object.values(OSpeciesPropertyKey).map(key => [
        key,
        z.object({ lower: z.string(), upper: z.string() }),
      ])
    )
  ),
  identifier: z.object(
    Object.fromEntries(
      Object.values(OSpeciesIdentifierKey).map(key => [key, z.string()])
    )
  ),
})

export function SpeciesForm({ allChemicalClasses, allUses }: SpeciesFormProps) {
  const form = useForm<z.infer<typeof FORM_SCHEMA>>({
    resolver: zodResolver(FORM_SCHEMA),
    defaultValues: {
      chemicalClass: undefined,
      use: undefined,
      property: Object.fromEntries(
        Object.values(OSpeciesPropertyKey).map(key => [
          key,
          { lower: '', upper: '' },
        ])
      ),
      identifier: Object.fromEntries(
        Object.values(OSpeciesIdentifierKey).map(key => [key, ''])
      )
    },
  })

  function onSubmit(values: z.infer<typeof FORM_SCHEMA>) {
    console.log(values)
  }

  return (
    <Form {...form}>
      <form
        onSubmit={form.handleSubmit(onSubmit)}
        className='w-full px-4 flex flex-col items-center'
      >
        <div className='w-full md:max-w-screen-md lg:max-w-screen-lg grid lg:grid-cols-2 gap-4 mb-4'>
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
          />
          <FormField
            control={form.control}
            name='use'
            render={({ field }) => (
              <FormItem>
                <FormLabel>Use</FormLabel>
                <FormControl>
                  <Combobox
                    itemCls='use'
                    value={field.value}
                    setValue={field.onChange}
                    items={allUses.map(({ IRI, label }) => ({
                      value: IRI,
                      label,
                    }))}
                  />
                </FormControl>
              </FormItem>
            )}
          />
          <Accordion type='multiple' className='col-span-2'>
            <AccordionItem value='identifier'>
              <AccordionTrigger>Identifiers</AccordionTrigger>
              <AccordionContent className='grid grid-cols-2 gap-x-8 gap-y-4 mx-2'>
                {Object.values(OSpeciesIdentifierKey).map((key, i) => (
                  <div key={i}>
                    <FormField
                      control={form.control}
                      name={`identifier.${key}`}
                      render={({ field }) => (
                        <FormItem>
                          <FormLabel>
                            {SPECIES_IDENTIFIER_KEY_LABELS[key]}
                          </FormLabel>
                          <FormControl>
                            <Input
                              value={field.value}
                              onChange={e => field.onChange(e.target.value)}
                            />
                          </FormControl>
                        </FormItem>
                      )}
                    />
                  </div>
                ))}
              </AccordionContent>
            </AccordionItem>
            <AccordionItem value='property'>
              <AccordionTrigger>Properties</AccordionTrigger>
              <AccordionContent className='grid grid-cols-4 gap-x-8 gap-y-4 mx-2'>
                {Object.values(OSpeciesPropertyKey).map((key, i) => (
                  <FormField
                    key={i}
                    control={form.control}
                    name={`property.${key}`}
                    render={({ field }) => (
                      <FormItem>
                        <FormLabel>{key}</FormLabel>
                        <FormControl>
                          <Input
                            type='number'
                            value={field.value.upper}
                            onChange={e =>
                              field.onChange({
                                upper: e.target.value,
                                lower: field.value.lower,
                              })
                            }
                            placeholder='min'
                          />
                          <MinusIcon className='h-4 w-4' />
                        </FormControl>
                        <Input
                          type='number'
                          value={field.value.lower}
                          onChange={e =>
                            field.onChange({
                              upper: field.value.upper,
                              lower: e.target.value,
                            })
                          }
                          placeholder='max'
                        />
                      </FormItem>
                    )}
                  />
                ))}
              </AccordionContent>
            </AccordionItem>
          </Accordion>
        </div>
        <Button type='submit'>Search</Button>
      </form>
    </Form>
  )
}
