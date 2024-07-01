'use client'

import * as React from 'react'
import { usePathname, useRouter } from 'next/navigation'
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
import { CaretSortIcon, MinusIcon } from '@radix-ui/react-icons'
import { capitalizeFirstLetter, cn } from '@/lib/utils'
import { Collapsible, CollapsibleTrigger } from '@/components/ui/collapsible'
import { CollapsibleContent } from '@radix-ui/react-collapsible'

export const SPECIES_FORM_SCHEMA = z.object({
  chemicalClass: z.string(),
  use: z.string(),
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

const FORM_INIT_VALUES = {
  chemicalClass: '',
  use: '',
  property: Object.fromEntries(
    Object.values(OSpeciesPropertyKey).map(key => [
      key,
      { lower: '', upper: '' },
    ])
  ),
  identifier: Object.fromEntries(
    Object.values(OSpeciesIdentifierKey).map(key => [key, ''])
  ),
}

export interface SpeciesFormProps
  extends React.HTMLAttributes<HTMLFormElement> {
  initValues: z.infer<typeof SPECIES_FORM_SCHEMA>
  allChemicalClasses: ChemicalClass[]
  allUses: Use[]
}

export function SpeciesForm({
  initValues,
  allChemicalClasses,
  allUses,
  className,
  ...props
}: SpeciesFormProps) {
  const pathname = usePathname()
  const router = useRouter()

  const [isIdentifiersPanelOpen, setIsIdentifiersPanelOpen] =
    React.useState(false)
  const [isPropertiesPanelOpen, setIsPropertyPanelOpen] = React.useState(false)

  const form = useForm<z.infer<typeof SPECIES_FORM_SCHEMA>>({
    resolver: zodResolver(SPECIES_FORM_SCHEMA),
    defaultValues: FORM_INIT_VALUES,
  })

  React.useEffect(() => {
    form.setValue('chemicalClass', initValues.chemicalClass)
    form.setValue('use', initValues.use)
    form.setValue('property', initValues.property)
    form.setValue('identifier', initValues.identifier)
  }, [form, initValues])

  function onSubmit(values: z.infer<typeof SPECIES_FORM_SCHEMA>) {
    const chemicalClassParams = values.chemicalClass
      ? [['chemicalClass', encodeURI(values.chemicalClass)]]
      : []
    const useParams = values.use ? [['use', encodeURI(values.use)]] : []
    const propertyParams = Object.entries(values.property).flatMap(
      ([key, { lower, upper }]) =>
        [
          ['gte', lower],
          ['lte', upper],
        ]
          .filter(([_, val]) => val.length > 0)
          .map(([op, val]) => [key, `${op}:${val}`])
    ) as [string, string][]
    const identifierParams = Object.entries(values.identifier).filter(
      ([_, value]) => value.length > 0
    )

    const queryParams = new URLSearchParams([
      ...chemicalClassParams,
      ...useParams,
      ...propertyParams,
      ...identifierParams,
    ])
    router.push(`${pathname}?${queryParams}`)
  }

  return (
    <Form {...form}>
      <form
        onSubmit={form.handleSubmit(onSubmit)}
        className={cn('w-full', className)}
        {...props}
      >
        <div className='w-full grid lg:grid-cols-2 gap-4 mb-4'>
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
          <Collapsible
            open={isIdentifiersPanelOpen}
            onOpenChange={setIsIdentifiersPanelOpen}
            className='col-span-2'
          >
            <CollapsibleTrigger asChild>
              <Button variant='ghost' size='sm'>
                <CaretSortIcon className='h-4 w-4' />
                <span className='sr-only'>Toggle</span>
              </Button>
            </CollapsibleTrigger>
            Identifiers
            <CollapsibleContent className='grid grid-cols-2 gap-x-8 gap-y-4 mx-2'>
              {Object.values(OSpeciesIdentifierKey).map((key, i) => (
                <div key={i}>
                  <FormField
                    control={form.control}
                    name={`identifier.${key}`}
                    render={({ field }) => (
                      <FormItem>
                        <FormLabel>
                          {capitalizeFirstLetter(
                            SPECIES_IDENTIFIER_KEY_LABELS[key]
                          )}
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
            </CollapsibleContent>
          </Collapsible>
          <Collapsible className='col-span-2'>
            <CollapsibleTrigger asChild>
              <Button variant='ghost' size='sm'>
                <CaretSortIcon className='h-4 w-4' />
                <span className='sr-only'>Toggle</span>
              </Button>
            </CollapsibleTrigger>
            Properties
            <CollapsibleContent className='grid grid-cols-2 md:grid-cols-3 lg:grid-cols-4 gap-x-8 gap-y-4 mx-2'>
              {Object.values(OSpeciesPropertyKey).map((key, i) => (
                <FormField
                  key={i}
                  control={form.control}
                  name={`property.${key}`}
                  render={({ field }) => (
                    <FormItem>
                      <FormLabel>{key}</FormLabel>
                      <FormControl>
                        <div className='flex items-center space-x-2'>
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
                        </div>
                      </FormControl>
                    </FormItem>
                  )}
                />
              ))}
            </CollapsibleContent>
          </Collapsible>
        </div>
        <Button
          type='button'
          variant='secondary'
          onClick={() => form.reset()}
          className='w-full mb-2'
        >
          Reset fields
        </Button>
        <Button type='submit' className='w-full'>
          Search
        </Button>
      </form>
    </Form>
  )
}
