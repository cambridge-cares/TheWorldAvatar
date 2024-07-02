'use client'

import * as React from 'react'
import { usePathname, useRouter, useSearchParams } from 'next/navigation'
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
import { CaretSortIcon } from '@radix-ui/react-icons'
import { capitalizeFirstLetter, cn, extractLowerUpperParams } from '@/lib/utils'
import { Collapsible, CollapsibleTrigger } from '@/components/ui/collapsible'
import { CollapsibleContent } from '@radix-ui/react-collapsible'
import { MinMaxInput } from '@/components/ui/min-max-input'

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
  allChemicalClasses: ChemicalClass[]
  allUses: Use[]
}

export function SpeciesForm({
  allChemicalClasses,
  allUses,
  className,
  ...props
}: SpeciesFormProps) {
  const searchParams = useSearchParams()
  const pathname = usePathname()
  const router = useRouter()

  const [isIdentifiersPanelOpen, setIsIdentifiersPanelOpen] =
    React.useState(false)
  const [isPropertiesPanelOpen, setIsPropertiesPanelOpen] =
    React.useState(false)

  const form = useForm<z.infer<typeof SPECIES_FORM_SCHEMA>>({
    resolver: zodResolver(SPECIES_FORM_SCHEMA),
    defaultValues: FORM_INIT_VALUES,
  })

  React.useEffect(() => {
    const chemicalClass = searchParams.get('chemical-class')
    if (chemicalClass) {
      form.setValue('chemicalClass', chemicalClass)
    }

    const use = searchParams.get('use')
    if (use) {
      form.setValue('use', use)
    }

    const identifier = Object.fromEntries(
      Object.values(OSpeciesIdentifierKey).map(key => [
        key,
        searchParams.get(key) || '',
      ])
    )
    form.setValue('identifier', identifier)

    const property = extractLowerUpperParams(searchParams, OSpeciesPropertyKey)
    form.setValue('property', property)
  }, [form, searchParams])

  function onSubmit(values: z.infer<typeof SPECIES_FORM_SCHEMA>) {
    const chemicalClassParams = values.chemicalClass
      ? [
          ['chemical-class', encodeURI(values.chemicalClass)] as [
            string,
            string,
          ],
        ]
      : []
    const useParams = values.use
      ? [['use', encodeURI(values.use)] as [string, string]]
      : []
    const propertyParams = Object.entries(values.property).flatMap(
      ([key, { lower, upper }]) =>
        [
          ['gte', lower],
          ['lte', upper],
        ]
          .filter(([_, val]) => val.length > 0)
          .map(([op, val]) => [key, `${op}:${val}`] as [string, string])
    )
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
                    items={allChemicalClasses.map(({ IRI, label }) => ({
                      value: IRI,
                      label,
                    }))}
                    value={field.value}
                    onCmdItemSelect={value =>
                      value === field.value
                        ? field.onChange('')
                        : field.onChange(value)
                    }
                    closePopoverOnCmdItemSelect
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
                    items={allUses.map(({ IRI, label }) => ({
                      value: IRI,
                      label,
                    }))}
                    value={field.value}
                    onCmdItemSelect={value =>
                      value === field.value
                        ? field.onChange('')
                        : field.onChange(value)
                    }
                    closePopoverOnCmdItemSelect
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
          <Collapsible
            open={isPropertiesPanelOpen}
            onOpenChange={setIsPropertiesPanelOpen}
            className='col-span-2'
          >
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
                        <MinMaxInput
                          minValue={field.value.lower}
                          onMinChange={e =>
                            field.onChange({
                              ...field.value,
                              lower: e.target.value,
                            })
                          }
                          maxValue={field.value.upper}
                          onMaxChange={e =>
                            field.onChange({
                              ...field.value,
                              upper: e.target.value,
                            })
                          }
                        />
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
