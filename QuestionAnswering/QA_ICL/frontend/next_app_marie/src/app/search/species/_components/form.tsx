'use client'

import * as React from 'react'
import { usePathname, useRouter, useSearchParams } from 'next/navigation'
import { z } from 'zod'
import { zodResolver } from '@hookform/resolvers/zod'
import { useForm } from 'react-hook-form'

import {
  CHEMICAL_CLASS_KEY,
  ChemicalClass,
  OSpeciesIdentifierKey,
  OSpeciesPropertyKey,
  Use,
  USE_KEY,
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
import { capitalize, cn, extractLowerUpperParams } from '@/lib/utils'
import { Collapsible, CollapsibleTrigger } from '@/components/ui/collapsible'
import { CollapsibleContent } from '@radix-ui/react-collapsible'
import { MinMaxInput } from '@/components/ui/min-max-input'

const SPECIES_IDENTIFIER_KEY_LABELS = {
  [OSpeciesIdentifierKey.CID]: 'CID',
  [OSpeciesIdentifierKey.CHEBI_ID]: 'ChEBI ID',
  [OSpeciesIdentifierKey.IUPAC_NAME]: 'IUPAC name',
  [OSpeciesIdentifierKey.INCHI]: 'InChI',
  [OSpeciesIdentifierKey.INCHI_KEY]: 'InChIKey',
  [OSpeciesIdentifierKey.MOLECULAR_FORMULA]: 'molecular formula',
  [OSpeciesIdentifierKey.SMILES]: 'SMILES string',
}

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
  chemicalClassOptions: ChemicalClass[]
  useOptions: Use[]
}

export function SpeciesForm({
  chemicalClassOptions,
  useOptions,
  className,
  ...props
}: SpeciesFormProps) {
  const searchParams = useSearchParams()
  const pathname = usePathname()
  const router = useRouter()

  const [isIdentifiersPanelOpen, setIsIdentifiersPanelOpen] =
    React.useState(true)
  const [isPropertiesPanelOpen, setIsPropertiesPanelOpen] =
    React.useState(false)

  const form = useForm<z.infer<typeof SPECIES_FORM_SCHEMA>>({
    resolver: zodResolver(SPECIES_FORM_SCHEMA),
    defaultValues: FORM_INIT_VALUES,
  })

  React.useEffect(() => {
    const chemicalClass = searchParams.get(CHEMICAL_CLASS_KEY)
    if (chemicalClass) {
      form.setValue('chemicalClass', chemicalClass)
    }

    const use = searchParams.get(USE_KEY)
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

    const property = extractLowerUpperParams(
      searchParams,
      Object.values(OSpeciesPropertyKey)
    )
    form.setValue('property', property)
  }, [form, searchParams])

  function onSubmit(values: z.infer<typeof SPECIES_FORM_SCHEMA>) {
    const chemicalClassParams: [string, string][] = values.chemicalClass
      ? [[CHEMICAL_CLASS_KEY, encodeURI(values.chemicalClass)]]
      : []
    const useParams: [string, string][] = values.use
      ? [[USE_KEY, encodeURI(values.use)]]
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
        className={cn('w-full flex flex-col space-y-4', className)}
        {...props}
      >
        <div>
          <div className='font-semibold text-lg'>Chemical class and use</div>
          <div className='grid lg:grid-cols-2 gap-x-8'>
            <FormField
              control={form.control}
              name='chemicalClass'
              render={({ field }) => (
                <FormItem>
                  <FormLabel>Chemical class</FormLabel>
                  <FormControl>
                    <Combobox
                      itemCls='chemical class'
                      items={chemicalClassOptions.map(({ IRI, label }) => ({
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
                      items={useOptions.map(({ IRI, label }) => ({
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
          </div>
        </div>
        <Collapsible
          open={isIdentifiersPanelOpen}
          onOpenChange={setIsIdentifiersPanelOpen}
          className='col-span-2'
        >
          <div className='flex items-center'>
            <span className='font-semibold text-lg'>Identifiers</span>
            <CollapsibleTrigger asChild>
              <Button variant='ghost' size='sm'>
                <CaretSortIcon className='h-4 w-4' />
                <span className='sr-only'>Toggle</span>
              </Button>
            </CollapsibleTrigger>
          </div>
          <CollapsibleContent className='grid grid-cols-2 gap-x-8 gap-y-4'>
            {Object.values(OSpeciesIdentifierKey).map((key, i) => (
              <div key={i}>
                <FormField
                  control={form.control}
                  name={`identifier.${key}`}
                  render={({ field }) => (
                    <FormItem>
                      <FormLabel>
                        {capitalize(SPECIES_IDENTIFIER_KEY_LABELS[key])}
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
          <div className='flex items-center'>
            <span className='font-semibold text-lg'>Properties</span>
            <CollapsibleTrigger asChild>
              <Button variant='ghost' size='sm'>
                <CaretSortIcon className='h-4 w-4' />
                <span className='sr-only'>Toggle</span>
              </Button>
            </CollapsibleTrigger>
          </div>
          <CollapsibleContent className='grid grid-cols-2 md:grid-cols-3 lg:grid-cols-4 gap-x-8 gap-y-4'>
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
        <div>
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
        </div>
      </form>
    </Form>
  )
}
