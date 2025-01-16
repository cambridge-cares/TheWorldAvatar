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
import { SPECIES_FORM_INIT_VALUES, SPECIES_FORM_SCHEMA } from './model'
import {
  convertSpeciesFormToSearchParams,
  populateSpeciesFormFields,
} from './utils'

const SPECIES_IDENTIFIER_KEY_LABELS = {
  [OSpeciesIdentifierKey.CID]: 'CID',
  [OSpeciesIdentifierKey.CHEBI_ID]: 'ChEBI ID',
  [OSpeciesIdentifierKey.IUPAC_NAME]: 'IUPAC name',
  [OSpeciesIdentifierKey.INCHI]: 'InChI',
  [OSpeciesIdentifierKey.INCHI_KEY]: 'InChIKey',
  [OSpeciesIdentifierKey.MOLECULAR_FORMULA]: 'molecular formula',
  [OSpeciesIdentifierKey.SMILES]: 'SMILES string',
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
    defaultValues: SPECIES_FORM_INIT_VALUES,
  })

  React.useEffect(() => {
    populateSpeciesFormFields(form, searchParams)
  }, [form, searchParams])

  function onSubmit(values: z.infer<typeof SPECIES_FORM_SCHEMA>) {
    const queryParams = convertSpeciesFormToSearchParams(values)
    router.push(`${pathname}?${queryParams}`)
  }

  return (
    <Form {...form}>
      <form
        onSubmit={form.handleSubmit(onSubmit)}
        className={cn('w-full flex flex-col space-y-4', className)}
        {...props}
      >
        <div className='grid lg:grid-cols-2 gap-x-8'>
          <FormField
            control={form.control}
            name='chemicalClass'
            render={({ field }) => (
              <FormItem>
                <FormLabel className='font-semibold text-lg'>
                  Chemical class
                </FormLabel>
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
                <FormLabel className='font-semibold text-lg'>Use</FormLabel>
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
                  />
                </FormControl>
              </FormItem>
            )}
          />
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
