'use client'

import * as React from 'react'
import { usePathname, useRouter, useSearchParams } from 'next/navigation'
import { z } from 'zod'
import { zodResolver } from '@hookform/resolvers/zod'
import { FormProvider, useForm } from 'react-hook-form'

import {
  Form,
  FormControl,
  FormField,
  FormItem,
  FormLabel,
} from '@/components/ui/form'
import { Button } from '@/components/ui/button'
import { Input } from '@/components/ui/input'
import { cn, extractLowerUpperParams } from '@/lib/utils'
import {
  Journal,
  OUnitCellAngleKey,
  OUnitCellLengthKey,
  UNIT_CELL_KEY_PREFIX,
  ZeoliteFrameworkBase,
} from '@/lib/model/ontozeolite'
import { MinMaxInput } from '@/components/ui/min-max-input'
import { Combobox } from '@/components/ui/combobox'
import { PtElement, SpeciesBase } from '@/lib/model/ontospecies'

const FRAMEWORK_KEY = 'Framework'
const MATERIAL_NAME_KEY = 'name'
const CHEMICAL_FORMULA_KEY = 'ChemicalFormula'
const FRAMEWORK_COMPONENT_KEY = 'FrameworkComponent'
const GUEST_COMPONENT_KEY = 'GuestComponent'
const AUTHOR_LAST_NAME_KEY = 'Author-family_name'
const CITATION_YEAR_KEY = 'year'
const JOURNAL_KEY = 'Journal'
const DOI_KEY = 'doi'

export const ZEOLITIC_MATERIAL_FORM_SCHEMA = z.object({
  framework: z.string(),
  name: z.string(),
  formula: z.string(),
  elements: z.array(z.string()),
  guests: z.array(z.string()),
  unitCell: z.object({
    lengths: z.object(
      Object.fromEntries(
        Object.values(OUnitCellLengthKey).map(key => [
          key,
          z.object({ lower: z.string(), upper: z.string() }),
        ])
      )
    ),
    angles: z.object(
      Object.fromEntries(
        Object.values(OUnitCellAngleKey).map(key => [
          key,
          z.object({ lower: z.string(), upper: z.string() }),
        ])
      )
    ),
  }),
  citation: z.object({
    authorLastName: z.string(),
    year: z.string(),
    journal: z.string(),
    doi: z.string(),
  }),
})

const FORM_INIT_VALUES = {
  framework: '',
  name: '',
  formula: '',
  elements: [],
  guests: [],
  unitCell: {
    lengths: Object.fromEntries(
      Object.values(OUnitCellLengthKey).map(key => [
        key,
        { lower: '', upper: '' },
      ])
    ),
    angles: Object.fromEntries(
      Object.values(OUnitCellAngleKey).map(key => [
        key,
        { lower: '', upper: '' },
      ])
    ),
  },
  citation: {
    authorLastName: '',
    year: '',
    journal: '',
    doi: '',
  },
}

export interface ZeoliticMaterialFormProps
  extends React.HTMLAttributes<HTMLFormElement> {
  frameworkOptions: ZeoliteFrameworkBase[]
  frameworkComponentOptions: PtElement[]
  guestOptions: SpeciesBase[]
  journalOptions: Journal[]
}

export function ZeoliticMaterialForm({
  frameworkOptions,
  frameworkComponentOptions,
  guestOptions,
  journalOptions,
  className,
  ...props
}: ZeoliticMaterialFormProps) {
  const searchParams = useSearchParams()
  const pathname = usePathname()
  const router = useRouter()

  const form = useForm<z.infer<typeof ZEOLITIC_MATERIAL_FORM_SCHEMA>>({
    resolver: zodResolver(ZEOLITIC_MATERIAL_FORM_SCHEMA),
    defaultValues: FORM_INIT_VALUES,
  })

  React.useEffect(() => {
    const framework = searchParams.get(FRAMEWORK_KEY)
    if (framework) form.setValue('framework', framework)

    const name = searchParams.get(MATERIAL_NAME_KEY)
    if (name) form.setValue('name', name)

    const formula = searchParams.get(CHEMICAL_FORMULA_KEY)
    if (formula) form.setValue('formula', formula)

    const elements = searchParams.getAll(FRAMEWORK_COMPONENT_KEY)
    if (elements.length > 0) form.setValue('elements', elements)

    const guests = searchParams.getAll(GUEST_COMPONENT_KEY)
    if (guests.length > 0) form.setValue('guests', guests)

    const unitCellLengths = extractLowerUpperParams(
      searchParams,
      Object.values(OUnitCellLengthKey),
      UNIT_CELL_KEY_PREFIX
    )
    const unitCellAngles = extractLowerUpperParams(
      searchParams,
      Object.values(OUnitCellAngleKey),
      UNIT_CELL_KEY_PREFIX
    )
    form.setValue('unitCell', {
      lengths: unitCellLengths,
      angles: unitCellAngles,
    })

    const authorLastName = searchParams.get(AUTHOR_LAST_NAME_KEY)
    if (authorLastName) form.setValue('citation.authorLastName', authorLastName)

    const year = searchParams.get(CITATION_YEAR_KEY)
    if (year) form.setValue('citation.year', year)

    const journal = searchParams.get(JOURNAL_KEY)
    if (journal) form.setValue('citation.journal', journal)

    const doi = searchParams.get(DOI_KEY)
    if (doi) form.setValue('citation.doi', doi)
  }, [form, searchParams])

  function onSubmit(values: z.infer<typeof ZEOLITIC_MATERIAL_FORM_SCHEMA>) {
    const unitCellParams = Object.values(values.unitCell).flatMap(params =>
      Object.entries(params).flatMap(([key, { lower, upper }]) =>
        [
          ['gte', lower],
          ['lte', upper],
        ]
          .filter(([_, val]) => val.length > 0)
          .map(([op, val]) => [key, `${op}:${val}`] as [string, string])
      )
    )

    const queryParams = new URLSearchParams(
      [
        values.framework ? [FRAMEWORK_KEY, values.framework] : undefined,
        values.name ? [MATERIAL_NAME_KEY, values.name] : undefined,
        values.formula ? [CHEMICAL_FORMULA_KEY, values.formula] : undefined,
        ...values.elements.map(
          x => [FRAMEWORK_COMPONENT_KEY, x] as [string, string]
        ),
        ...values.guests.map(x => [GUEST_COMPONENT_KEY, x] as [string, string]),
        ...unitCellParams,
        values.citation.authorLastName
          ? [AUTHOR_LAST_NAME_KEY, values.citation.authorLastName]
          : undefined,
        values.citation.year
          ? [CITATION_YEAR_KEY, values.citation.year]
          : undefined,
        values.citation.journal
          ? [JOURNAL_KEY, values.citation.journal]
          : undefined,
        values.citation.doi ? [DOI_KEY, values.citation.doi] : undefined,
      ].filter(x => x !== undefined)
    )
    router.push(`${pathname}?${queryParams}`)
  }

  return (
    <Form {...form}>
      <form
        onSubmit={form.handleSubmit(onSubmit)}
        className={cn('w-full', className)}
        {...props}
      >
        <div className='w-full flex flex-col space-y-8'>
          <div className='grid md:grid-cols-3 gap-x-4'>
            <FormField
              control={form.control}
              name='framework'
              render={({ field }) => (
                <FormItem>
                  <FormLabel className='font-semibold text-lg'>
                    Framework
                  </FormLabel>
                  <FormControl>
                    <Combobox
                      items={frameworkOptions.map(x => ({
                        value: x.IRI,
                        label: x.code,
                      }))}
                      value={field.value}
                      onCmdItemSelect={value => field.onChange(value)}
                    />
                  </FormControl>
                </FormItem>
              )}
            />
            <FormField
              control={form.control}
              name='name'
              render={({ field }) => (
                <FormItem>
                  <FormLabel className='font-semibold text-lg'>
                    Material label
                  </FormLabel>
                  <FormControl>
                    <Input
                      value={field.value}
                      onChange={evt => field.onChange(evt.target.value)}
                    />
                  </FormControl>
                </FormItem>
              )}
            />
            <FormField
              control={form.control}
              name='formula'
              render={({ field }) => (
                <FormItem>
                  <FormLabel className='font-semibold text-lg'>
                    Material formula
                  </FormLabel>
                  <FormControl>
                    <Input
                      value={field.value}
                      onChange={evt => field.onChange(evt.target.value)}
                    />
                  </FormControl>
                </FormItem>
              )}
            />
          </div>
          <FormField
            control={form.control}
            name='elements'
            render={({ field }) => (
              <FormItem>
                <FormLabel className='font-semibold text-lg'>
                  Framework composition
                </FormLabel>
                <FormControl>
                  <Combobox
                    items={frameworkComponentOptions.map(({ IRI, symbol }) => ({
                      value: IRI,
                      label: symbol,
                    }))}
                    value={field.value}
                    onCmdItemSelect={val =>
                      field.value.includes(val)
                        ? field.onChange(field.value.filter(x => x !== val))
                        : field.onChange([...field.value, val])
                    }
                  />
                </FormControl>
              </FormItem>
            )}
          />
          <FormField
            control={form.control}
            name='guests'
            render={({ field }) => (
              <FormItem>
                <FormLabel className='font-semibold text-lg'>
                  Guest molecules
                </FormLabel>
                <Combobox
                  itemCls='guest molecule'
                  items={guestOptions.map(
                    ({ IRI, label, IUPACName, InChI }) => ({
                      value: IRI,
                      label: label
                        ? `${label} (${IUPACName || InChI})`
                        : IUPACName || InChI,
                    })
                  )}
                  value={field.value}
                  onCmdItemSelect={val =>
                    field.value.includes(val)
                      ? field.onChange(field.value.filter(x => x !== val))
                      : field.onChange([...field.value, val])
                  }
                />
              </FormItem>
            )}
          />
          <div>
            <div className='font-semibold text-lg'>Unit cell parameters</div>
            <div className='mb-2'>
              <div>Lengths</div>
              <div className='grid md:grid-cols-3 gap-4'>
                {Object.values(OUnitCellLengthKey).map((key, i) => (
                  <FormField
                    key={i}
                    control={form.control}
                    name={`unitCell.lengths.${key}`}
                    render={({ field }) => (
                      <FormItem>
                        <FormLabel>{key} (Å)</FormLabel>
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
              </div>
            </div>
            <div>
              <div>Angles</div>
              <div className='grid md:grid-cols-3 gap-4'>
                {Object.values(OUnitCellAngleKey).map((key, i) => (
                  <FormField
                    key={i}
                    control={form.control}
                    name={`unitCell.angles.${key}`}
                    render={({ field }) => (
                      <FormItem>
                        <FormLabel>{key} (°)</FormLabel>
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
              </div>
            </div>
          </div>
          <div>
            <div className='font-semibold text-lg'>Citation</div>
            <div className='grid md:grid-cols-2 gap-x-4 gap-y-2'>
              <FormField
                control={form.control}
                name='citation.authorLastName'
                render={({ field }) => (
                  <FormItem>
                    <FormLabel>Author&apos;s last name</FormLabel>
                    <FormControl>
                      <Input
                        value={field.value}
                        onChange={evt => field.onChange(evt.target.value)}
                      />
                    </FormControl>
                  </FormItem>
                )}
              />
              <FormField
                control={form.control}
                name='citation.year'
                render={({ field }) => (
                  <FormItem>
                    <FormLabel>Year</FormLabel>
                    <FormControl>
                      <Input
                        type='number'
                        value={field.value}
                        onChange={evt => field.onChange(evt.target.value)}
                      />
                    </FormControl>
                  </FormItem>
                )}
              />
              <FormField
                control={form.control}
                name='citation.journal'
                render={({ field }) => (
                  <FormItem>
                    <FormLabel>Journal</FormLabel>
                    <FormControl>
                      <Combobox
                        value={field.value}
                        items={journalOptions.map(({ IRI, title }) => ({
                          value: IRI,
                          label: title,
                        }))}
                        onCmdItemSelect={val => field.onChange(val)}
                      />
                    </FormControl>
                  </FormItem>
                )}
              />
              <FormField
                control={form.control}
                name='citation.doi'
                render={({ field }) => (
                  <FormItem>
                    <FormLabel>DOI</FormLabel>
                    <FormControl>
                      <Input
                        value={field.value}
                        onChange={evt => field.onChange(evt.target.value)}
                      />
                    </FormControl>
                  </FormItem>
                )}
              />
            </div>
          </div>
        </div>
        <Button
          type='button'
          variant='secondary'
          onClick={() => form.reset()}
          className='w-full mt-4 mb-2'
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
