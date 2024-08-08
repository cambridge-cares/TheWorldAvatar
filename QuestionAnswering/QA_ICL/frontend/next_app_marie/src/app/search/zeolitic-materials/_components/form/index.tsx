'use client'

import * as React from 'react'
import { usePathname, useRouter, useSearchParams } from 'next/navigation'
import { z } from 'zod'
import { zodResolver } from '@hookform/resolvers/zod'
import { useForm } from 'react-hook-form'

import {
  Form,
  FormControl,
  FormField,
  FormItem,
  FormLabel,
} from '@/components/ui/form'
import { Button } from '@/components/ui/button'
import { Input } from '@/components/ui/input'
import { cn } from '@/lib/utils'
import {
  Journal,
  OUnitCellAngleKey,
  OUnitCellLengthKey,
  ZeoliteFrameworkBase,
} from '@/lib/model/ontozeolite'
import { MinMaxInput } from '@/components/ui/min-max-input'
import { Combobox } from '@/components/ui/combobox'
import { PtElement, SpeciesBase } from '@/lib/model/ontospecies'
import { ZEOMATERIAL_FORM_INIT_VALUES, ZEOMATERIAL_FORM_SCHEMA } from './model'
import {
  convertZeoMaterialFormToSearchParams,
  populateZeoMaterialFormFields,
} from './utils'

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

  const form = useForm<z.infer<typeof ZEOMATERIAL_FORM_SCHEMA>>({
    resolver: zodResolver(ZEOMATERIAL_FORM_SCHEMA),
    defaultValues: ZEOMATERIAL_FORM_INIT_VALUES,
  })

  React.useEffect(() => {
    populateZeoMaterialFormFields(form, searchParams)
  }, [form, searchParams])

  function onSubmit(values: z.infer<typeof ZEOMATERIAL_FORM_SCHEMA>) {
    const queryParams = convertZeoMaterialFormToSearchParams(values)
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
