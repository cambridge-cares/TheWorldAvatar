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
import { MinusCircledIcon, PlusCircledIcon } from '@radix-ui/react-icons'
import { cn } from '@/lib/utils'
import {
  OUnitCellAngleKey,
  OUnitCellLengthKey,
  TOPO_PROP_UNITS,
} from '@/lib/model/ontozeolite'
import { MinMaxInput } from '@/components/ui/min-max-input'
import { Combobox } from '@/components/ui/combobox'
import {
  ZEOFRAMEWORK_FORM_INIT_VALUES,
  ZEOFRAMEWORK_FORM_SCHEMA,
} from './model'
import {
  convertZeoFrameworkFormToSearchParams,
  populateZeoFrameworkFormFields,
} from './utils'

export interface ZeoFrameworkFormProps
  extends React.HTMLAttributes<HTMLFormElement> {
  CBUOptions: string[]
  SBUOptions: string[]
}

export function ZeoFrameworkForm({
  CBUOptions,
  SBUOptions,
  className,
  ...props
}: ZeoFrameworkFormProps) {
  const searchParams = useSearchParams()
  const pathname = usePathname()
  const router = useRouter()

  const form = useForm<z.infer<typeof ZEOFRAMEWORK_FORM_SCHEMA>>({
    resolver: zodResolver(ZEOFRAMEWORK_FORM_SCHEMA),
    defaultValues: ZEOFRAMEWORK_FORM_INIT_VALUES,
  })

  React.useEffect(() => {
    populateZeoFrameworkFormFields(form, searchParams)
  }, [form, searchParams])

  function onSubmit(values: z.infer<typeof ZEOFRAMEWORK_FORM_SCHEMA>) {
    const queryParams = convertZeoFrameworkFormToSearchParams(values)
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
          <FormField
            control={form.control}
            name='xrdPeaks'
            render={({ field }) => (
              <FormItem>
                <FormLabel className='font-semibold text-lg'>
                  XRD peaks
                </FormLabel>
                <FormControl>
                  <div className='grid md:grid-cols-2 gap-x-8 gap-y-4 place-items-center'>
                    {field.value.map(({ position, width, threshold }, i) => (
                      <div key={i}>
                        <div className='flex items-center space-x-2'>
                          <div>Peak {i + 1}</div>
                          {field.value.length >= 2 && (
                            <Button variant='ghost' className='p-2'>
                              <MinusCircledIcon
                                onClick={() =>
                                  field.onChange([
                                    ...field.value.slice(0, i),
                                    ...field.value.slice(i + 1),
                                  ])
                                }
                                className='w-4 h-4'
                              />
                            </Button>
                          )}
                        </div>
                        <div className='grid grid-cols-3 gap-4'>
                          <div>
                            <div>Peak position (2θ)</div>
                            <Input
                              type='number'
                              value={position}
                              onChange={e =>
                                field.onChange([
                                  ...field.value.slice(0, i),
                                  {
                                    ...field.value,
                                    position: e.target.value,
                                  },
                                  ...field.value.slice(i + 1),
                                ])
                              }
                            />
                          </div>
                          <div>
                            <div>Peak width (±Δθ)</div>
                            <Input
                              type='number'
                              value={width}
                              onChange={e =>
                                field.onChange([
                                  ...field.value.slice(0, i),
                                  {
                                    ...field.value,
                                    width: e.target.value,
                                  },
                                  ...field.value.slice(i + 1),
                                ])
                              }
                            />
                          </div>
                          <div>
                            <div>Peak threshold (%)</div>
                            <Input
                              type='number'
                              value={threshold}
                              onChange={e =>
                                field.onChange([
                                  ...field.value.slice(0, i),
                                  {
                                    ...field.value,
                                    threshold: e.target.value,
                                  },
                                  ...field.value.slice(i + 1),
                                ])
                              }
                            />
                          </div>
                        </div>
                      </div>
                    ))}
                    <Button
                      type='button'
                      variant='ghost'
                      onClick={() =>
                        field.onChange([
                          ...field.value,
                          { position: '', width: '', threshold: '' },
                        ])
                      }
                      className='w-full'
                    >
                      <PlusCircledIcon className='h-4 w-4' />
                    </Button>
                  </div>
                </FormControl>
              </FormItem>
            )}
          />
          <div>
            <div className='font-semibold text-lg'>Unit cell parameters</div>
            <div className='mb-2'>
              <div>Lengths</div>
              <div className='grid md:grid-cols-3 lg:grid-cols-4 gap-4'>
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
              <div className='grid md:grid-cols-3 lg:grid-cols-4 gap-4'>
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
            <div className='font-semibold text-lg'>Topological properties</div>
            <div className='grid md:grid-cols-3 lg:grid-cols-4 gap-x-4 gap-y-2'>
              {Object.entries(TOPO_PROP_UNITS).map(([key, unit], i) => (
                <FormField
                  key={i}
                  control={form.control}
                  name={`scalarTopoProps.${key}`}
                  render={({ field }) => (
                    <FormItem>
                      <FormLabel>{`${key} (${unit})`}</FormLabel>
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
            <div className='font-semibold text-lg'>Building units</div>
            <div className='grid md:grid-cols-5 gap-4'>
              <FormField
                control={form.control}
                name='compositeBUs'
                render={({ field }) => (
                  <FormItem className='md:col-span-3'>
                    <FormLabel>Composite building units</FormLabel>
                    <FormControl>
                      <Combobox
                        itemCls='CBU'
                        items={CBUOptions.map(x => ({ value: x, label: x }))}
                        value={field.value}
                        onCmdItemSelect={value =>
                          field.value.includes(value)
                            ? field.onChange(
                                field.value.filter(x => x !== value)
                              )
                            : field.onChange([...field.value, value])
                        }
                      />
                    </FormControl>
                  </FormItem>
                )}
              />
              <FormField
                control={form.control}
                name='secondaryBU'
                render={({ field }) => (
                  <FormItem className='md:col-span-2'>
                    <FormLabel>Secondary buildling units</FormLabel>
                    <FormControl>
                      <Combobox
                        itemCls='SBU'
                        items={SBUOptions.map(x => ({ value: x, label: x }))}
                        value={field.value}
                        onCmdItemSelect={value =>
                          field.onChange(field.value === value ? '' : value)
                        }
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
