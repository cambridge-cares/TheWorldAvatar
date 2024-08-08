import { type ClassValue, clsx } from 'clsx'
import { ReadonlyURLSearchParams } from 'next/navigation'
import { twMerge } from 'tailwind-merge'

export function cn(...inputs: ClassValue[]) {
  return twMerge(clsx(inputs))
}

export function isObjectEmtpy(obj: object) {
  for (const i in obj) return false
  return true
}

const URI2PREFIXNAME = {
  'http://www.theworldavatar.com/ontology/ontospecies/OntoSpecies.owl#': 'os',
  'http://www.theworldavatar.com/ontology/ontokin/OntoKin.owl#': 'okin',
  'http://www.theworldavatar.com/ontology/ontocompchem/OntoCompChem.owl#':
    'occ',
  'http://purl.org/gc/': 'gc',
}

export function makePrefixedIRI(iri: string): string {
  for (const [uri, prefixname] of Object.entries(URI2PREFIXNAME)) {
    if (iri.startsWith(uri)) {
      return `${prefixname}:${iri.slice(uri.length)}`
    }
  }
  return iri
}

export function capitalize(text: string) {
  return text.charAt(0).toUpperCase() + text.slice(1)
}

export function extractLowerUpperParams(
  searchParams: ReadonlyURLSearchParams,
  keys: readonly string[],
  keyPrefix: string = ''
) {
  return Object.fromEntries(
    keys.map(key => {
      const vals = searchParams.getAll(`${keyPrefix}${key}`)
      const lower =
        vals.find(val => val.startsWith('gte:'))?.substring('gte:'.length) || ''
      const upper =
        vals.find(val => val.startsWith('lte:'))?.substring('lte:'.length) || ''
      return [key, { lower, upper }]
    })
  )
}
