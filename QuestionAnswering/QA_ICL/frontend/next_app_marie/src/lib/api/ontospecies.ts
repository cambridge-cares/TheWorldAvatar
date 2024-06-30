import { getJson, BACKEND_ENDPOINT } from '.'
import { ChemicalClass, SpeciesBase, Use } from '../model/ontospecies'

const GET_CHEMICAL_CLASSES_ENDPOINT = new URL(
  './ontospecies/chemical-classes',
  BACKEND_ENDPOINT
)
export function getChemicalClasses() {
  return getJson<ChemicalClass[]>(GET_CHEMICAL_CLASSES_ENDPOINT, {
    next: { revalidate: 24 * 60 * 60 }, // revalidate every 24 hours
  })
}

const GET_USES_ENDPOINT = new URL('./ontospecies/uses', BACKEND_ENDPOINT)
export function getUses() {
  return getJson<Use[]>(GET_USES_ENDPOINT, {
    next: { revalidate: 24 * 60 * 60 }, // revalidate every 24 hours
  })
}

const GET_SPECIES_ENDPOINT = new URL('./ontospecies/species', BACKEND_ENDPOINT)
export function getSpecies(searchParams: {
  [key: string]: string | string[] | undefined
}) {
  const queryParams = Object.entries(searchParams)
    .flatMap(([key, val]) =>
      typeof val === 'string'
        ? [[key, val]]
        : typeof val === 'undefined'
          ? []
          : val.map(x => [key, x])
    )
    .map(([k, v]) => `${k}=${v}`)

  if (queryParams.length === 0) {
    return null
  }

  return getJson<SpeciesBase[]>(
    `${GET_SPECIES_ENDPOINT}?${queryParams.join('&')}`
  )
}
