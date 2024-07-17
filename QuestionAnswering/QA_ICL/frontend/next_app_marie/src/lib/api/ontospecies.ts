import { ReadonlyURLSearchParams } from 'next/navigation'
import { getJson, BACKEND_ENDPOINT, getReq } from '.'
import { ChemicalClass, Species, SpeciesBase, Use } from '../model/ontospecies'

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
export function getSpeciesMany(searchParams: ReadonlyURLSearchParams) {
  return getJson<SpeciesBase[]>(`${GET_SPECIES_ENDPOINT}?${searchParams}`)
}

export function getSpeciesOne(iriEncoded: string) {
  return getJson<Species>(`${GET_SPECIES_ENDPOINT}/${iriEncoded}`)
}

export function getSpeciesXYZ(iriEncoded: string) {
  return getReq(`${GET_SPECIES_ENDPOINT}/${iriEncoded}/xyz`).then(res =>
    res.text()
  )
}
