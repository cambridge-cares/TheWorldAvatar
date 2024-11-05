import { BACKEND_ENDPOINT, getJson } from '.'
import {
  Journal,
  ZeoliteFramework,
  ZeoliteFrameworkBase,
  ZeoliteFrameworkPartial,
  ZeoliticMaterial,
  ZeoliticMaterialBase,
} from '@/lib/model/ontozeolite'
import { PtElement, SpeciesBase } from '../model/ontospecies'

const GET_CBUS_ENDPOINT = new URL(
  './ontozeolite/composite-building-units',
  BACKEND_ENDPOINT
)
export function getCBUs() {
  return getJson<string[]>(GET_CBUS_ENDPOINT, {
    next: { revalidate: 24 * 60 * 60 }, // revalidate every 24 hours
  })
}

const GET_SBUS_ENDPOINT = new URL(
  './ontozeolite/secondary-building-units',
  BACKEND_ENDPOINT
)
export function getSBUs() {
  return getJson<string[]>(GET_SBUS_ENDPOINT, {
    next: { revalidate: 24 * 60 * 60 }, // revalidate every 24 hours
  })
}

const GET_FRAMEWORK_COMPONENTS_ENDPOINT = new URL(
  './ontozeolite/framework-components',
  BACKEND_ENDPOINT
)
export function getFrameworkComponents() {
  return getJson<PtElement[]>(GET_FRAMEWORK_COMPONENTS_ENDPOINT, {
    next: { revalidate: 24 * 60 * 60 }, // revalidate every 24 hours
  })
}

const GET_GUEST_COMPONENTS_ENDPOINT = new URL(
  './ontozeolite/guest-components',
  BACKEND_ENDPOINT
)
export function getGuestComponents() {
  return getJson<SpeciesBase[]>(GET_GUEST_COMPONENTS_ENDPOINT, {
    next: { revalidate: 24 * 60 * 60 }, // revalidate every 24 hours
  })
}

const GET_JOURNALS_ENDPOINT = new URL(
  './ontozeolite/journals',
  BACKEND_ENDPOINT
)
export function getJournals() {
  return getJson<Journal[]>(GET_JOURNALS_ENDPOINT, {
    next: { revalidate: 24 * 60 * 60 }, // revalidate every 24 hours
  })
}

const GET_ZEOLITE_FRAMEWORKS_ENDPOINT = new URL(
  './ontozeolite/zeolite-frameworks',
  BACKEND_ENDPOINT
)
export function getZeoliteFrameworksMany(searchParams?: URLSearchParams) {
  const url = searchParams
    ? `${GET_ZEOLITE_FRAMEWORKS_ENDPOINT}?${searchParams}`
    : GET_ZEOLITE_FRAMEWORKS_ENDPOINT
  return getJson<ZeoliteFrameworkBase[]>(url)
}

const GET_ZEOLITE_FRAMEWORKS_ONE_ENDPOINT = new URL(
  './ontozeolite/zeolite-frameworks/one',
  BACKEND_ENDPOINT
)
export function getZeoliteFrameworkOne(iriEncoded: string) {
  return getJson<ZeoliteFramework>(
    `${GET_ZEOLITE_FRAMEWORKS_ONE_ENDPOINT}/?iri=${iriEncoded}`
  )
}

const GET_ZEOLITE_FRAMEWORKS_CIF_ENDPOINT = new URL(
  './ontozeolite/zeolite-frameworks/cif',
  BACKEND_ENDPOINT
)
export function getZeoliteFrameworkCIF(iriEncoded: string) {
  return fetch(`${GET_ZEOLITE_FRAMEWORKS_CIF_ENDPOINT}/?iri=${iriEncoded}`, {
    method: 'GET',
  }).then(res => res.text())
}

const GET_ZEOLITE_FRAMEWORKS_PARTIAL_ENDPOINT = new URL(
  './ontozeolite/zeolite-frameworks-partial',
  BACKEND_ENDPOINT
)
export function getZeoliteFrameworksPartialMany(searchParams: URLSearchParams) {
  return getJson<ZeoliteFrameworkPartial[]>(
    `${GET_ZEOLITE_FRAMEWORKS_PARTIAL_ENDPOINT}?${searchParams}`
  )
}

const GET_ZEOLITIC_MATERIALS_ENDPOINT = new URL(
  './ontozeolite/zeolitic-materials',
  BACKEND_ENDPOINT
)
export function getZeoliticMaterialsMany(searchParams: URLSearchParams) {
  return getJson<ZeoliticMaterialBase[]>(
    `${GET_ZEOLITIC_MATERIALS_ENDPOINT}?${searchParams}`
  )
}

const GET_ZEOLITIC_MATERIALS_ONE_ENDPOINT = new URL(
  './ontozeolite/zeolitic-materials/one',
  BACKEND_ENDPOINT
)
export function getZeoliticMaterialOne(iriEncoded: string) {
  return getJson<ZeoliticMaterial>(
    `${GET_ZEOLITIC_MATERIALS_ONE_ENDPOINT}?iri=${iriEncoded}`
  )
}

const GET_ZEOLITIC_MATERIALS_cif_ENDPOINT = new URL(
  './ontozeolite/zeolitic-materials/cif',
  BACKEND_ENDPOINT
)
export function getZeoliticMaterialCIF(iriEncoded: string) {
  return fetch(`${GET_ZEOLITIC_MATERIALS_cif_ENDPOINT}?iri=${iriEncoded}`, {
    method: 'GET',
  }).then(res => res.text())
}
