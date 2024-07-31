import { BACKEND_ENDPOINT, getJson } from '.'
import {
  ZeoliteFramework,
  ZeoliteFrameworkBase,
  ZeoliteFrameworkPartial,
} from '@/lib/model/ontozeolite'

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

const GET_ZEOLITE_FRAMEWORKS_ENDPOINT = new URL(
  './ontozeolite/zeolite-frameworks',
  BACKEND_ENDPOINT
)
export function getZeoliteFrameworksMany(searchParams: URLSearchParams) {
  return getJson<ZeoliteFrameworkBase[]>(
    `${GET_ZEOLITE_FRAMEWORKS_ENDPOINT}?${searchParams}`
  )
}

export function getZeoliteFrameworkOne(iriEncoded: string) {
  return getJson<ZeoliteFramework>(
    `${GET_ZEOLITE_FRAMEWORKS_ENDPOINT}/${iriEncoded}`
  )
}

export function getZeoliteFrameworkCIF(iriEncoded: string) {
  return fetch(`${GET_ZEOLITE_FRAMEWORKS_ENDPOINT}/${iriEncoded}/cif`, {
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
