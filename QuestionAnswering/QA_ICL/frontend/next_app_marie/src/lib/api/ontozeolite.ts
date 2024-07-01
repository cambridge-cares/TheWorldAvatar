import { BACKEND_ENDPOINT, getJson } from '.'

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
