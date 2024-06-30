function throwFetchResponseIfNotOk(res: Response) {
  if (!res.ok) {
    throw new Error(res.statusText)
  }
  return res
}

export function getJson<ResT>(
  url: string | URL,
  init?: RequestInit | undefined
) {
  return fetch(url, {
    method: 'GET',
    ...init,
  })
    .then(throwFetchResponseIfNotOk)
    .then(res => res.json() as ResT)
}

export function postJson<ReqT>(
  url: string | URL,
  json_body: ReqT,
  init?: Omit<RequestInit, 'method' | 'headers' | 'body'>
) {
  return fetch(url, {
    method: 'POST',
    headers: {
      Accept: 'application/json',
      'Content-Type': 'application/json',
    },
    body: JSON.stringify(json_body),
    ...init,
  }).then(throwFetchResponseIfNotOk)
}

export const BACKEND_ENDPOINT = process.env.NEXT_PUBLIC_BACKEND_ENDPOINT || ''
