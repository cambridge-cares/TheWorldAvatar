export class BadRequestError extends Error {}
export class NotFoundError extends Error {}
export class InternalServerError extends Error {}

async function rejectNotOkRes(res: Response) {
  if (res.ok) return res

  let errorMsg: string = ''
  try {
    errorMsg = JSON.stringify(await res.json())
  } catch {}

  let err: Error
  switch (res.status) {
    case 400:
      err = new BadRequestError(errorMsg)
      break
    case 404:
      err = new NotFoundError(errorMsg)
      break
    case 500:
      err = new InternalServerError(errorMsg)
      break
    default:
      err = new Error(`Status code: ${res.status}; Error: ${errorMsg}`)
  }

  return Promise.reject(err)
}

export function getJson<ResT>(
  url: string | URL,
  init?: RequestInit | undefined
) {
  return fetch(url, {
    method: 'GET',
    ...init,
  })
    .then(rejectNotOkRes)
    .then(res => res.json() as ResT)
}

export function postJson<ReqT>(
  url: string | URL,
  jsonBody: ReqT,
  init?: Omit<RequestInit, 'method' | 'headers' | 'body'>
) {
  return fetch(url, {
    method: 'POST',
    headers: {
      Accept: 'application/json',
      'Content-Type': 'application/json',
    },
    body: JSON.stringify(jsonBody),
    ...init,
  }).then(rejectNotOkRes)
}

export const BACKEND_ENDPOINT = process.env.NEXT_PUBLIC_BACKEND_ENDPOINT || ''
