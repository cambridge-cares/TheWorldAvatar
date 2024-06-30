import { ChatRequest } from './model/chat'
import { ChemicalClass, Use } from './model/ontospecies'
import { QARequest, QAResponse } from './model/qa'

function throwFetchResponseIfNotOk(res: Response) {
  if (!res.ok) {
    throw new Error(res.statusText)
  }
  return res
}

function getJson<ResT>(url: string | URL, init?: RequestInit | undefined) {
  return fetch(url, {
    method: 'GET', ...init
  }).then(throwFetchResponseIfNotOk).then(res => res.json() as ResT)
}

function postJson<ReqT>(
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

const BACKEND_ENDPOINT = process.env.NEXT_PUBLIC_BACKEND_ENDPOINT || ''


const QA_ENDPOINT = new URL('./qa', BACKEND_ENDPOINT)
export function queryQa(question: string) {
  return postJson<QARequest>(QA_ENDPOINT, { question }, { cache: 'no-store' }).then(
    res => res.json() as Promise<QAResponse>
  )
}


const CHAT_ENDPOINT = new URL('./chat', BACKEND_ENDPOINT)
export function queryChat(
  qa_request_id: string
): [AbortController, Promise<ReadableStreamDefaultReader<string>>] {
  const abortController = new AbortController()
  return [
    abortController,
    postJson<ChatRequest>(
      CHAT_ENDPOINT,
      { qa_request_id },
      { signal: abortController.signal, cache: 'no-store' }
    ).then(res => {
      if (res.body === null) {
        throw new Error('Null response body')
      }
      return res.body.pipeThrough(new TextDecoderStream()).getReader()
    }),
  ]
}

const GET_CHEMICAL_CLASSES_ENDPOINT = new URL("./ontospecies/chemical-classes", BACKEND_ENDPOINT)
export function getChemicalClasses() {
  return getJson<ChemicalClass[]>(GET_CHEMICAL_CLASSES_ENDPOINT, { cache: 'no-store' })
}

const GET_USES_ENDPOINT = new URL("./ontospecies/uses", BACKEND_ENDPOINT)
export function getUses() {
  return getJson<Use[]>(GET_USES_ENDPOINT, { cache: 'no-store' })
}