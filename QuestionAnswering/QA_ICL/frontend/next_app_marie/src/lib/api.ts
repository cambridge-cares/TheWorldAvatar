import { ChatRequest, QARequest, QAResponse } from './model'

function postJson<ReqT>(url: string | URL, json_body: ReqT) {
  return fetch(url, {
    method: 'POST',
    headers: {
      Accept: 'application/json',
      'Content-Type': 'application/json',
    },
    body: JSON.stringify(json_body),
  }).then(res => {
    if (!res.ok) {
      throw new Error(res.statusText)
    }
    return res
  })
}

const BACKEND_ENDPOINT = process.env.NEXT_PUBLIC_BACKEND_ENDPOINT || ''
const QA_ENDPOINT = new URL('./qa', BACKEND_ENDPOINT)
const CHAT_ENDPOINT = new URL('./chat', BACKEND_ENDPOINT)

export function queryQa(question: string) {
  return postJson<QARequest>(QA_ENDPOINT, { question }).then(
    res => res.json() as Promise<QAResponse>
  )
}

export function queryChat(qa_request_id: string) {
  return postJson<ChatRequest>(CHAT_ENDPOINT, { qa_request_id }).then(res => {
    if (res.body === null) {
      throw new Error('Null response body')
    }
    return res.body.pipeThrough(new TextDecoderStream()).getReader()
  })
}
