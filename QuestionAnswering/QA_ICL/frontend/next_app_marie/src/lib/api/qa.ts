import { BACKEND_ENDPOINT, postJson } from '.'
import { QARequest, QAResponse } from '../model/qa'

const QA_ENDPOINT = new URL('./qa', BACKEND_ENDPOINT)

export function queryQa(question: string) {
  return postJson<QARequest>(
    QA_ENDPOINT,
    { question },
    { cache: 'no-store' }
  ).then(res => res.json() as Promise<QAResponse>)
}
