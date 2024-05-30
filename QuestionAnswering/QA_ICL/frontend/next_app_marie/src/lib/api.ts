import { DataItem } from "./model"

export interface QARequest {
  question: string
}

interface QAResponse {
  metdata: object,
  data: DataItem[]
}

function fetchJson<ReqT, ResT>(url: string | URL, method: string, json_body: ReqT): Promise<ResT> {
  return fetch(url, {
    method,
    headers: {
      "Accept": "application/json",
      "Content-Type": "application/json"
    },
    body: JSON.stringify(json_body)
  }).then(res => {
    if (!res.ok) {
      throw new Error(res.statusText)
    }
    return res.json() as Promise<ResT>
  })
}

const BACKEND_ENDPOINT = process.env.NEXT_PUBLIC_BACKEND_ENDPOINT || ""
const QA_ENDPOINT = new URL("./qa", BACKEND_ENDPOINT)

export function queryQa(question: string): Promise<QAResponse> {
  return fetchJson<QARequest, QAResponse>(QA_ENDPOINT, "POST", { question })
}