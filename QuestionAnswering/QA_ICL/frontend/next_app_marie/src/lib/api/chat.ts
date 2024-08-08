import { BACKEND_ENDPOINT, postJson } from '.'
import { ChatRequest } from '../model/chat'

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
