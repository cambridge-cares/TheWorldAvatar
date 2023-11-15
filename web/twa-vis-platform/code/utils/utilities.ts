/**
 * Returns the server/client state of the current process.
 * 
 * @returns Returns true if running on server.
 */
export function isServer() {
    return ! (typeof window != 'undefined' && window.document);
}