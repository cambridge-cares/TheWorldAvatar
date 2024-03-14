/**
 * Utilties to be run on the server.
 */
import { createApi, fetchBaseQuery } from '@reduxjs/toolkit/query/react';

/**
 * Returns the server/client state of the current process.
 * 
 * @returns Returns true if running on server.
 */
export function isServer() {
    return !(typeof window != 'undefined' && window.document);
};

/**
 * Define a service that fetches supporting data for a given feature based on its IRI, stack endpoint, and a specified scenario ID.
 * This data includes, but is not limited to, metadata and timeseries data associated with the feature. 
 * It will require the feature's IRI, stack name, and the scenario ID in this sequence.
 */
export const featureInfoAgentApi = createApi({
    reducerPath: 'api',
    baseQuery: fetchBaseQuery({ baseUrl: '' }),
    endpoints: (builder) => ({
        getMetadata: builder.query<any, string>({
            query: (url) => `${url}`,
        }),
    }),
});

// Export hooks for usage in functional components, which are auto-generated based on the defined endpoints
export const { useGetMetadataQuery } = featureInfoAgentApi;