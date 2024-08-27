import { createApi, fetchBaseQuery } from '@reduxjs/toolkit/query/react';

import { JsonObject } from 'types/json';
import { ScenarioDimensionsData } from 'types/timeseries';

/**
 * Define a service that fetches supporting data for a given feature based on its IRI, stack endpoint, and a specified scenario ID.
 * This data includes, but is not limited to, metadata and timeseries data associated with the feature. 
 * It will require the feature's IRI, stack name, and the scenario ID in this sequence.
 */
export const featureInfoAgentApi = createApi({
  reducerPath: 'api',
  baseQuery: fetchBaseQuery({ baseUrl: '' }),
  endpoints: (builder) => ({
    fetchData: builder.query<JsonObject, string>({
      query: (url) => `${url}`,
    }),
    fetchDimensions: builder.query<ScenarioDimensionsData, string>({
      query: (url) => `${url}`,
    }),
  }),
});

// Export hooks for usage in functional components, which are auto-generated based on the defined endpoints
export const { useFetchDataQuery, useFetchDimensionsQuery } = featureInfoAgentApi;