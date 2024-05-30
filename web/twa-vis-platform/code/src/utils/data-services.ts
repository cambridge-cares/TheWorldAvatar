import { useEffect, useState } from 'react';

import { useFetchDataQuery } from 'state/api/fia-api';

/**
 * A utility function for generating the Feature Info Agent endpoint for a specific feature.
 * 
 * @param {string} iri The IRI of the target feature.
 * @param {string} stack The stack endpoint associated with the target feature.
 * @param {string} scenario The current scenario ID (if any).
 */
export function genFIAEndpoint(iri: string, stack: string, scenario: string): string {
  if (scenario) {
    return `${stack}/CReDoAccessAgent/getMetadataPrivate/${scenario}?iri=${encodeURIComponent(iri)}`;
  }
  return `${stack}/feature-info-agent/get?iri=${encodeURIComponent(iri)}`;
}

/**
 * Custom hook for fetching and processing feature information from the Feature Info Agent.
 * If no data is available in the response, it defaults to the inherent feature properties, excluding the 'iri' key.
 * 
 * @param {string} endpoint The target FIA endpoint.
 * @param {object} featureProperties The selected feature's inherent properties, which is used as a fallback.
 * 
 * @returns {{ queriedData: object | null, isFetching: boolean }} - An object containing the queried data and the fetching status.
 */
export const useFeatureInfoAgentService = (endpoint: string, featureProperties: object) => {
  const { data, isFetching } = useFetchDataQuery(endpoint);

  const [queriedData, setQueriedData] = useState(null);

  useEffect(() => {
    if (!isFetching) {
      // If there is any data retrieved, set that first
      if (data && Object.keys(data).length !== 0) {
        setQueriedData(data);
      } else if (featureProperties) {
        // Else default to built-in data that excludes IRI
        const builtInData = {
          meta: {
            Properties: Object.fromEntries(
              Object.entries(featureProperties).filter(([key]) => key !== 'iri')
            ),
          },
        };
        setQueriedData(builtInData);
      }
    }
  }, [isFetching]);

  return { queriedData, isFetching };
};