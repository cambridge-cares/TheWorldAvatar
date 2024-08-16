/**
 * Server actions for adding, deleting, and editing entities.
 */
'use server';

import { FieldValues } from 'react-hook-form';

import { Routes } from 'io/config/routes';
import { getAfterDelimiter } from './client-utils';

export interface HttpResponse {
  success: boolean;
  message: string;
}

/**
 * Retrieves all data of the specified type.
 * 
 * @param {string} agentApi API endpoint.
 * @param {string} entityType Type of entity to retrieve.
 * @param {string} identifier Optional identifier of the parent entity.
 * @param {string} subEntityType Optional type of sub entity to retrieve entities associated with the specific parent entity.
 */
export async function getData(agentApi: string, entityType: string, identifier?: string, subEntityType?: string): Promise<FieldValues[]> {
  // Append identifier to the url if it exist
  let url: string = `${agentApi}/${entityType}`;
  if (identifier) {
    url += `/${identifier}`;
    if (subEntityType) {
      url += `/${subEntityType}`;
    }
  }
  const res = await fetch(url);
  if (!res.ok) {
    throw new Error("Failed to fetch data");
  }
  const responseData = await res.json();
  let parsedResponse: FieldValues[];
  // If response is a single object, store it as an array
  if (!Array.isArray(responseData)) {
    parsedResponse = [responseData];
  } else {
    parsedResponse = responseData;
  }
  return parsedResponse.map((item: FieldValues) => {
    return {
      ...item,
      id: getAfterDelimiter(item.id, "/"),
    }
  });
}