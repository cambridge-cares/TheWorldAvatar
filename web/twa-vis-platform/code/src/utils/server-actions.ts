/**
 * Server actions for adding, deleting, and editing entities.
 */
'use server';

import { FieldValues } from 'react-hook-form';

import { getAfterDelimiter } from './client-utils';
import { FormTemplate, OntologyConcept } from 'types/form';

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
  return parsedResponse;
}

/**
 * Retrieves all available ontology types for the associated entity type.
 * 
 * @param {string} agentApi API endpoint.
 * @param {string} entityType Type of the entity.
 */
export async function getAvailableTypes(agentApi: string, entityType: string): Promise<OntologyConcept[]> {
  const res = await fetch(`${agentApi}/type/${entityType}`);
  if (!res.ok) {
    throw new Error("Failed to fetch data");
  }
  return await res.json();
}

/**
 * Retrieves the form template for the associated entity type.
 * 
 * @param {string} agentApi API endpoint.
 * @param {string} entityType Type of the entity.
 * @param {string} identifier Optional identifier of the parent entity.
 */
export async function getFormTemplate(agentApi: string, entityType: string, identifier?: string): Promise<FormTemplate> {
  let url: string = `${agentApi}/form/${entityType}`;
  if (identifier) {
    url += `/${identifier}`;
  }
  const res = await fetch(url);
  if (!res.ok) {
    throw new Error("Failed to fetch data");
  }
  return await res.json();
}

/**
 * Sends a GET request to the specified agent to execute its task. 
 * Note that this method is only concerned with a successful GET request and will not return any parameter.
 * 
 * @param {string} agentApi API endpoint.
 */
export async function sendGetRequest(agentApi: string): Promise<void> {
  const res = await fetch(agentApi);
  if (!res.ok) {
    throw new Error("Failed to fetch data");
  }
}


/**
 * Add the entity to the knowledge graph.
 * 
 * @param {string} agentApi API endpoint.
 * @param {FieldValues} form Form storing the input data.
 * @param {string} entityType Target entity type.
 */
export async function addEntity(agentApi: string, form: FieldValues, entityType: string): Promise<HttpResponse> {
  try {
    const response = await fetch(`${agentApi}/${entityType}`, {
      method: "POST",
      headers: {
        "Content-Type": "application/json",
      },
      body: JSON.stringify({
        ...form,
        entity: entityType,
      })
    });
    const responseBody: string = await response.text();
    return { success: response.ok, message: responseBody };
  } catch (error) {
    console.error("Error occurred while adding entity.", error);
  }
}

/**
 * Update the entity information within the knowledge graph.
 * 
 * @param {string} agentApi API endpoint.
 * @param {FieldValues} form Form storing the input data.
 * @param {string} entityType Target entity type.
 */
export async function updateEntity(agentApi: string, form: FieldValues, entityType: string): Promise<HttpResponse> {
  try {
    const response = await fetch(`${agentApi}/${entityType}/${form.id}`, {
      method: "PUT",
      headers: {
        "Content-Type": "application/json",
      },
      body: JSON.stringify({
        ...form,
        entity: entityType,
      })
    });
    const responseBody: string = await response.text();
    return { success: response.ok, message: responseBody };
  } catch (error) {
    console.error("Error occurred while updating entity.", error);
  }
}

/**
 * Delete the entity associated with the id.
 * 
 * @param {string} agentApi API endpoint.
 * @param {string} id Target entity id.
 * @param {string} entityType Target entity type.
 */
export async function deleteEntity(agentApi: string, id: string, entityType: string): Promise<HttpResponse> {
  try {
    const response = await fetch(`${agentApi}/${entityType}/${id}`, {
      method: "DELETE",
      headers: {
        "Content-Type": "application/json",
      },
    });
    const responseBody: string = await response.text();
    return { success: response.ok, message: responseBody };
  } catch (error) {
    console.error("Error occurred when deleting entity.", error);
  }
}