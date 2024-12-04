/**
 * Server actions for adding, deleting, and editing entities.
 */
'use server';

import { FieldValues } from 'react-hook-form';

import { RegistryFieldValues, FormTemplate, OntologyConcept } from 'types/form';

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
export async function getData(agentApi: string, entityType: string, identifier?: string, subEntityType?: string): Promise<RegistryFieldValues[]> {
  // Append identifier to the url if it exist
  let url: string = `${agentApi}/${entityType}`;
  if (identifier) {
    url += `/${identifier}`;
    if (subEntityType) {
      url += `/${subEntityType}`;
    }
  }
  const res = await sendRequest(url, "GET");
  const responseData = await res.json();
  let parsedResponse: RegistryFieldValues[];
  // If response is a single object, store it as an array
  if (!Array.isArray(responseData)) {
    parsedResponse = [responseData];
  } else {
    parsedResponse = responseData;
  }
  return parsedResponse;
}

/**
 * Retrieves the geolocation from the API endpoint based on the query parameters.
 * 
 * @param {string} agentApi API endpoint.
 * @param {Record<string, string | undefined>} params query parameters.
 */
export async function getGeolocation(agentApi: string, params: Record<string, string | undefined>): Promise<number[]> {
  const searchParams = new URLSearchParams();
  Object.entries(params).forEach(([key, value]) => {
    // Only append the search param if there is a value
    if (value) searchParams.append(key, value);
  });
  const url: string = `${agentApi}/location/geocode?${searchParams.toString()}`;
  const results = await sendGetRequest(url);
  if (results == "There are no coordinates associated with the parameters in the knowledge graph.") {
    return [];
  }
  return JSON.parse(results);
}

/**
 * Retrieves all data of the specified type with human-readable labels for the fields.
 * 
 * @param {string} agentApi API endpoint.
 * @param {string} entityType Type of entity to retrieve.
 */
export async function getLabelledData(agentApi: string, entityType: string): Promise<RegistryFieldValues[]> {
  const res = await sendRequest(`${agentApi}/${entityType}/label`, "GET");
  const responseData = await res.json();
  return responseData;
}

/**
 * Retrieves all available ontology types for the associated entity type.
 * 
 * @param {string} agentApi API endpoint.
 * @param {string} entityType Type of the entity.
 */
export async function getAvailableTypes(agentApi: string, entityType: string): Promise<OntologyConcept[]> {
  const res = await sendRequest(`${agentApi}/type/${entityType}`, "GET");
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
  const res = await sendRequest(url, "GET");
  return await res.json();
}

/**
 * Sends a GET request to the specified agent to execute its task, and return its text if required.
 * 
 * @param {string} agentApi API endpoint.
 */
export async function sendGetRequest(agentApi: string): Promise<string> {
  const res = await sendRequest(agentApi, "GET");
  return res.text();
}

/**
 * Sends a POST request with parameters to the specified agent to execute its task, and return its text if required.
 * 
 * @param {string} agentApi API endpoint.
 * @param {string} jsonBody Parameters in JSONIFIED string.
 */
export async function sendPostRequest(agentApi: string, jsonBody: string): Promise<HttpResponse> {
  const response = await sendRequest(agentApi, "POST", "application/json", jsonBody);
  const responseBody: string = await response.text();
  return { success: response.ok, message: responseBody };
}

/**
 * Retrieves the form template for the associated entity type.
 * 
 * @param {string} agentApi API endpoint.
 * @param {string} entityType Type of the entity.
 * @param {FieldValues} form Form storing the input data.
 */
export async function getMatchingInstances(agentApi: string, entityType: string, form: FieldValues): Promise<HttpResponse> {
  const url: string = `${agentApi}/${entityType}/search`;
  const reqBody: string = JSON.stringify(form);
  const response = await sendRequest(url, "POST", "application/json", reqBody);
  const responseBody: string = await response.text();
  return { success: response.ok, message: responseBody };
}

/**
 * Add the entity to the knowledge graph.
 * 
 * @param {string} agentApi API endpoint.
 * @param {FieldValues} form Form storing the input data.
 * @param {string} entityType Target entity type.
 */
export async function addEntity(agentApi: string, form: FieldValues, entityType: string): Promise<HttpResponse> {
  const reqBody: string = JSON.stringify({
    ...form,
    entity: entityType,
  });
  const response = await sendRequest(`${agentApi}/${entityType}`, "POST", "application/json", reqBody);
  const responseBody: string = await response.text();
  return { success: response.ok, message: responseBody };
}

/**
 * Update the entity information within the knowledge graph.
 * 
 * @param {string} agentApi API endpoint.
 * @param {FieldValues} form Form storing the input data.
 * @param {string} entityType Target entity type.
 */
export async function updateEntity(agentApi: string, form: FieldValues, entityType: string): Promise<HttpResponse> {
  const reqBody: string = JSON.stringify({
    ...form,
    entity: entityType,
  });
  const response = await sendRequest(`${agentApi}/${entityType}/${form.id}`, "PUT", "application/json", reqBody);
  const responseBody: string = await response.text();
  return { success: response.ok, message: responseBody };
}

/**
 * Delete the entity associated with the id.
 * 
 * @param {string} agentApi API endpoint.
 * @param {string} id Target entity id.
 * @param {string} entityType Target entity type.
 */
export async function deleteEntity(agentApi: string, id: string, entityType: string): Promise<HttpResponse> {
  const response = await sendRequest(`${agentApi}/${entityType}/${id}`, "DELETE", "application/json");
  const responseBody: string = await response.text();
  return { success: response.ok, message: responseBody };
}

/**
 * This function is a reusable method to send a request to the specified endpoint with any optional values.
 * 
 * @param {string} endpoint The target endpoint for sending the request
 * @param {string} methodType Type of request method - DELETE, GET, PUT, POST
 * @param {string} contentType Type of request content - application/json - Optional for GET request
 * @param {string} jsonBody Optional body parameter to be passed in request
 */
async function sendRequest(endpoint: string, methodType: string, contentType?: string, jsonBody?: string): Promise<Response> {
  const options: RequestInit = {
    method: methodType,
    headers: {
      "Content-Type": contentType,
    },
  };

  if (jsonBody) {
    options.body = jsonBody;
  }

  const response = methodType === "GET" ? await fetch(endpoint) : await fetch(endpoint, options);
  if (!response.ok) {
    console.error("Failed to complete request: ", response);
  }
  return response;
}