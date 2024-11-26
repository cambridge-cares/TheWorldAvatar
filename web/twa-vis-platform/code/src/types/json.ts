// For dealing with JSON objects of an unknown structure.
export type JsonObject = {
    [key: string]: string | number | boolean | JsonArray | JsonObject
}

// For dealing with array of JSON objects with unknown structure.
export type JsonArray = JsonObject[];

// For API response from the vis backend agent
export interface ApiResponse {
    iri: string;
    message: string;
}
