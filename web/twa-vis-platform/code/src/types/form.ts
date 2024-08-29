export const CONTEXT_KEY = "@context";
export const ID_KEY = "@id";
export const TYPE_KEY = "@type";
export const VALUE_KEY = "@value";
export const PROPERTY_GROUP_TYPE = "PropertyGroup";

export type OntologyConcept = {
  type: string;
  label: string;
};

export type FormTemplate = {
  "@context": Record<string, string>;
  property: PropertyShapeOrGroup[];
};

export type PropertyShapeOrGroup = PropertyShape | PropertyGroup;

export interface PropertyShape {
  "@id": string;
  "@type": string;
  name: JsonLdLiteral;
  description: JsonLdLiteral;
  order: number;
  fieldId?: string; // Not present but appended after
  defaultValue?: string;
  group?: JsonLdInstance;
  datatype?: string;
  class?: JsonLdInstance;
  qualifiedValueShape?: JsonLdInstance[];
  nodeKind?: JsonLdInstance;
  in?: JsonLdInstance;
  minCount?: JsonLdLiteral;
  maxCount?: JsonLdLiteral;
}

export interface PropertyGroup {
  "@id": string;
  "@type": string;
  label: JsonLdLiteral;
  comment: JsonLdLiteral;
  order: number;
  property: PropertyShape[];
}

interface JsonLdInstance {
  "@id": string;
}

interface JsonLdLiteral {
  "@value": string;
  "@type"?: string;
}
