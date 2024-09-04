export const CONTEXT_KEY = "@context";
export const ID_KEY = "@id";
export const TYPE_KEY = "@type";
export const VALUE_KEY = "@value";
export const PROPERTY_GROUP_TYPE = "PropertyGroup";

interface RegistryFieldValue {
  value: string;
  type: string;
  dataType: string;
  lang: string;
}

export type RegistryFieldValues = Record<string, RegistryFieldValue>;

export type OntologyConcept = {
  type: RegistryFieldValue;
  label: RegistryFieldValue;
  description: RegistryFieldValue;
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
  defaultValue?: RegistryFieldValue;
  group?: JsonLdInstance;
  datatype?: string;
  class?: JsonLdInstance;
  qualifiedValueShape?: JsonLdInstance[];
  nodeKind?: JsonLdInstance;
  in?: JsonLdInstance;
  minCount?: JsonLdLiteral;
  maxCount?: JsonLdLiteral;
  minInclusive?: JsonLdLiteral ;
  maxInclusive?: JsonLdLiteral ;
  minExclusive?: JsonLdLiteral ;
  maxExclusive?: JsonLdLiteral ;
  minLength?: JsonLdLiteral ;
  maxLength?: JsonLdLiteral ;
  pattern?: JsonLdLiteral 
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
