import { FORM_STATES } from "ui/interaction/form/form-utils";

export const SEARCH_FORM_TYPE = "search";
export const CONTEXT_KEY = "@context";
export const ID_KEY = "@id";
export const TYPE_KEY = "@type";
export const VALUE_KEY = "@value";
export const PROPERTY_GROUP_TYPE = "PropertyGroup";
export const ONTOLOGY_CONCEPT_ROOT = "root";

interface RegistryFieldValue {
  value: string;
  type: string;
  dataType: string;
  lang: string;
}

export type RegistryFieldValues = Record<string, RegistryFieldValue>;

export type OntologyConceptMappings = Record<string, OntologyConcept[]>;

export type OntologyConcept = {
  type: RegistryFieldValue;
  label: RegistryFieldValue;
  description: RegistryFieldValue;
  parent?: RegistryFieldValue;
};

export interface FormOptionType {
  label: string;
  value: string;
}

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
  minInclusive?: JsonLdLiteral;
  maxInclusive?: JsonLdLiteral;
  minExclusive?: JsonLdLiteral;
  maxExclusive?: JsonLdLiteral;
  minLength?: JsonLdLiteral;
  maxLength?: JsonLdLiteral;
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

export const defaultSearchOption: OntologyConcept = {
  type: {
    value: "",
    type: "literal",
    dataType: "http://www.w3.org/2001/XMLSchema#string",
    lang: "",
  },
  label: {
    value: "Select All",
    type: "literal",
    dataType: "http://www.w3.org/2001/XMLSchema#string",
    lang: "",
  },
  description: {
    value: "This option allows you to select all available criteria at once.",
    type: "literal",
    dataType: "http://www.w3.org/2001/XMLSchema#string",
    lang: "",
  }
}

export const remarksShape: PropertyShape = {
  "@id": "string",
  "@type": "http://www.w3.org/ns/shacl#PropertyShape",
  name: { "@value": FORM_STATES.REMARKS },
  fieldId: FORM_STATES.REMARKS,
  datatype: "string",
  description: { "@value": "Reason for this action" },
  order: 1,
};