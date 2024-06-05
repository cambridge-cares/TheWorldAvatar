import { type ClassValue, clsx } from "clsx"
import { twMerge } from "tailwind-merge"

export function cn(...inputs: ClassValue[]) {
  return twMerge(clsx(inputs))
}



const URI2PREFIXNAME = {
  "http://www.theworldavatar.com/ontology/ontospecies/OntoSpecies.owl#": "os",
  "http://www.theworldavatar.com/ontology/ontokin/OntoKin.owl#": "okin",
  "http://www.theworldavatar.com/ontology/ontocompchem/OntoCompChem.owl#": "occ",
  "http://purl.org/gc/": "gc"
}

export function makePrefixedIRI(iri: string): string {
  for (const [uri, prefixname] of Object.entries(URI2PREFIXNAME)) {
    if (iri.startsWith(uri)) {
      return `${prefixname}:${iri.slice(uri.length)}`;
    }
  }
  return iri;
}
