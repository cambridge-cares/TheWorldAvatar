export type InjectableMapProperties = {
  [interaction: string]: InjectableProperty;
};

// An empty base type that must be extended for any injectable property
// eslint-disable-next-line @typescript-eslint/no-explicit-any
export type InjectableProperty = { style: any[] }

/** A mapbox hover property will always be in the form of:
  [
    "case", ["==", ["get", "iri"], "EDITABLE: CURRENT IRI"],
    EDITABLE SELECTED OPACITY,
    EDITABLE UNSELECTED OPACITY
  ]
*/
export interface MapboxHoverProperty extends InjectableProperty {
  style: [
    string,
    [string, [string, string], string],
    number,
    number
  ]
}

export interface MapboxClickableProperty extends InjectableProperty {
  style: [
    boolean
  ]
}