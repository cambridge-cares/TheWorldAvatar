# Knowledge Graph-backed Form UI

In meeting the demand for a generalisable Knowledge Graph approach to form UI generation, The World Avatar (TWA) Visualisation Platform have implemented such an approach based on [SHACL](https://www.w3.org/TR/shacl/). [SHACL](https://www.w3.org/TR/shacl/) is generally a language for validating RDF graphs against a set of conditions. As a description of a valid data graph, they can also be implemented to generate forms in a generalisable knowledge graph enabled approach. This section will describe the form template required from the backend so that The World Avatar (TWA) Visualisation Platform can parse it into the corresponding form UI.

The form template must be in the `JSON-LD` format, with a sample format as follows. Do note that the overall form template structure does not necessarily follow a real ontology, but is intended to structure the template. The template must contain a list of either a property group or property themselves. Properties in a property group are grouped together as fieldsets in the form, whereas property are listed separately.

```json
{
  "@context": {
    "prefix": "namespace",
    "PropertyGroup": "http://www.w3.org/ns/shacl#PropertyGroup",
    "PropertyShape": "http://www.w3.org/ns/shacl#PropertyShape",
    "class": "http://www.w3.org/ns/shacl#class",
    "comment": "http://www.w3.org/2000/01/rdf-schema#comment",
    "datatype": "http://www.w3.org/ns/shacl#datatype",
    "defaultValue": "http://www.w3.org/ns/shacl#defaultValue",
    "description": "http://www.w3.org/ns/shacl#description",
    "group": "http://www.w3.org/ns/shacl#group",
    "in": "http://www.w3.org/ns/shacl#in",
    "label": "http://www.w3.org/2000/01/rdf-schema#label",
    "name": "http://www.w3.org/ns/shacl#name",
    "order": "http://www.w3.org/ns/shacl#order",
    "property": "http://www.w3.org/ns/shacl#property",
    "qualifiedValueShape": "http://www.w3.org/ns/shacl#qualifiedValueShape"
  },
  "property": [
    {
      "@id": "iri",
      "@type": "http://www.w3.org/ns/shacl#PropertyShape",
      "name": { "@value": "field name" },
      "description": { "@value": "The field's description." }
    },
    {
      "@id": "groupiri",
      "@type": "http://www.w3.org/ns/shacl#PropertyGroup",
      "label": { "@value": "The fieldset's name" },
      "comment": { "@value": "The fieldset's description" },
      "property": [
        {
          "@id": "group field 1 iri",
          "@type": "http://www.w3.org/ns/shacl#PropertyShape",
          "name": { "@value": "group subfield 1 name" },
          "description": { "@value": "The field's description." }
        },
        {
          "@id": "group field 2 iri",
          "@type": "http://www.w3.org/ns/shacl#PropertyShape",
          "name": { "@value": "group subfield 2 name" },
          "description": { "@value": "The field's description." }
        }
      ]
    },
    {
      "@id": "iri",
      "@type": "http://www.w3.org/ns/shacl#PropertyShape",
      "name": { "@value": "field name" },
      "description": { "@value": "The field's description." }
    },
    {
      "@id": "scheduleiri",
      "@type": "http://www.w3.org/ns/shacl#PropertyGroup",
      "label": { "@value": "schedule" },
      "comment": {
        "@value": "A property group that requires a schedule UI. Label must include schedule in the name"
      }
    }
  ]
}
```

## 1. Property Groups

In general, property groups are intended to group related fields into a fieldset in the form UI and requires the following format:

```json
 {
  "@id": "groupiri",
  "@type": "http://www.w3.org/ns/shacl#PropertyGroup",
  "label": { "@value": "The fieldset's name" },
  "comment": { "@value": "The fieldset's description" },
  "property": [
    property shapes...
  ]
 }
```

There is also a special Schedule UI section, providing users the ability to choose a schedule (day of week, interval). This can be invoked with a `Property Group` fieldset that must include `schedule` in its `label` key () `"label": { "@value": "any schedule" }`. Note that any other fields/keys are ignored in this case.

```json
{
  "@id": "scheduleiri",
  "@type": "http://www.w3.org/ns/shacl#PropertyGroup",
  "label": { "@value": "schedule" },
  "comment": {
    "@value": "A property group that requires a schedule UI. Label must include schedule in the name"
  },
  "property": [
    property shapes...
  ]
}
```

## 2. Property Shapes

With that said, the properties `PropertyShape` themselves must comply with [SHACL](https://www.w3.org/TR/shacl/) restrictions. All fields/shapes must contain a `@type`, `name`, and `description` at the minimum. The following sections highlights how the associated `SHACL` property is used in generating the form.

### 2.1. Form section/field representation

1. `datatype`: Generates form input types from the associated value of data type specified in the `xsd` namespace

- **Text**: `string`
- **Number**: `decimal`, `integer`
- **Date**: `date`, `time`, `dateTime`

2. `in`: Generates a select input field with options containing the **subclasses** on the associated class `{"@id": "class"}`
3. `class`: Generates a form section based on a select input field, containing the **instances** of the associated class `{"@id": "class"}`. Additional buttons are available to either add a new instance or view more details about the selected instance. This property can be used as a standalone without any dependency capabilities enabled. In enabling dependencies, please also include the following properties:

- `qualifiedValueShape`: An optional property that must be included with `class` to enable dependencies between two fields. For instance, an employee must always be linked to a specific employer. This property must contain an array of the associated node shape instances in the format `{"@id": "node shape"}`. Note that one property may have as many node shapes as possible using the `sh:and` property in the original `SHACL` format.
- `nodeKind`: An optional property that must be included with `class` and `qualifiedValueShape` to denote if the property is a dependent property, that is dependent on a separate (independent) field with the same node shape. If `nodeKind` is not added, we assume that the property is an independent field that other dependent fields will require.

### 2.2. Form utilities

The following fields are optional utility fields that does not affect the category of form components/fields generated, but supports the overall form functionality.

1. `order`: A `number` that can be set at either the group and property level to arrange the form accordingly
2. `defaultValue`: An optional `string` at each `property` that contains the corresponding value of the specific entity instance

### 2.3. Form validation

For form validation purposes, several SHACL restrictions can also be utilised in the following combinations:

1. Required form field: `minCount: 1`, `maxCount: 1`
2. Number form field >= value: `minInclusive: number`
3. Number form field > value: `minExclusive: number`
4. Number form field <= value: `maxInclusive: number`
5. Number form field < value: `maxExclusive: number`
6. Text form field with a minimum length: `minLength: string`
7. Text form field with a maximum length: `maxLength: string`
8. Text form field that contains only digits: `pattern: "^\d+$"`
9. Text form field conforming to a specified regex pattern: `pattern: string`
