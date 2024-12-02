import styles from '../form.module.css';

import React, { useEffect, useRef, useState } from 'react';
import { FieldValues, SubmitHandler, UseFormReturn } from 'react-hook-form';

import { Address } from 'types/address';
import { FormTemplate, PROPERTY_GROUP_TYPE, PropertyGroup, PropertyShape, TYPE_KEY, VALUE_KEY } from 'types/form';
import { parseWordsForLabels } from 'utils/client-utils';
import { getFormTemplate, getGeolocation, sendGetRequest } from 'utils/server-actions';
import MaterialIconButton from 'ui/graphic/icon/icon-button';
import LoadingSpinner from 'ui/graphic/loader/spinner';
import GeocodeMapContainer from 'ui/map/geocode/geocode-map-container';
import ErrorComponent from 'ui/text/error/error';
import FormFieldComponent from '../field/form-field';
import { FORM_STATES } from '../form-utils';
import { Paths } from 'io/config/routes';

interface FormGeocoderProps {
  agentApi: string;
  field: PropertyShape;
  form: UseFormReturn;
}

/**
 * This component renders a geocoding section for the form.
 * 
 * @param {string} agentApi The target agent endpoint for any registry related functionalities.
 * @param {PropertyShape} field The SHACL restrictions for geolocation.
 * @param {UseFormReturn} form A react-hook-form hook containing methods and state for managing the associated form.
 */
export default function FormGeocoder(props: Readonly<FormGeocoderProps>) {
  const formType: string = props.form.getValues(FORM_STATES.FORM_TYPE);

  const postalCode: string = "postal code";
  const postalCodeUnderscored: string = "postal_code";
  const latitudeShape: PropertyShape = {
    "@id": "_:latitude",
    "@type": "http://www.w3.org/ns/shacl#PropertyShape",
    name: {
      "@value": FORM_STATES.LATITUDE,
    },
    description: {
      "@value": `The latitude of the ${props.field.name[VALUE_KEY]}`,
    },
    datatype: "decimal",
    fieldId: FORM_STATES.LATITUDE,
    order: 10,
    minCount: {
      "@value": "1",
      "@type": "http://www.w3.org/2001/XMLSchema#integer",
    },
    maxCount: {
      "@value": "1",
      "@type": "http://www.w3.org/2001/XMLSchema#integer",
    },
  };

  const longitudeShape: PropertyShape = {
    "@id": "_:longitude",
    "@type": "http://www.w3.org/ns/shacl#PropertyShape",
    name: {
      "@value": FORM_STATES.LONGITUDE,
    },
    description: {
      "@value": `The longitude of the ${props.field.name[VALUE_KEY]}`
    },
    order: 11,
    fieldId: FORM_STATES.LONGITUDE,
    datatype: "decimal",
    minCount: {
      "@value": "1",
      "@type": "http://www.w3.org/2001/XMLSchema#integer",
    },
    maxCount: {
      "@value": "1",
      "@type": "http://www.w3.org/2001/XMLSchema#integer",
    },
  };

  const isInitialFetching: React.MutableRefObject<boolean> = useRef<boolean>(true);
  const [isEmptyAddress, setIsEmptyAddress] = useState<boolean>(false);
  const [hasGeolocation, setHasGeolocation] = useState<boolean>(false);
  const [addressShapes, setAddressShapes] = useState<PropertyShape[]>([]);
  const [postalCodeShape, setPostalCodeShape] = useState<PropertyShape>(null);
  const [addresses, setAddresses] = useState<Address[]>([]);
  const [selectedAddress, setSelectedAddress] = useState<Address>(null);

  useEffect(() => {
    // Declare an async function to get all address related shapes
    const getAddressShapes = async (agentApi: string, locationIdentifier: string): Promise<void> => {
      isInitialFetching.current = true;
      // The location resource must mapped to the field name on the backend
      const template: FormTemplate = await getFormTemplate(agentApi, locationIdentifier.replace(/\s+/g, "_"))
      const addressField: PropertyGroup = template.property.find(field => {
        if (field[TYPE_KEY].includes(PROPERTY_GROUP_TYPE)) {
          const fieldset: PropertyGroup = field as PropertyGroup;
          if (fieldset.label[VALUE_KEY] === "address") {
            return true;
          }
        }
        return false;
      }) as PropertyGroup;
      const addressProperties: PropertyShape[] = addressField.property.map(field => {
        return {
          ...field,
          fieldId: field.name[VALUE_KEY],
        };
      });
      // Search for postal code shape
      setPostalCodeShape(addressProperties.find(field => field.fieldId === postalCode));
      // Get all address related shape that isnt the id or postal code
      setAddressShapes(addressProperties.filter(field => field.fieldId != "id" && field.fieldId != postalCode));
      isInitialFetching.current = false;
    }

    // Declare an async function to get geocoordinates associated with the location
    const getGeoCoordinates = async (agentApi: string, location: string): Promise<void> => {
      isInitialFetching.current = true;
      const coordinates: number[] = await getGeolocation(`${agentApi}/location`, { iri: location });
      // Only set coordinates if they are available
      if (coordinates.length === 2) {
        // Geolocation is in longitude(x), latitude(y) format
        setHasGeolocation(true);
        props.form.setValue(FORM_STATES.LATITUDE, coordinates[1].toString())
        props.form.setValue(FORM_STATES.LONGITUDE, coordinates[0].toString())
        props.form.setValue(props.field.fieldId, `POINT(${coordinates[0]}, ${coordinates[1]})`)
      };
      isInitialFetching.current = false;
    }

    if (formType == Paths.REGISTRY_ADD || formType == Paths.REGISTRY_EDIT) {
      getAddressShapes(props.agentApi, props.field.name[VALUE_KEY]);
    }
    if (formType == Paths.REGISTRY || formType == Paths.REGISTRY_EDIT) {
      getGeoCoordinates(props.agentApi, props.field.defaultValue.value);
    }
  }, []);

  /**
   * A submit action to search for the address based on a postal code
   * 
   * @param {FieldValues} data Values inputed into the form fields.
   */
  const onSearchForAddress: SubmitHandler<FieldValues> = async (data: FieldValues) => {
    // Reset states
    setAddresses([]);
    setSelectedAddress(null);
    setIsEmptyAddress(false);
    // Start search
    const searchParams: URLSearchParams = new URLSearchParams();
    searchParams.append(postalCodeUnderscored, data[postalCode]);

    const url: string = `${props.agentApi}/location/addresses?${searchParams.toString()}`;
    const results = await sendGetRequest(url);
    if (results == "There are no address associated with the parameters in the knowledge graph.") {
      setIsEmptyAddress(true);
    } else {
      setAddresses(JSON.parse(results));
    }
  }

  /**
   * A submit action to search for the geocoordinates based on address components.
   * 
   * @param {FieldValues} data Values inputed into the form fields.
   */
  const onGeocoding: SubmitHandler<FieldValues> = async (data: FieldValues) => {
    // Reset location
    setHasGeolocation(false);
    // Searches for geolocation in the following steps
    const searchParamsList: Record<string, string | undefined>[] = [
      // First by postal code
      { [postalCodeUnderscored]: data[postalCode] },
      // If no coordinates are found, search by block (if available) and street name
      { block: data.block, street: data.street },
      // If no coordinates are found, search for any coordinate in the same city and country
      { city: data.city, country: data.country },
    ];

    for (const params of searchParamsList) {
      const coordinates: number[] = await getGeolocation(`${props.agentApi}/location/geocode`, params);
      // Only set coordinates if they are available
      if (coordinates.length === 2) {
        // Geolocation is in longitude(x), latitude(y) format
        setHasGeolocation(true);
        props.form.setValue(FORM_STATES.LATITUDE, coordinates[1].toString())
        props.form.setValue(FORM_STATES.LONGITUDE, coordinates[0].toString())
        props.form.setValue(props.field.fieldId, `POINT(${coordinates[0]}, ${coordinates[1]})`)
        break; // Stop function if found
      };
    }
  }

  // A click action to set the selected address
  const handleAddressClick = (address: Address) => {
    // Set default values based on the clicked address
    addressShapes.map((shape) => {
      let defaultVal: string;
      if (shape.fieldId.includes("block")) {
        defaultVal = address.block;
      } else if (shape.fieldId.includes("city")) {
        defaultVal = address.city;
      } else if (shape.fieldId.includes("country")) {
        defaultVal = address.country;
      } else if (shape.fieldId.includes("street")) {
        defaultVal = address.street;
      }
      props.form.setValue(shape.fieldId, defaultVal);
    });
    setSelectedAddress(address);
  }

  return (
    <fieldset className={styles["form-fieldset"]}>
      <legend className={styles["form-fieldset-label"]}>
        {parseWordsForLabels(props.field.name[VALUE_KEY])}
      </legend>
      {isInitialFetching.current &&
        <div className={styles["loader-container"]}>
          <LoadingSpinner isSmall={true} />
        </div>
      }
      <div className={styles["form-fieldset-contents"]}>
        {postalCodeShape && <FormFieldComponent
          entityType={props.field.name[VALUE_KEY]}
          field={postalCodeShape}
          form={props.form}
        />}
      </div>
      {!isInitialFetching.current && (formType == Paths.REGISTRY_ADD || formType == Paths.REGISTRY_EDIT) &&
        <div className={styles["form-dependent-button-layout"]}>
          <MaterialIconButton
            iconName={"search"}
            className={styles["button"] + " " + styles["button-layout"]}
            iconStyles={[styles["icon"]]}
            text={{
              styles: [styles["button-text"]],
              content: "Find address"
            }}
            onClick={props.form.handleSubmit(onSearchForAddress)}
          />
        </div>}
      {isEmptyAddress && <div style={{ margin: "0.5rem 0.75rem" }}>
        <ErrorComponent
          message="No address found! The postal code may be incorrect. Please enter the address manually."
        />
      </div>}
      {addresses.length > 0 && !selectedAddress && <div className={styles["form-menu"]}>
        {addresses.map((address, index) => (
          <button
            key={address.street + index}
            className={styles["form-menu-item"]}
            onClick={() => handleAddressClick(address)}>
            {String.fromCharCode(62)} {address.block} {parseWordsForLabels(address.street)}, {address.city}
          </button>
        ))}
      </div>
      }
      {addressShapes.length > 0 && (selectedAddress || isEmptyAddress) &&
        <div className={styles["form-fieldset-contents"]}>
          {addressShapes.map((shape, index) => <FormFieldComponent
            key={shape.fieldId + index}
            entityType={props.field.name[VALUE_KEY]}
            agentApi={props.agentApi}
            field={shape}
            form={props.form}
          />
          )}
          <div className={styles["form-dependent-button-layout"]}>
            <MaterialIconButton
              iconName={"edit_location"}
              className={styles["button"] + " " + styles["button-layout"]}
              iconStyles={[styles["icon"]]}
              text={{
                styles: [styles["button-text"]],
                content: "Select location"
              }}
              onClick={props.form.handleSubmit(onGeocoding)}
            />
          </div>
        </div>}
      {hasGeolocation && <div className={styles["form-fieldset-contents"]}>
        <GeocodeMapContainer
          form={props.form}
          fieldId={props.field.fieldId}
        />
        <FormFieldComponent
          entityType={props.field.name[VALUE_KEY]}
          field={latitudeShape}
          form={props.form}
          options={{ disabled: formType == Paths.REGISTRY || formType == Paths.REGISTRY_DELETE }}
        />
        <FormFieldComponent
          entityType={props.field.name[VALUE_KEY]}
          field={longitudeShape}
          form={props.form}
          options={{ disabled: formType == Paths.REGISTRY || formType == Paths.REGISTRY_DELETE }}
        />
      </div>}
    </fieldset >);
}