import { LayerSource } from './layer-source';
import { Interactions } from 'io/config/interactions';
import { InjectableMapProperties, InjectableProperty, MapboxClickableProperty, MapboxHoverProperty } from 'types/map-properties';
import { JsonArray, JsonObject } from 'types/json';

/**
 * Represents a single visual layer of data. Will have concrete classes for each 
 * map provider type (i.e. MapboxDataLayer, CesiumDataLayer).
 */
export abstract class DataLayer {
    /**
     * Unique ID of layer.
     */
    public readonly id: string;

    /**
     * Public facing name of layer.
     */
    public readonly name: string;

    /**
     * Source of the layer's data.
     */
    public readonly source: LayerSource;

    /**
     * The JSON object that originally defined this layer (unadjusted).
     */
    public readonly definition: JsonObject;

    /**
     * Zero-based display order.
     */
    public order: number = 0;

    /**
     * Type of interactions that are allowed ("all"|"hover-only"|"click-only"|"none")
     */
    public interactions: string = "all";

    // Stores all the injectable map properties as a mapping for ease of access
    public injectableProperties?: InjectableMapProperties = {};

    // Field for indicating their groupings
    public grouping?: string;

    /**
     * A cached visibility state that persists across map terrain
     * changes. Should be updated whenever visibility is changed
     * via the mapping API
     */
    public cachedVisibility: boolean = true;

    /*
    * Indicates if the parent is visible
    */
    public isGroupExpanded: boolean;

    /**
     * Initialise a new DataLayer instance.
     * 
     * @param id Unique ID of layer.
     * @param isGroupExpanded Indicates if the layer's group is expanded.
     * @param source Source of the layer's data.
     * @param definition The JSON object that originally defined this layer.
     */
    constructor(id: string, isGroupExpanded: boolean, source: LayerSource, definition: object) {
        this.id = id;
        this.source = source;
        this.isGroupExpanded = isGroupExpanded;
        this.definition = definition as JsonObject;
        this.name = this.definition["name"] as string;

        if (this.definition["order"]) {
            this.order = this.definition["order"] as number;
        }

        if (this.definition["grouping"]) {
            this.grouping = this.definition["grouping"] as string;
        }
        // Inject clickable state if indicated
        const clickableState: boolean = (this.definition[Interactions.CLICKABLE] ?? true) as boolean;
        const clickableProperty: MapboxClickableProperty = { style: [clickableState] };
        this.updateInjectableProperty(Interactions.CLICKABLE, clickableProperty)

        // Inject hover state if indicated
        if (this.definition[Interactions.HOVER]) {
            const hoverJsonArray: JsonArray = this.definition[Interactions.HOVER] as JsonArray;
            if (hoverJsonArray.length !== 2) {
                throw new Error(`Invalid hover property detected for layer: ${this.id}. The hover property should have two values.`);
            }
            const hoverProperty: MapboxHoverProperty = { style: ["case", ["==", ["get", "iri"], "[HOVERED-IRI]"], Number(hoverJsonArray[0]), Number(hoverJsonArray[1])] };
            this.updateInjectableProperty(Interactions.HOVER, hoverProperty)
        }
    }

    /**
     * Retrieves the injectable property associated with the specific interaction type.
     * 
     * @param {string} interactionType The type of interaction ("hover")
     */
    public getInjectableProperty(interactionType: string): InjectableProperty {
        return this.injectableProperties[interactionType];
    }

    /**
    * Check if the specific interaction type exists in this layer.
    * 
    * @param {string} interactionType The type of interaction ("hover")
    */
    public hasInjectableProperty(interactionType: string): boolean {
        return interactionType in this.injectableProperties;
    }

    /**
     * Update an injectable property for a specific interaction type and map type.
     * 
     * @param {string} interactionType The type of interaction ("hover")
     * @param {InjectableProperty} property An injectable property to add
     */
    public updateInjectableProperty(interactionType: string, property: InjectableProperty): void {
        this.injectableProperties[interactionType] = property;
    }
}