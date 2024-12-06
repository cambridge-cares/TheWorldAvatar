import { createSlice, PayloadAction } from '@reduxjs/toolkit';
import { ReduxState } from 'app/store';
import { ScenarioDefinition } from '../types/scenario';

export interface MapFeaturePayload {
    // eslint-disable-next-line @typescript-eslint/no-explicit-any
    [name: string]: any;
}

export const mapFeatureSlice = createSlice({
    name: 'mapFeature',
    initialState: {
        properties: null,
        iri: null,
        stack: null,
        scenarioDefinitions: [],
        scenarioID: null,
        scenarioName: null,
        scenarioType: null,
        features: [] as MapFeaturePayload[],
        filterLayerIds: [],
        filterFeatureIris: [],
        filterTimes: [],
    },
    reducers: {
        setProperties: (state, action) => {
            state.properties = action.payload;
        },
        setIri: (state, action) => {
            state.iri = action.payload;
        },
        setStack: (state, action) => {
            state.stack = action.payload;
        },
        setScenarioDefinitions: (state, action: PayloadAction<ScenarioDefinition[]>) => {
            state.scenarioDefinitions = action.payload;
        },
        setScenarioID: (state, action) => {
            state.scenarioID = action.payload;
        },
        setScenarioName: (state, action) => {
            state.scenarioName = action.payload
        },
        setScenarioType: (state, action) => {
            state.scenarioType = action.payload
        },
        addFeatures: (state, action: PayloadAction<MapFeaturePayload[]>) => {
            state.features = state.features.concat(action.payload);
        },
        clearFeatures: (state) => {
            state.features = [];
        },
        setFilterLayerIds: (state, action) => {
            state.filterLayerIds = action.payload
        },
        setFilterFeatureIris: (state, action) => {
            state.filterFeatureIris = action.payload
        },
        setFilterTimes: (state, action) => {
            state.filterTimes = action.payload
        },
    },
});

// Export selectors 
export const getProperties = (state: ReduxState) => state.mapFeature.properties;
export const getIri = (state: ReduxState) => state.mapFeature.iri;
export const getStack = (state: ReduxState) => state.mapFeature.stack;
export const getScenarioDefinitions = (state: ReduxState) => state.mapFeature.scenarioDefinitions;
export const getScenarioID = (state: ReduxState) => state.mapFeature.scenarioID;
export const getScenarioType = (state: ReduxState) => state.mapFeature.scenarioType;
export const getScenarioName = (state: ReduxState) => state.mapFeature.scenarioName;
export const getFeatures = (state: ReduxState) => state.mapFeature.features;
export const getFilterLayerIds = (state: ReduxState) => state.mapFeature.filterLayerIds;
export const getFilterFeatureIris = (state: ReduxState) => state.mapFeature.filterFeatureIris;
export const getFilterTimes = (state: ReduxState) => state.mapFeature.filterTimes;

// Export the actions
export const {
    setProperties,
    setIri,
    setStack,
    setScenarioID,
    setScenarioName,
    setScenarioType,
    setScenarioDefinitions,
    addFeatures,
    clearFeatures,
    setFilterLayerIds,
    setFilterFeatureIris,
    setFilterTimes,
} = mapFeatureSlice.actions;

// Export the reducer
export default mapFeatureSlice.reducer;