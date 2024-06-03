import { createSlice, PayloadAction } from '@reduxjs/toolkit';
import { ReduxState } from 'app/store';

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
        scenario: null,
        queryTrigger: false,
        features: [] as MapFeaturePayload[],
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
        setScenario: (state, action) => {
            state.scenario = action.payload;
        },
        setQueryTrigger: (state, action) => {
            state.queryTrigger = action.payload;
        },
        addFeatures: (state, action: PayloadAction<MapFeaturePayload[]>) => {
            state.features = state.features.concat(action.payload);
        },
        clearFeatures: (state) => {
            state.features = [];
        },
    },
});

// Export selectors 
export const getProperties = (state: ReduxState) => state.mapFeature.properties;
export const getIri = (state: ReduxState) => state.mapFeature.iri;
export const getStack = (state: ReduxState) => state.mapFeature.stack;
export const getScenario = (state: ReduxState) => state.mapFeature.scenario;
export const getQueryTrigger = (state: ReduxState) => state.mapFeature.queryTrigger;
export const getFeatures = (state: ReduxState) => state.mapFeature.features;

// Export the actions
export const { setProperties, setIri, setStack, setScenario, setQueryTrigger, addFeatures, clearFeatures } = mapFeatureSlice.actions;

// Export the reducer
export default mapFeatureSlice.reducer;