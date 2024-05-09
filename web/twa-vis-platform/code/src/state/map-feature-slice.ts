import { createSlice } from '@reduxjs/toolkit';
import { ReduxState } from 'app/store';

export const mapFeatureSlice = createSlice({
    name: 'mapFeature',
    initialState: {
        properties: null,
        iri: null,
        stack: null,
        scenario: null,
        queryTrigger: false,
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
    },
});

// Export selectors 
export const getProperties = (state: ReduxState) => state.mapFeature.properties;
export const getIri = (state: ReduxState) => state.mapFeature.iri;
export const getStack = (state: ReduxState) => state.mapFeature.stack;
export const getScenario = (state: ReduxState) => state.mapFeature.scenario;
export const getQueryTrigger = (state: ReduxState) => state.mapFeature.queryTrigger;

// Export the actions
export const { setProperties, setIri, setStack, setScenario, setQueryTrigger } = mapFeatureSlice.actions;

// Export the reducer
export default mapFeatureSlice.reducer;