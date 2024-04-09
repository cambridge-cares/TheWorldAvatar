import { createSlice, PayloadAction } from '@reduxjs/toolkit';
import { ReduxState } from 'app/store';

// Define the LatLngPayload interface directly above the slice where it's used
interface LatLngPayload {
    lat: number;
    lng: number;
}

export const mapFeatureSlice = createSlice({
    name: 'mapFeature',
    initialState: {
        name: null,
        latLng: null as LatLngPayload | null,
        iri: null,
        stack: null,
        scenario: null,
        queryTrigger: false,
    },
    reducers: {
        setName: (state, action) => {
            state.name = action.payload;
        },
        setLatLng: (state, action: PayloadAction<LatLngPayload>) => {
            state.latLng = action.payload; // Update state with new coordinates
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
export const getLatLng = (state: ReduxState) => state.mapFeature.latLng;
export const getName = (state: ReduxState) => state.mapFeature.name;
export const getIri = (state: ReduxState) => state.mapFeature.iri;
export const getStack = (state: ReduxState) => state.mapFeature.stack;
export const getScenario = (state: ReduxState) => state.mapFeature.scenario;
export const getQueryTrigger = (state: ReduxState) => state.mapFeature.queryTrigger;

// Export the actions
export const { setLatLng, setName, setIri, setStack, setScenario, setQueryTrigger } = mapFeatureSlice.actions;

// Export the reducer
export default mapFeatureSlice.reducer;