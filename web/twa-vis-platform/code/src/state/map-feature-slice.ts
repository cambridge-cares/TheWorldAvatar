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
        properties: null,
        sourceLayerId: null,
        latLng: null as LatLngPayload | null
    },
    reducers: {
        setName: (state, action) => {
            state.name = action.payload;
        },
        setLatLng: (state, action: PayloadAction<LatLngPayload>) => {
            state.latLng = action.payload; // Update state with new coordinates
        },
        setProperties: (state, action) => {
            state.properties = action.payload;
        },
        setSourceLayerId: (state, action) => {
            state.sourceLayerId = action.payload;
        },
    },
});

// Export selectors 
export const getLatLng = (state: ReduxState) => state.mapFeature.latLng;
export const getProperties = (state: ReduxState) => state.mapFeature.properties;
export const getName = (state: ReduxState) => state.mapFeature.name;
export const getSourceLayerId = (state: ReduxState) => state.mapFeature.sourceLayerId;

// Export the actions
export const { setLatLng, setName, setProperties, setSourceLayerId } = mapFeatureSlice.actions;

// Export the reducer
export default mapFeatureSlice.reducer;