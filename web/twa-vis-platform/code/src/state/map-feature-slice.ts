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
        url: null,
        queryTrigger: false,
    },
    reducers: {
        setName: (state, action) => {
            state.name = action.payload;
        },
        setLatLng: (state, action: PayloadAction<LatLngPayload>) => {
            state.latLng = action.payload; // Update state with new coordinates
        },
        setUrl: (state, action) => {
            state.url = action.payload;
        },
        setQueryTrigger: (state, action) => {
            state.queryTrigger = action.payload;
        },
    },
});

// Export selectors 
export const getLatLng = (state: ReduxState) => state.mapFeature.latLng;
export const getName = (state: ReduxState) => state.mapFeature.name;
export const getUrl = (state: ReduxState) => state.mapFeature.url;
export const getQueryTrigger = (state: ReduxState) => state.mapFeature.queryTrigger;

// Export the actions
export const { setLatLng, setName, setUrl, setQueryTrigger } = mapFeatureSlice.actions;

// Export the reducer
export default mapFeatureSlice.reducer;