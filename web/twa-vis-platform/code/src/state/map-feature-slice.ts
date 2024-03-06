import { createSlice } from '@reduxjs/toolkit';
import { ReduxState } from "../app/store";

export const mapFeatureSlice = createSlice({
    name: 'mapFeature',
    initialState: {
        properties: null,
        sourceLayerId: null,
    },
    reducers: {
        setProperties: (state, action) => {
            state.properties = action.payload;
        },
        setSourceLayerId: (state, action) => {
            state.sourceLayerId = action.payload;
        },
    },
});

// Export selectors 
export const getProperties = (state: ReduxState) => state.mapFeature.properties;
export const getSourceLayerId = (state: ReduxState) => state.mapFeature.sourceLayerId;

// Export the actions
export const { setProperties, setSourceLayerId } = mapFeatureSlice.actions;

// Export the reducer
export default mapFeatureSlice.reducer;