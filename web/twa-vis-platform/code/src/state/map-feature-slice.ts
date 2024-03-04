import { createSlice } from '@reduxjs/toolkit';

export const mapFeatureSlice = createSlice({
    name: 'mapFeature',
    initialState: {
        selectedFeature: null,
    },
    reducers: {
        setSelectedFeature: (state, action) => {
            state.selectedFeature = action.payload;
        },
    },
});

export const { setSelectedFeature } = mapFeatureSlice.actions;

export const selectSelectedFeature = (state) => state.mapFeature.selectedFeature;

export default mapFeatureSlice.reducer;