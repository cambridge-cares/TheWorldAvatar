import { createSlice } from '@reduxjs/toolkit';

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

export const { setProperties, setSourceLayerId } = mapFeatureSlice.actions;

export default mapFeatureSlice.reducer;