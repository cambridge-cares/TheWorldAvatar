import { createSlice } from '@reduxjs/toolkit';
import { ReduxState } from 'app/store';

export const floatingPanelSlice = createSlice({
    name: "floatingPanel",
    initialState: {
        index: 0,
        isStyleLoaded: false,
    },
    reducers: {
        setIndex: (state, action) => {
            state.index = action.payload.index;
        },
        setIsStyleLoaded: (state, action) => {
            state.isStyleLoaded = action.payload;
        },
    }
})

// Export selectors 
export const getIndex = (state: ReduxState) => state.floatingPanel.index;
export const getIsStyleLoaded = (state: ReduxState) => state.floatingPanel.isStyleLoaded;

// Export the actions
export const { setIndex, setIsStyleLoaded } = floatingPanelSlice.actions;

// Export the reducer
export default floatingPanelSlice.reducer;