import { createSlice } from '@reduxjs/toolkit';
import { ReduxState } from 'app/store';

export const floatingPanelSlice = createSlice({
    name: "floatingPanel",
    initialState: {
        index: 0,
        hasExistingData: false,
    },
    reducers: {
        setIndex: (state, action) => {
            state.index = action.payload.index;
        },
        setHasExistingData: (state, action) => {
            state.hasExistingData = action.payload;
        },
    }
})

// Export selectors 
export const getIndex = (state: ReduxState) => state.floatingPanel.index;
export const getHasExistingData = (state: ReduxState) => state.floatingPanel.hasExistingData;

// Export the actions
export const { setIndex, setHasExistingData } = floatingPanelSlice.actions;

// Export the reducer
export default floatingPanelSlice.reducer;