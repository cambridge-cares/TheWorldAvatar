import { createSlice } from "@reduxjs/toolkit";
import { ReduxState } from "../app/store";

export const floatingPanelSlice = createSlice({
    name: "floatingPanel",
    initialState: {
        index: 0
    },
    reducers: {
         setIndex: (state, action) => {
            state.index = action.payload.index;
        }
    }
})

// Export selectors 
export const getIndex = (state: ReduxState) => state.floatingPanel.index;

// Export the actions
export const { setIndex } = floatingPanelSlice.actions;

// Export the reducer
export default floatingPanelSlice.reducer;
