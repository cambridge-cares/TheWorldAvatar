import { createSlice } from "@reduxjs/toolkit";
import { ReduxState } from "../app/store";

export const ribbonComponentSlice = createSlice({
    name: "ribbonComponent",
    initialState: {
        items: []
    },
    reducers: {
         setOption: (state, action) => {
            // Find entry by name
            const match = state.items.find((item) => 
                item.name === action.payload.name
            );

            if(match == null) {
                state.items = state.items.concat(action.payload);
            } else {
                match["selection"] = action.payload["selection"];
            }
        }
    }
})

// Export selectors 
export const getOption = (name: string) => (state: ReduxState) => {
    if(state?.ribbonComponents?.items == null) return null;
    return state.ribbonComponents.items.find((item) => item.name === name)
};

// Export the actions
export const { setOption } = ribbonComponentSlice.actions;

// Export the reducer
export default ribbonComponentSlice.reducer;
