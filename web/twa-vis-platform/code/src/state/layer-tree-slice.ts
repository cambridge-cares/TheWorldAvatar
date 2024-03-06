import { createSlice } from '@reduxjs/toolkit';
import { ReduxState } from 'app/store';

export const layerTreeSlice = createSlice({
    name: "layerTree",
    initialState: {
        items: []
    },
    reducers: {
        addItem: (state, action) => {
            // Check for collision
            const match = state.items.find((item) => 
                item.name === action.payload.name
            );

            if(match == null) {
                // Update state with new item
                state.items = state.items.concat(action.payload);
            }
        },
        removeItem: (state, action) => {
            state.items = state.items.filter(function(item){
                return item.name !== action.payload
            })
        },
        setVisibility: (state, action) => {
            const match = state.items.find((item) => 
                item.name === action.payload
            );

            // Set visibility state
            if(match?.visibility != null) {
                match.visibility = action.payload.visibility;
            }
        },
        setExpanded: (state, action) => {
            const match = state.items.find((item) => 
                item.name === action.payload
            );

            // Set expanded state
            if(match?.expanded != null) {
                match.expanded = action.payload.expanded;
            }
        }
    }
})

// Export selectors 
export const selectItems = (state: ReduxState) => state.contextMenu.items;
export const selectItem = (name: string) => (state: ReduxState) => {
    if(state?.contextMenu?.items == null) return null;
    return state.contextMenu.items.find((item) => item.name === name)
};

// Export the actions
export const { addItem, removeItem, setVisibility, setExpanded} = layerTreeSlice.actions;

// Export the reducer
export default layerTreeSlice.reducer;
