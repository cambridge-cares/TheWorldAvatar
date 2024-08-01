import { createSlice } from '@reduxjs/toolkit';
import { ReduxState } from 'app/store';

export const contextMenuSlice = createSlice({
    name: "contextMenu",
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
        toggleItem: (state, action) => {
            // Find entry by name
            const match = state.items.find((item) => 
                item.name === action.payload
            );

            // Set toggled state
            if(match?.toggled != null) {
                match.toggled = !match.toggled;
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
export const { addItem, removeItem, toggleItem} = contextMenuSlice.actions;

// Export the reducer
export default contextMenuSlice.reducer;
