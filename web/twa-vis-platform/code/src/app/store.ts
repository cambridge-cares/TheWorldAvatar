
import { configureStore } from '@reduxjs/toolkit';
import contextMenuReducer from "state/context-menu-slice";
import ribbonComponentReducer from "state/ribbon-component-slice";

// Initialise and export store
export const reduxStore = configureStore({
    reducer: {
        contextMenu: contextMenuReducer,
        ribbonComponents: ribbonComponentReducer
    }
});

// Export the type used for the store object.
export type ReduxStore = typeof reduxStore;

// Export the type used for the state object within the store.
export type ReduxState = ReturnType<typeof reduxStore.getState>;