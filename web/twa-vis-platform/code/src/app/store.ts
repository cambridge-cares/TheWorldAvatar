
import { configureStore } from '@reduxjs/toolkit';
import contextMenuReducer from 'state/context-menu-slice';
import ribbonComponentReducer from 'state/ribbon-component-slice';
import floatingPanelReducer from 'state/floating-panel-slice';
import mapFeatureReducer from 'state/map-feature-slice';
import { featureInfoAgentApi } from 'state/api/fia-api';
import dimensionSliderSlice from 'state/dimension-slider-slice';
import modalReducer from 'state/modal-slice';

// Initialise and export store
export const reduxStore = configureStore({
    reducer: {
        contextMenu: contextMenuReducer,
        ribbonComponents: ribbonComponentReducer,
        floatingPanel: floatingPanelReducer,
        mapFeature: mapFeatureReducer,
        modal: modalReducer,
        dimensionSlider: dimensionSliderSlice,
        [featureInfoAgentApi.reducerPath]: featureInfoAgentApi.reducer,
    },
    // Adding the api middleware enables caching, invalidation, polling, and other useful features of `rtk-query`.
    middleware: (getDefaultMiddleware) => getDefaultMiddleware().concat(featureInfoAgentApi.middleware),
});

// Export the type used for the store object.
export type ReduxStore = typeof reduxStore;

// Export the type used for the state object within the store.
export type ReduxState = ReturnType<typeof reduxStore.getState>;