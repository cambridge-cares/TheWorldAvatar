import { createSlice, PayloadAction  } from "@reduxjs/toolkit";
import { ReduxState } from "../app/store";

// Define the LatLngPayload interface directly above the slice where it's used
interface LatLngPayload {
    lat: number;
    lng: number;
}

export const mapClickSlice = createSlice({
    name: "mapClick",
    initialState: {
        latLng: null as LatLngPayload | null
    },
    reducers: {
        setLatLng: (state, action: PayloadAction<LatLngPayload>) => {
            state.latLng = action.payload; // Update state with new coordinates
        }
    }
});

// Export selectors 
export const getLatLng = (state: ReduxState) => state.mapClick.latLng;

// Export the actions
export const { setLatLng } = mapClickSlice.actions;

// Export the reducer
export default mapClickSlice.reducer;