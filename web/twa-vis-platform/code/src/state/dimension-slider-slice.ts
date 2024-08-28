import { createSlice, PayloadAction } from '@reduxjs/toolkit';
import { ReduxState } from 'app/store';
// import { ScenarioDimensionStep } from 'types/timeseries';

interface DimensionSliderState {
    value: number | number[];
    // values: ScenarioDimensionStep[];
}

const initialState: DimensionSliderState = {
    value: 0,
    // values: [],
};

const dimensionSliderSlice = createSlice({
    name: 'dimensionSlider',
    initialState,
    reducers: {
        setValue: (state, action: PayloadAction<number| number[]>) => {
            if (Array.isArray(action.payload)) {
                state.value = action.payload;
            } else {
                state.value = [action.payload];
            }
        }
        // ,
        // setValues: (state, action: PayloadAction<ScenarioDimensionStep[]>) => {
        //     state.values = action.payload;
        // },
    },
});

export const { setValue } = dimensionSliderSlice.actions;

export const selectDimensionSliderValue = (state: ReduxState) => state.dimensionSlider.value

export default dimensionSliderSlice.reducer;
