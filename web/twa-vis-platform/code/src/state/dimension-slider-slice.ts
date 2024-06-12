import { createSlice, PayloadAction } from '@reduxjs/toolkit';
import { ScenarioDimensionStep } from 'types/timeseries';

interface DimensionSliderState {
    value: number;
    values: ScenarioDimensionStep[];
}

const initialState: DimensionSliderState = {
    value: 0,
    values: [],
};

const dimensionSliderSlice = createSlice({
    name: 'dimensionSlider',
    initialState,
    reducers: {
        setValue: (state, action: PayloadAction<number>) => {
            state.value = action.payload;
        },
        setValues: (state, action: PayloadAction<ScenarioDimensionStep[]>) => {
            state.values = action.payload;
        },
    },
});

export const { setValue, setValues } = dimensionSliderSlice.actions;

export default dimensionSliderSlice.reducer;
