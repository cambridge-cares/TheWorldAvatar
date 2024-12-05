import Slider from '@mui/material/Slider';
import * as React from 'react';
import { useEffect, useState } from 'react';
import { useDispatch, useSelector } from 'react-redux';
import { selectDimensionSliderValue, setValue } from 'state/dimension-slider-slice';
import { ScenarioDimensionsData, ScenarioDimensionStep } from 'types/timeseries';
import styles from './slider.module.css';

interface DimensionSliderProps {
    data: ScenarioDimensionsData;
}

function valuetext(value: number, values: ScenarioDimensionStep[]) {
    const selectedStep = values.find(step => step.value === value);
    return selectedStep ? selectedStep.label : 'Unknown';
}


export default function DimensionSlider({ data }: DimensionSliderProps) {
    const values = Object.values(data).flat();
    const min = values[0]?.value;
    const max = values[values.length - 1]?.value;
    const middle = Math.round((min + max) / 2);
    const dispatch = useDispatch();
    const dimensionSliderValue = useSelector(selectDimensionSliderValue);
    const [tempValue, setTempValue] = useState(dimensionSliderValue);
    
    const handleChange = (event : React.SyntheticEvent | Event, newValue: number | number[]) => {
        setTempValue(newValue);
    };

    const handleChangeCommitted = (
        event: React.SyntheticEvent | Event,
        value: number | number[],
        values: ScenarioDimensionStep[]
    ) => {
        // Slider supports multiple thumbs, so 'value' could be an array, need to handle both cases
        const firstValue = Array.isArray(value) ? value[0] : value;
        const selectedStep = values.find(step => step.value === firstValue);
        const label = selectedStep ? selectedStep.label : 'Unknown';

        dispatch(setValue(value));
    }

    return (
        <div className={styles.sliderContainer}>
            {/* <span>{Object.keys(data).flat()}</span> ---TODO slider label if we want later */}
            <Slider
                key={`slider-${Object.keys(data).flat()}`}
                className={styles.discreteSlider}
                aria-label="Time"
                defaultValue={middle}
                value={tempValue}
                valueLabelFormat={(value: number) => valuetext(value, values)}
                valueLabelDisplay="auto"
                shiftStep={1}
                step={1}
                marks
                min={min}
                max={max}
                onChange={handleChange}
                onChangeCommitted={(event: React.SyntheticEvent | Event, value: number | number[]) => handleChangeCommitted(event, value, values)}
            />
        </div>
    );
}
