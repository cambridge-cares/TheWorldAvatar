import styles from './slider.module.css';
import Slider from '@mui/material/Slider';
import * as React from 'react';
import { ScenarioDimensionStep } from 'types/timeseries';

interface DiscreteSliderProps {
    values: ScenarioDimensionStep[];
}

function valuetext(value: number, values: ScenarioDimensionStep[]) {
    const selectedStep = values.find(step => step.value === value);
    return selectedStep ? selectedStep.label : 'Unknown';
}

function handleChange(
    event: React.SyntheticEvent | Event,
    value: number | number[],
    values: ScenarioDimensionStep[]
) {
    // Slider supports multiple thumbs, so 'value' could be an array, need to handle both cases
    const singleValue = Array.isArray(value) ? value[0] : value;
    const selectedStep = values.find(step => step.value === singleValue);
    const label = selectedStep ? selectedStep.label : 'Unknown';
    console.log('slider moved to index', singleValue, 'with label:', label);
}

export default function DiscreteSlider({ values }: DiscreteSliderProps) {
    const min = values[0].value;
    const max = values[values.length - 1].value;
    const middle = Math.round((min + max) / 2);

    return (
        <div className={styles.sliderContainer}>
            <Slider
                className={styles.discreteSlider}
                aria-label="Time"
                defaultValue={middle}
                valueLabelFormat={(value: number) => valuetext(value, values)}
                valueLabelDisplay="auto"
                shiftStep={1}
                step={1}
                marks
                min={min}
                max={max}
                onChangeCommitted={(event, value) => handleChange(event, value, values)}
            />
        </div>
    );
}
