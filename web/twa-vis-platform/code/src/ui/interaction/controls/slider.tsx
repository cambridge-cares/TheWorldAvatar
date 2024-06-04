import styles from './slider.module.css'
import Slider from '@mui/material/Slider'
import * as React from 'react';
import { ScenarioDimensionStep } from 'types/timeseries';


function valuetext(value: number) {
    return `${value}`;
}

interface DiscreteSliderProps {
    values: ScenarioDimensionStep[];
}

export default function DiscreteSlider({values}: DiscreteSliderProps) {

    const min = values[0].value;
    const max = values[values.length - 1].value;
    const middle = (min + max) / 2

    return (
        <div className={styles.sliderConainer}>
            <Slider
            className='time-slider'
                aria-label="Time"
                defaultValue={middle}
                getAriaValueText={valuetext}
                valueLabelDisplay="auto"
                shiftStep={1}
                step={1}
                marks
                min={min}
                max={max}
            />
        </div>
    );
}