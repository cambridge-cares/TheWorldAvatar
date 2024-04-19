type ScenarioDimensionStep = {
    value: number;
    label: string;
};

type ScenarioDimensionsData = {
    [key: string]: ScenarioDimensionStep[];
};

class SliderHandler {

    /**
     * Object with dimensions and their ranges from scenario endpoint
     */
    public scenarioTimesData: ScenarioDimensionsData;


    constructor(scenarioTimesData: ScenarioDimensionsData) {
        this.scenarioTimesData = scenarioTimesData;
    }

    public initialiseSlider(sliderActions: (e: Event) => void): void {

        let dimensions: ScenarioDimensionStep[][] = Object.values(this.scenarioTimesData);
        let sliderNames: string[] = Object.keys(this.scenarioTimesData)
        
        for (const i of [...this.scenarioTimesData.keys()]) {
            const sliderName = sliderNames[i];
            const array = dimensions[i]
            if (Array.isArray(array) && array.length > 1) {
                this.populateSlider(sliderName, array, sliderActions);
                this.changeLabel(array, 0);
                break;
            }
        }
    }

    public removeSlider() {
        if (document.getElementById("timeSliderContainer")) {
            document.getElementById("timeSliderContainer").remove();
        }
    }
    private populateSlider(sliderName: string, indexArray: Array<ScenarioDimensionStep>, sliderActions: (e: Event) => void): void {
        const firstIndex: number = indexArray[0].value;
        const lastIndex: number = indexArray[indexArray.length - 1].value;
        const firstLabel: string = indexArray[0].label;
        const lastLabel: string = indexArray[indexArray.length - 1].label;
        console.log(firstIndex, firstLabel, lastIndex, lastLabel)

        // slider container
        const sliderContainer: HTMLDivElement = document.createElement('div');
        sliderContainer.className = 'sliderContainer';
        sliderContainer.id = 'timeSliderContainer';

        // slider itself
        const slider: HTMLInputElement = document.createElement('input');
        slider.name = sliderName + '_Slider';
        slider.id = 'timeSlider_' + sliderName;
        slider.className = 'time-slider time-slider-jq blue';
        slider.type = 'range';
        slider.value = '1';
        slider.min = firstIndex.toString();
        slider.max = lastIndex.toString();

        slider.addEventListener('change', sliderActions);

        // Append slider to its container
        sliderContainer.appendChild(slider);

        // start time label
        const startTime: HTMLDivElement = document.createElement('div');
        startTime.className = 'time-label';
        startTime.id = 'sliderStartTime';
        startTime.innerHTML = firstLabel;
        sliderContainer.appendChild(startTime);

        // end time label
        const endTime: HTMLDivElement = document.createElement('div');

        endTime.className = 'time-label';
        endTime.id = 'sliderEndTime';
        endTime.innerHTML = lastLabel;
        sliderContainer.appendChild(endTime);

        // current time label
        const timeSliderLabel: HTMLDivElement = document.createElement('div');
        timeSliderLabel.className = 'time-label';
        timeSliderLabel.id = 'timeSliderLabel';
        sliderContainer.appendChild(timeSliderLabel);

        document.body.appendChild(sliderContainer);
    }


    private changeLabel(indexArray: Array<ScenarioDimensionStep>, currentSliderIndex: number): void {
        const label: string = indexArray[currentSliderIndex].label;
        const sliderLabel: HTMLElement | null = document.getElementById('timeSliderLabel');
        if (sliderLabel) {
            sliderLabel.innerHTML = label;
        }
    }
}