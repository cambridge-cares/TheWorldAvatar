type ScenarioTimeStep = {
    value: number;
    label: string;
};

type ScenarioTimesData = {
    [key: string]: ScenarioTimeStep[];
};

class SliderHandler {

    /**
     * Object with dimensions and their ranges from scenario endpoint
     */
    public scenarioTimesData: ScenarioTimesData;


    constructor(scenarioTimesData: ScenarioTimesData) {
        this.scenarioTimesData = scenarioTimesData;
    }

    public initialiseSlider(sliderActions: (e: Event) => void): void {

        let dimensions = Object.values(this.scenarioTimesData);
        for (let array of dimensions) {
            if (Array.isArray(array) && array.length > 1) {
                this.populateSlider(array, sliderActions);
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
    private populateSlider(indexArray: Array<ScenarioTimeStep>, sliderActions: (e: Event) => void): void {
        const firstIndex: number = indexArray[0].value;
        const lastIndex: number = indexArray[indexArray.length - 1].value;
        const firstLabel: string = indexArray[0].label;
        const lastLabel: string = indexArray[indexArray.length - 1].label;
        console.log(firstIndex, firstLabel, lastIndex, lastLabel)

        // slider container
        const sliderContainer: HTMLDivElement = document.createElement('div');
        sliderContainer.className = 'slidecontainer';
        sliderContainer.id = 'timeSliderContainer';

        // slider itself
        const slider: HTMLInputElement = document.createElement('input');
        slider.name = 'time-slider';
        slider.id = 'timeSlider';
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


    private changeLabel(indexArray: Array<ScenarioTimeStep>, currentSliderIndex: number): void {
        const label: string = indexArray[currentSliderIndex].label;
        const sliderLabel: HTMLElement | null = document.getElementById('timeSliderLabel');
        if (sliderLabel) {
            sliderLabel.innerHTML = label;
        }
    }
}