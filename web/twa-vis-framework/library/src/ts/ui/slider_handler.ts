type ScenarioDimensionStep = {
    value: number;
    label: string;
};

type ScenarioDimensionsData = {
    [key: string]: ScenarioDimensionStep[];
};

type SliderActions = (e: Event) => void;

type SliderActionsArray = SliderActions[];

class SliderHandler {

    /**
     * Object with dimensions and their ranges from scenario endpoint
     */
    public scenarioDimensionsData: ScenarioDimensionsData;

    

    /**
     * Pass through the manager instance to be able to call any sibling class methods
     */
    private manager: Manager;


    constructor(scenarioDimensionsData: ScenarioDimensionsData, manager: Manager) {
        this.scenarioDimensionsData = scenarioDimensionsData;
        this.manager = manager;
    }

    public initialiseAllSliders(sliderActionsArray: SliderActionsArray): void {

        let dimensions: ScenarioDimensionStep[][] = Object.values(this.scenarioDimensionsData);
        let sliderNames: string[] = Object.keys(this.scenarioDimensionsData)
        
        for (let i = 0; i < sliderNames.length; i++ ) {
            const sliderName = sliderNames[i];
            const array = dimensions[i]
            const sliderActions = sliderActionsArray[i]
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
    private populateSlider(sliderName: string, indexArray: Array<ScenarioDimensionStep>, sliderActions: SliderActions) : void {
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


    /**
     * Change the label that shows what current timeStep is on the slider
     * @param indexArray the array of timesteps to be passed through
     * @param currentSliderIndex the current slider value / index to get from the above array
     */
    private changeLabel(indexArray: Array<ScenarioDimensionStep>, currentSliderIndex: number): void {
        const label: string = indexArray[currentSliderIndex].label;
        const sliderLabel: HTMLElement | null = document.getElementById('timeSliderLabel');
        if (sliderLabel) {
            sliderLabel.innerHTML = label;
        }
    }

}