// public async fetchScenarioDimensions(): Promise < ScenarioDimensionsData > {
//     try {
//         const response = await fetch(`${this.agentBaseURL}/getScenarioTimes/${this.selectedScenario}`);
//         const data: ScenarioDimensionsData = await response.json();
//         return data;
//     } catch(error) {
//         console.error('Error fetching times from CentralStackAgent/getScenarioTimes:', error);
//     }
// }