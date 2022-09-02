export {};

declare global {
    interface Window {
        terrain: string;
        selectFeatures: Object;
        manager: Manager;
        currentFeature: Object;
    }
}