
// Camera settings object
export interface CameraSettings {
    default: string,
    positions: CameraPosition[]
}

// Camera position object
export interface CameraPosition {
    name: string,
    center: [number, number],
    zoom: number,
    bearing: number,
    pitch: number
}

// Overall map settings object
export interface MapSettings {
    type: string,
    camera: CameraSettings,
    credentials: {
        user: string,
        key: string
    }
}