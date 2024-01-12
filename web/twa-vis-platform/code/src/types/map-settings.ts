
// Imagery settings object
export interface ImagerySettings {
    default: string,
    options: ImageryOption[]
}

// Imagery option object
export interface ImageryOption {
    name: string,
    url: string,
    time?: string
}

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
    imagery: ImagerySettings,
    credentials: {
        username: string,
        key: string
    }
}