import { DataStore } from "./data-store"

/**
 * This class caches instances of DataStore objects, each one representing a
 * single parsed data.json file.
 */
export class DataStoreCache {

    // Single store of cached instances
    public static readonly STORES: {
        [key: string]: DataStore
    } = {}

    /**
     * Singleton.
     */
    private constructor() {
        // No.
    }
}
// End of class.