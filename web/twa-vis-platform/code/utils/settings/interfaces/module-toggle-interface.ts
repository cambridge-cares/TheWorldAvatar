import ModuleToggle from "../module-toggle";
import { IllegalArgumentError } from "../../errors";

/**
 * Interface for module toggles JSON object.
 */
export declare interface ModuleToggleInterface {
    [key: string]: boolean;
}

/**
 * Tests that the input ModuleToggleJSON object is
 * valid for use.
 * 
 * @param jsonObject 
 * 
 * @returns validity.
 * 
 * @throws {IllegalArgumentError} if object is invalid.
 */
export function isValid(jsonObject: object): boolean {
    for (const [key, value] of Object.entries(jsonObject)) {
        // Only valid is value is boolean
        if(typeof value !== "boolean") {
            console.log("THROWING!!!");
            throw new IllegalArgumentError("UI module setting does not use a boolean value: " + key);
        }

        // Is there a ModuleToggle instance with matching name
        let match: boolean = false;
        for(const [a, b] of Object.entries(ModuleToggle)) {
            if(!match && b instanceof ModuleToggle) {
                if(b.key === key) match = true;       
            }
        }

        if(!match) {
            console.log("THROWING!!!");
            throw new IllegalArgumentError("UI module setting does not correspond to a valid key: " + key);
        }
    }
    return true;
}