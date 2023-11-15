import path from "path";
import { IllegalArgumentError } from "@/utils/errors";
import { ModuleToggle } from "@/utils/settings/module-toggle";

// Test data directory.
const DATA_DIR = "./__tests__/__data__/";

// Test suite for ModuleToggle class.
describe("All ModuleToggle tests", function() {

    /**
     * Clear cached settings after each test as each
     * suite shares a runtime environment.
     */
    afterEach(() => {
        ModuleToggle.clearModuleStatuses();
    });

    /**
     * Tests that a valid UI modules configuration file
     * can be read without causing any errors.
     */
    test('Read valid ModuleToggle config.', () => {
        // Read module toggles
        ModuleToggle.readModuleSettings(path.join(DATA_DIR, "modules.1.json"));
        ModuleToggle.getAllModuleStatuses();
    });

    /**
     * Tests that a sample UI modules configuration file with correct
     * keys but incorrectly typed values correctly causes the expected
     * IllegalArgumentError.
     */
    test('Read badly valued ModuleToggle config.', () => { 
        const func = () => {
            // Read module toggles
            ModuleToggle.readModuleSettings(path.join(DATA_DIR, "modules.2.json"));
            ModuleToggle.getAllModuleStatuses();
        };
        expect(func).toThrow(IllegalArgumentError);
    });

    /**
     * Tests that a sample UI modules configuration file with incorrect
     * keys but correctly typed values correctly causes the expected
     * IllegalArgumentError.
     */
     test('Read badly keyed ModuleToggle config.', () => { 
        const func = () => {
            // Read module toggles
            ModuleToggle.readModuleSettings(path.join(DATA_DIR, "modules.2.json"));
            ModuleToggle.getAllModuleStatuses();
        };
        expect(func).toThrow(IllegalArgumentError);
    });

    
});
// End of test suite.