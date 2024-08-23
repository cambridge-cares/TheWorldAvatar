/**
 * Handles formatting of JSON metadata before being sent to the side 
 * panel for visualisation.
 */
class JSONFormatter {


    /**
     * Formats the input JSON object using a transformer.
     * 
     * @param input raw JSON object input
     * 
     * @returns formatted JSON object
     */
    public static formatJSON(input) {
        // Parse with a transform and return
        let string = JSON.stringify(input);
        return JSON.parse(string, function(key, value) {

            if(typeof key === "string") {
                if(typeof value === "string") {
                    let newKey = JSONFormatter.formatKey(key);
                    let newValue = JSONFormatter.formatValue(value);
                    this[newKey] = newValue;

                    if(newKey === key) {
                        return newValue;
                    } else {
                        return undefined;
                    }
                }
            }
           
            return value;
        });
    }

    private static formatKey(key) {
        let newKey = key;

        if(newKey.includes("\"")) {
            newKey = newKey.replaceAll("\"", "");
        }
        if(newKey.endsWith(".")) {
            newKey = newKey.substring(0, key.length - 1);
        }
        return newKey;
    }

    private static formatValue(value) {
        let newValue = value;

        // If JSON in string form, parse as JSON
        if(newValue.startsWith("{") || newValue.startsWith("[")) {
            try {
                return JSON.parse(newValue);
            } catch(error) {
                // Nothing, continue to treat as a String
            }
        }
        if(newValue.includes("Â")) {
            newValue = newValue.replaceAll("Â", "");
        }
        if(newValue.includes("\"")) {
            newValue = newValue.replaceAll("\"", "");
        }
        if(newValue.includes("^^")) {
            newValue = newValue.split("^^")[0];
        }

        if(value.includes("[") && value.includes("]") && !newValue.includes("[") && !newValue.includes("]")) {
            let pattern = new RegExp("(?<=\\[).*(?=\\])");
            let unit = value.match(pattern);
            newValue = newValue.trim() + " [" + unit + "]";
        }

        return newValue;
    }


}
// End of class.