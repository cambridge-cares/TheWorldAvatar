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
        return JSON.parse(JSON.stringify(input), function(key, value) {

            if(typeof value === "string") {
                let newKey = key;
                let newValue = value;

                if(newKey.includes("\"")) {
                    newKey = newKey.replaceAll("\"", "");
                }
                if(newKey.endsWith(".")) {
                    newKey = newKey.substring(0, key.length - 1);
                }
    
                if(newValue.includes("\"")) {
                    newValue = newValue.replaceAll("\"", "");
                }
                if(newValue.includes("^^")) {
                    newValue = newValue.split("^^")[0];
                }

                this[newKey] = newValue;
                return (newKey === key) ? value : undefined;
            }
           
            return value;
        });
    }


}
// End of class.