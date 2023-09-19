/**
 * This class handles the creation, population, and visibility of a custom
 * HTML component to show when hovering over features.
 * 
 * This is done centrally (rather than using each mapping library's API)
 * to ensure consistency across all visualisation types.
 */
class PopupHandler {

    /**
     * Popup element.
     */
    private static POPUP;
    
    /**
     * Cache of currently shown metadata.
     */
    private static CACHED_META;

    /**
     * Builds the basic frame of the popup element.
     */
    private static buildPopup() {
        // Build empty element
        let container = document.createElement("div");
        container.classList.add("popup");
        container.id = "popup";

        // Add movement listeners
        let mapElement = document.getElementById("map");

        mapElement.addEventListener('mousemove', function(e) {
            if(container.style.display !== "none") {
                let left = e.offsetX;
                let top = e.offsetY;
                container.style.left = left + 'px';
                container.style.top = top + 'px';
            }
        });
        mapElement.addEventListener('mouseleave', function(e) {
            if(container.style.display !== "none") {
                PopupHandler.POPUP.style.display = "none";
            }
        });

        document.body.appendChild(container);
        this.POPUP = container;
    }

    /**
     * Updates the popup component with elements from the input metadata.
     * 
     * @param featureMetadata Object defining hovered feature's properties.
     */
    public static updatePopup(featureMetadata) {
        if(featureMetadata === this.CACHED_META) return;
        if(this.POPUP == null) this.buildPopup();

        // Running content string
        let html = "";

        // Add name if known
        let name = getName(featureMetadata);
        if(name != null && name !== "") {
            if(name.length >= 50) {
                name = name.substring(0, 30) + "-<br/>" + name.substring(50);
            }
            html += "<b>" + name + "</b>";
        }

        // Add description if known
        let desc = getDescription(featureMetadata);
        if(desc != null && desc !== "") {
            if(name != null && name !== "") {
                html += "<br/><br/>";
            }
            html += "<p>" + desc + "</p>";
        }

        // Add thumbnail if present
        if(featureMetadata.hasOwnProperty("thumbnail")) {
            html += "<img class='thumbnail' src='" + featureMetadata["thumbnail"] + "'>";
        }

        // If empty, then append a special class
        if(html.length === 0) {
            this.POPUP.classList.add("empty");
        } else if(this.POPUP.classList.contains("empty")) {
            this.POPUP.classList.remove("empty");
        }

        // Apply contents
        this.POPUP.innerHTML = html;

        // Cache the metadata to avoid later repetition
        this.CACHED_META = featureMetadata;
    }

    /**
     * Update the visibility of the popup element.
     * 
     * @param visible desired state.
     */
    public static setVisibility(visible) {
        if(this.POPUP == null) this.buildPopup();

        if(visible && this.POPUP.classList.contains("empty")) return;
        this.POPUP.style.display = (visible) ? "block" : "none";
    }

}
// End of class.