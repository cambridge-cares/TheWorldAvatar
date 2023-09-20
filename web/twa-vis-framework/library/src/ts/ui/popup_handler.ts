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
        container.style.transform = "translate(-50%,calc(100% + 20px))"
        container.id = "popup";

        // Add movement listeners
        let mapElement = document.getElementById("map");

        mapElement.addEventListener('mousemove', function(e) {
            if(container.style.display !== "none") {
                let left = e.offsetX;
                let top = e.offsetY;
                container.style.left = left + 'px';
                container.style.top = top + 'px';

                let translateX = "-50%";
                if(left < 200) {
                    translateX = "20px";
                } else if (left > (mapElement.offsetWidth - 200)) {
                    translateX = "calc(-100% + 20px)";
                }

                let translateY = "20px";
                if(top < 200) {
                    translateY = "20px";
                } else if (top > (mapElement.offsetHeight - 200)) {
                    translateY = "calc(-100% - 20px)";
                }

                container.style.transform = "translate(" + translateX + "," + translateY + ")";
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
            name = this.wrapString(name);
            html += "<b>" + name + "</b>";
        }

        // Add description if known
        let desc = getDescription(featureMetadata);
        if(desc != null && desc !== "") {
            if(name != null && name !== "") {
                html += "<br/><br/>";
            }
            desc = this.wrapString(desc);

            if(desc.length > 250) {
                desc = desc.substring(0, 247) + "...";
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
     * 
     */
    public static wrapString(content) {
        let oldParts = content.split(" ");
        let newString = "";

        for(let i = 0; i < oldParts.length; i++) {
            if(oldParts[i].length > 40) {

                let newParts = oldParts[i].match(new RegExp('.{1,' + 40 + '}', 'g'));
                for(let j = 0; j < newParts.length; j++) {
                    newString += newParts[j];
                    if(j < (newParts.length - 1)) {
                        newString += "-<br/>";
                    } else {
                        newString += " ";
                    }
                }

            } else {
                newString += oldParts[i];
                if(i < (oldParts.length - 1)) newString += " ";
            }
        }

        return newString;
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