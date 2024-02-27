import styles from "./info-tree.module.css";
import React, { useEffect, useState } from "react";
import SVG from "react-inlinesvg";
import { Icon } from "@mui/material";
import { useSelector } from "react-redux";
import { getLatLng } from "../../state/floating-panel-click-slice";
import { selectSelectedFeature } from "../../state/map-feature-slice";

export default function InfoTree() {
  const latLng = useSelector(getLatLng);
  const selectedFeature = useSelector(selectSelectedFeature); // Use useSelector here

  // State to store the fetched stack data
  const [stack, setStack] = useState("");

  // Fetch data from API on component mount
  useEffect(() => {
    const fetchData = async () => {
      try {
        const response = await fetch("/api/visualisation/data");
        if (!response.ok) {
          throw new Error("Network response was not ok");
        }
        const data = await response.json();
        // Assuming the stack you want to display is at the root of your JSON
        setStack(data.stack);
      } catch (error) {
        console.error("Failed to fetch stack:", error);
      }
    };

    fetchData();
  }, []); // Empty dependency array means this effect runs once on mount

  return (
    <div className={styles.infoPanelContainer}>
      <h2>Information</h2>
      {latLng && (
        <div className={styles.infoHeadSection}>
          <h3>Clicked Location</h3>
          <p>
            Latitude: {latLng.lat.toFixed(2)} Longitude: {latLng.lng.toFixed(2)}{" "}
          </p>
        </div>
      )}

      {selectedFeature && ( // Ensure selectedFeature is not null or undefined
        <div className={styles.infoSection}>
          <h3>Feature Information</h3>
          <p>Name: {selectedFeature.name}</p> {/* Correctly display the name */}
          <p>Description: {selectedFeature.description}</p>{" "}
          <p>IRI: {selectedFeature.iri}</p> {/* Display the description */}
        </div>
      )}

      {stack && ( // Display the stack if it's not empty
        <div className={styles.infoSection}>
          <h3>Stack Information</h3>
          <p>{stack}</p>
        </div>
      )}

      <div className={styles.infoSection}>
        <h3>How to Use</h3>
        <ul>
          <li>Basic interaction 1</li>
          <li>Basic interaction 2</li>
          <li>Additional instructions</li>
        </ul>
      </div>
    </div>
  );
}
