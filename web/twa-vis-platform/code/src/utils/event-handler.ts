/**
 * Creates a click handler for a button that sets a boolean state to `true`. 
 * 
 * @param setState A React state updater function (`setState`) for a `boolean` state.
 */
export function genBooleanClickHandler(
  setState: React.Dispatch<React.SetStateAction<boolean>>
): React.MouseEventHandler<HTMLButtonElement> {
  return () => {
    setState(true);
  };
}