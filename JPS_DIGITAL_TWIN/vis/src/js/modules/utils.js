/*
*	Utility module for Digital Twin visualisation.
*/

// Exported features
export {
	arrayStringToNumber
};


/**
*	Given an array of strings, this function will attempt to parse
*	each entry as a number and return the result (if all entries
*	were valid numbers).
*
*	Parameters:
*		stringArray	-	Array of strings
*
*	Returns:
*		Array parsed as numbers (if not possible, original array
*			is returned.
*/
function arrayStringToNumber(stringArray) {
	var numberArray = [];
	
	for(var i = 0; i < stringArray.length; i++) {
		try {
			let entry = parseFloat(stringArray[i]);
			numberArray.push(entry);
		} catch(error) {
			return stringArray;
		}
	}
	
	return numberArray;
}