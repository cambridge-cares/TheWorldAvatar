# Check if the correct number of arguments is provided
if [ "$#" -ne 2 ] || [ "$1" != "start" ]; then
    echo "Usage: $0 start <STACK_NAME>"
    exit 1
fi

STACK_NAME="$2"
routing="accessagent_resources/routing.json"  # Adjust the path as needed

# Check if the source file exists
if [ -e "$routing" ]; then
    # Replace <STACK-NAME> with the second argument in the source
    sed -i "s/<STACK-NAME>/$STACK_NAME/g" "$routing"

    # Check if the sed command was successful
    if [ $? -eq 0 ]; then
        echo "Placeholder replaced successfully."
    else
        echo "Error: Failed to replace the placeholder in the file."
        exit 1
    fi
else
    echo "Error: Source file not found."
    exit 1
fi


cd accessagent_resources || exit 1

./createStoreRouter.sh

./uploadRouting.sh

echo "Commands executed successfully."



