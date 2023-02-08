#!/bin/bash

# Create directory for stack_client resource files (to be imported by py4jps)
mkdir tmp_stack

# Build latest STACK_CLIENTS .jar
cd ../../Deploy/stacks/dynamic/stack-clients
mvn clean package -DskipTests
cp ./target/stack-clients*.jar ../../../../Agents/AverageSquareMetrePriceAgent/tmp_stack/
cp -r ./target/lib ../../../../Agents/AverageSquareMetrePriceAgent/tmp_stack/
