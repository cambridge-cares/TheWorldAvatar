##A Template maker script for ESP32 arduino sensor systems in TWA systems
import json
import os
import sys
import logging

logging.basicConfig(filename='templatemaker.log', filemode='w', encoding='utf-8', level=logging.DEBUG)

RESULT_INO = "result"

#Change this according to filename and location
SENSOR_JSON = "prox_sens.json"
SENSOR_JSON_LOC = "./example/"

TARGET_DIR = f"{SENSOR_JSON_LOC}/target"

if not os.path.exists(TARGET_DIR):
    logging.info(f"Create folder {TARGET_DIR}")
    os.makedirs(TARGET_DIR)

if not os.path.exists(os.path.join(TARGET_DIR, RESULT_INO)):
    logging.info(f"Create folder {TARGET_DIR, RESULT_INO}")
    os.makedirs(os.path.join(TARGET_DIR, RESULT_INO))

def create_pin_conn (pin, pintype):
    '''
        Create pin connection during setup in the .ino template

        @param pin (int): The pin number
        @param pintype (string): One of 2, INPUT or OUTPUT
    '''
    logging.info("Writing pin connection...")
    try:
        pintype = pintype.upper()
        if pintype == "INPUT" or pintype == "OUTPUT":
            logging.debug(f"PIN: {pin}, TYPE:{pintype}")
            return f"\tpinMode({pin}, {pintype});\n"
        raise TypeError("Pintype must be either: INPUT or OUTPUT")
    
    except Exception as error:
        logging.error(f"ERROR GENERATING PIN CONNECTION for pin {pin}, {pintype}: "+ str(error))

def define_pin (label, pinnum):
    '''
        Define pin label to pin num on top of .ino file
    '''
    logging.info("DEFINING pin labels...")
    try:
        logging.debug(f"PIN: {label}, PIN#:{pinnum}")
        return f"#define {label} {int(pinnum)}\n"
    except Exception as error:
        logging.error(f"FAILED TO DFINE PIN {label} WITH PIN NUM{pinnum}:" + str(error))


def create_glob_output (sensor, var, datatype, def_val = "NULL"):
    '''
        Create a global variable to store all sensor measurement result.
    '''
    logging.info("CREATING GLOB VAR FOR SENSOR OUTPUT...")
    try:
        if isinstance(def_val, bool):
            def_val = str(def_val).lower()
        if def_val == None:
            def_val = "NULL"
        logging.debug(f"{datatype} {sensor}_{var} = {def_val}")
        return f"{datatype} {sensor}_{var} = {def_val};\n"

    except Exception as error:
        logging.error(f"ERROR GENERATING VARIABLE {var} WITH DATATYPE {datatype}: " + str(error))

def include_lib (libname):
    '''
        Create 'include' statements for external libraries if required by sensor
        Include .h in the name
        IS NOT RESPONSIBLE OF ACTUALLY INSTALLING THE LIB
    '''
    logging.info("WRITING EXTERNAL LIBRARIES FOR SENSOR...")
    try:
        logging.debug(f"#include \"{libname}\"")
        if "h" in libname.split(".")[-1]:
            return f"#include \"{libname}\"\n"
        raise Exception("Libname invalid. Must end with '.h'")

    except Exception as error:
        logging.error(f"ERROR CREATING INCLUDE STATEMENT {libname}: " + str(error))

def create_getter_template (sensor, var, pins, datatype):
    logging.info("CREATING SENSOR READING GETTER...")
    try:
        prompt = ""

        prompt += f"{datatype} get{sensor}_{var} ("
        for pin in pins:
            lab = pin["label"]
            prompt += f"int {lab},"
        prompt = prompt[:-1] + ") {\n\t//MODIFY HERE FOR YOUR SENSOR\n}\n"
        logging.debug(prompt)
        return prompt

    except Exception as error:
        logging.error(f"FAILED TO CREATE TEMPLATE GETTER FOR SENSOR {sensor} WITH VAR {var}:" + str(error))

def create_sender_template (sensor, fieldname, varname, datatype):
    logging.info("CREATING TB SENDER FUNCTIONS...")
    try:
        prompt = ""

        prompt += f"void send{sensor}_{varname} ({datatype} {sensor}_{varname}) "
        prompt += "{\n"
        prompt += f"\ttb.sendTelemetryData(\"{varname}\", {sensor}_{varname});\n"
        prompt += "}"
        logging.debug(prompt)
        return prompt

    except Exception as error:
        logging.error(f"FAILED TO CREATE TEMPLATE SENDER FOR SENSOR {sensor} WITH VAR {varname}:" + str(error))

def create_sender_caller (sensors_dict):
    logging.info("CALLING TB SENDER FUNCTIONS...")
    prompt = ""
    for sensor in sensors_dict:
        prompt += f"\tsend{sensor}_{sensors_dict[sensor]['output']['fieldname']}({sensor}_{sensors_dict[sensor]['output']['fieldname']});\n"

    logging.debug(prompt)
    return prompt


def create_getter_caller (sensors_dict):
    logging.info("CALLING SENSOR READING GETTER FUNCTIONS...")
    prompt = ""
    logging.debug(sensors_dict)
    for sensor,sensor_dict in sensors_dict.items():
        logging.debug(f"{sensor} : {sensor_dict}")
        
        prompt += f"\t{sensor}_{sensor_dict['output']['fieldname']} = get{sensor}_{sensor_dict['output']['fieldname']}("
        for pin in sensor_dict["pin"]:
            if pin["isIO"]:
                lab = pin["label"]
                prompt += f"{lab},"

        prompt = prompt[:-1] + ");\n"

    logging.debug(prompt)
    return prompt


def write_to_getter_h (data):
    logging.info("WRITING TO getter.h...")
    src = open(f"./templates/getter.h", "r")
    tar = open(os.path.join(TARGET_DIR, RESULT_INO, "getter.h"), "w")
    

    for lines in src.readlines():
        lines = lines.strip("\n")
        lines = lines.strip()
        
        if lines == "//START EXT LIB":
            for sensor in data:
                ext_lib = data[sensor]["ext_lib"]

                if len(ext_lib) > 0:
                    for ext in ext_lib:
                        tar.write(include_lib(ext))


        elif lines == "//START TEMPLATE":
            for sensor in data:
                sensor_dict = data[sensor]
                logging.debug(f"{sensor} : {sensor_dict}")
                    
                logging.debug(f"WRITING FOR {sensor}, {sensor_dict['output']['fieldname']}")
                tar.write(create_getter_template (
                    sensor, 
                    sensor_dict["output"]["fieldname"], 
                    sensor_dict["pin"], 
                    sensor_dict["output"]["datatype"]
                    ) + "\n"
                )

        else:
            tar.write(lines + "\n")

    tar.close()
    src.close()


def write_to_sender_h (data):
    logging.info("WRITING TO sender.h...")
    logging.info(f"TARGET LOC: {os.path.join(TARGET_DIR, RESULT_INO,'sender.h')}")
    src = open(f"./templates/sender.h", "r")
    tar = open(os.path.join(TARGET_DIR, RESULT_INO, "sender.h"), "w")

    for lines in src.readlines():
        lines = lines.strip("\n")
        lines = lines.strip()
        if lines == "//START TEMPLATE":
            for sensor in data:
                sensor_dict = data[sensor]
                logging.debug(f"WRITING FOR {sensor}, {sensor_dict['output']['fieldname']}")
                tar.write(create_sender_template (
                    sensor,
                    sensor_dict["output"]["fieldname"],
                    sensor_dict["output"]["fieldname"],  
                    sensor_dict["output"]["datatype"]
                    ) + "\n"
                )
        else:
            tar.write(lines + "\n")

    tar.close()
    src.close()


if not os.path.exists(f"{SENSOR_JSON_LOC}/{SENSOR_JSON}"):
    logging.error(f'{SENSOR_JSON_LOC}/{SENSOR_JSON} NOT FOUND')

src = open(f"./templates/credentials.h", "r")
tar = open(os.path.join(TARGET_DIR, RESULT_INO, "credentials.h"), "w")
for line in src.readlines():
    tar.write(line)

src.close()
tar.close()

with open(F"{SENSOR_JSON_LOC}/{SENSOR_JSON}") as file:
    inputTemplate = json.load(file)
    for MainSensorName in inputTemplate["MicroController"]["MainSensorMap"].keys():
        data = inputTemplate["MicroController"]["MainSensorMap"][MainSensorName]
        logging.debug('DATA:')
        logging.debug(data)
        
        write_to_getter_h (data)
        write_to_sender_h (data)


        tar = open(os.path.join(TARGET_DIR, RESULT_INO, f"{RESULT_INO}.ino"), "w")
        src = open(f"./templates/ESP32_template/ESP32_template.ino", "r")
        
        for line in src.readlines():
            c_line = line.strip("\n")
            c_line = line.strip()
            logging.debug(line)
            #start writing external library for sensor
            if c_line == "//GET EXT LIB":
                for sensor in data:
                    ext_lib = data[sensor]["ext_lib"]

                    if len(ext_lib) > 0:
                        for ext in ext_lib:
                            tar.write(include_lib(ext))

            #start writing global vars
            elif c_line == "//TEMPLATE GLOBAL":
                for sensor in data:
                    for var in data[sensor]["output"]:
                        tar.write(create_glob_output (
                            sensor, 
                            var, 
                            data[sensor]["output"]["datatype"], 
                            data[sensor]["output"]["defval"] 
                            )
                        )

            #start defining pin labels
            elif c_line == "//DEFINE PIN LABEL":
                for sensor in data:
                    for pin in data[sensor]["pin"]:
                        if pin["isIO"]:
                            label = pin["label"]
                            pinnum =  pin["pinnum"]

                            tar.write(define_pin (label, pinnum))

            #start writing pin setup
            elif c_line == "//TEMPLATE PIN":
                for sensor in data:
                    for pin in data[sensor]["pin"]:
                        if pin["isIO"]:
                            label = pin["label"]
                            pintype =  pin["pintype"]

                        tar.write(create_pin_conn (label, pintype))

            #start writing getter caller
            elif c_line == "//TEMPLATE GET":
                '''
                for sensor in data:
                    sensor_dict = data[sensor]

                    tar.write(create_getter_caller(sensor_dict))
                
                '''
                tar.write(create_getter_caller(data))
                

            #start writing sender caller
            elif c_line == "//TEMPLATE TELEMETRY":
                '''
                for sensor in data:
                    sensor_dict = data[sensor]

                    tar.write(create_sender_caller(sensor_dict))
                '''
                tar.write(create_sender_caller(data))

            else:
                tar.write(line)



        src.close()
        tar.close()


        



