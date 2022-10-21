###Libraries###
import httplib2
from pprint import pformat

import re
import pandas as pd
import sys
from datetime import datetime, timedelta


####Time###
def period_to_time(Period):
    #Returns hh:mm for each Period
    halfHour = {
        1: "00:00",
        2: "00:30",
        3: "01:00",
        4: "01:30",
        5: "02:00",
        6: "02:30",
        7: "03:00",
        8: "03:30",
        9: "04:00",
        10: "04:30",
        11: "05:00",
        12: "05:30",
        13: "06:00",
        14: "06:30",
        15: "07:00",
        16: "07:30",
        17: "08:00",
        18: "08:30",
        19: "09:00",
        20: "09:30",
        21: "10:00",
        22: "10:30",
        23: "11:00",
        24: "11:30",
        25: "12:00",
        26: "12:30",
        27: "13:00",
        28: "13:30",
        29: "14:00",
        30: "14:30",
        31: "15:00",
        32: "15:30",
        33: "16:00",
        34: "16:30",
        35: "17:00",
        36: "17:30",
        37: "18:00",
        38: "18:30",
        39: "19:00",
        40: "19:30",
        41: "20:00",
        42: "20:30",
        43: "21:00",
        44: "21:30",
        45: "22:00",
        46: "22:30",
        47: "23:00",
        48: "23:30"
    }
    return halfHour[int(Period)]


def time_to_period(Time):
    #Returns Period for each half hourly hh:mm (counter to period_to_time)
    halfHourTime = {
        "00:00": 1,
        "00:30": 2,
        "01:00": 3,
        "01:30": 4,
        "02:00": 5,
        "02:30": 6,
        "03:00": 7,
        "03:30": 8,
        "04:00": 9,
        "04:30": 10,
        "05:00": 11,
        "05:30": 12,
        "06:00": 13,
        "06:30": 14,
        "07:00": 15,
        "07:30": 16,
        "08:00": 17,
        "08:30": 18,
        "09:00": 19,
        "09:30": 20,
        "10:00": 21,
        "10:30": 22,
        "11:00": 23,
        "11:30": 24,
        "12:00": 25,
        "12:30": 26,
        "13:00": 27,
        "13:30": 28,
        "14:00": 29,
        "14:30": 30,
        "15:00": 31,
        "15:30": 32,
        "16:00": 33,
        "16:30": 34,
        "17:00": 35,
        "17:30": 36,
        "18:00": 37,
        "18:30": 38,
        "19:00": 39,
        "19:30": 40,
        "20:00": 41,
        "20:30": 42,
        "21:00": 43,
        "21:30": 44,
        "22:00": 45,
        "22:30": 46,
        "23:00": 47,
        "23:30": 48
    }
    return halfHourTime[str(Time)]


def week_ago():
    #Get a week ago (returns str for year, month, day: 
    weekAgo = datetime.utcnow() - timedelta(days=7)
    #print(weekAgoFormat)
    return str(weekAgo.year), str(weekAgo.month), str(weekAgo.day)


def day_over_a_week_ago():
    #Get a week ago + 1 day (8 days) for a slightly longer time ago (returns str for year, month, day: 
    weekAgo = datetime.utcnow() - timedelta(days=8)
    #print(weekAgoFormat)
    return str(weekAgo.year), str(weekAgo.month), str(weekAgo.day)


def two_weeks_ago():
    #Get a two weeks ago (14 days) here, returns str for year, month, day: 
    weekAgo = datetime.utcnow() - timedelta(days=14)
    #print(weekAgoFormat)
    return str(weekAgo.year), str(weekAgo.month), str(weekAgo.day)


def str0_2(value):
    #Converts the day or month int to a string of length 2. Thus, 12 -> "12", and 1 -> "01", the leading 0 is important.
    #This function thus performs a similar role as str(), but also can add the 0 at the start, and is applied to length 2 instances.
    #Must use for day or month, but use for period is optional, as length 1 and 2 is accepted by the API format the period, but not the month or day. 
    value = str(value)
    if len(value) == 1:
        value = "0"+value
    return value


def format_time(Year, Month, Day, Period):
    #Formats the time
    #EG. "2021-11-26T09:30:00Z"
    return str(Year) + '-' + str0_2(Month) + '-' + str0_2(Day) + "T" + period_to_time(Period) + ":00Z"


def split_time(timestamp):
    #Splits up a timestamp into year, month, day, period.
    #Counter to format_time.
    timestamp = str(timestamp)
    timestamp = timestamp.replace(":00Z", "")
    timestamp = timestamp.replace("T", "-")
    timestamp = timestamp.split("-")
    return timestamp[0], str0_2(timestamp[1]), str0_2(timestamp[2]), time_to_period(timestamp[3])


######
#Test/Example
#print(split_time(format_time(2022, 10, 4, 1)))
######

