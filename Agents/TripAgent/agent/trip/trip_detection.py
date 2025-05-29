"""
trip_detection.py
Author: Janelle Berscheid, July 2018

Copied over from FenlandTrajectoryAgent by KFL and removed some redundant codes

# MODIFIED BY JIYING CHEN in 2025
# Focused on improving compatibility and maintainability:
# - Replaced deprecated package use (e.g. pd.np.nan) 
# - Adjusted relative imports and modular structure for TWA stack

Parameters:

CELL_SIZE: The square cell size of the location grid, in meters.
KERNEL_BANDWIDTH: The uncertainty of the GPS reading, in meters. Used for KDE calculations.
MAX_RASTER_SIZE: If trip detection fails with simply "Killed" printed to the console,
                you probably ran out of memory (most likely in hotspot extraction).
                Try reducing the maximum raster size. If this is found to be
                exceeded, the cell size will automatically be increased to meet this
                requirement, up to the point it becomes incompatible with the kernel bandwidth.
INTERPOLATE_MAX_DELAY: The time threshold for starting interpolation, in seconds.
INTERPOLATE_MAX_DROP_TIME: The maximum time between points after which not to interpolate, in seconds.
INTERPOLATE_MAX_DISTANCE: The maximum distance between points, in meters, to interpolate over.
MIN_VISIT_DURATION: Minimum duration of a dwell, in seconds, for it to be considered a valid visit
MIN_VISIT_TIMESPAN: Minimum time, in seconds, that must elapse between dwells; otherwise they will be merged into a single visit.
MIN_LOOP_DURATION: Minimum duration of a looping path, in second, to keep it and consider it a valid path.
DOWNSAMPLE_FREQUENCY: The frequency to downsample GPS signals to: one point every ?? seconds.
MIN_HOTSPOT_KEEP_DURATION: Minimum duration of a hotspot, in seconds, to keep it and consider it a hotspot.
BUFFER_WEIGHT: Array for convolving the GPS points in temporal order to weigh the kernel values to identify hotspots.
BUFFER_SNAP: Array for convolving the GPS points in temporal order to determine which should be snapped to hotspots.
MIN_PATH_KEEP_DURATION: (new parameter) The minimum duration of a path to keep it, in seconds.
KERNEL_HOTSPOT_THRESHOLD: (new parameter) A threshold above which kernel values may be considered on hotspots.
                        The original code set it as 1.0 (standard deviations above the mean zone kernel value), but this
                        port uses a different kernel and performs better with lower values.
                        Warning: this value may be tuned to fit the sample data too closely. Test with other participants
                        to determine the best default value for this parameter.
SIGNIFICANT_GAP_LENGTH: (new parameter) Minimum length, in seconds, of a GPS data gap for it to be labeled as a period
                        of missing data. Gaps with missing data cannot be labeled as either trips or visits.
                        In the final results, some gaps may be folded into temporally adjacent visits at the same location
                        to create a single, longer visit if the length of the gap is less than MIN_VISIT_TIMESPAN.

Remaining work:
- Implement smoothing on the gap identification, to discard single observations surrounded by large gaps.
- Test to see if TinyExtentsError will ever actually trigger; if so, port code which handles it, otherwise remove it
- Test to see if the maximum raster size will ever be a problem; if not, remove code which limits it
- Add option to smooth out the detected missing data gaps, combining long adjacent gaps separated by a few observations

Quality of life improvements:
- Verbose mode to save more intermediate steps to disk
- Clean up temporary files when finished, if desired
- Write log file of each run
"""


from agent.trip.utilities import get_euclidean_distance, get_projection
import numpy as np
import pandas as pd
import sys
import os
import logging
from time import perf_counter, strftime
import matplotlib.pyplot as plt
from skimage.segmentation import watershed
from skimage.feature import peak_local_max
from scipy import ndimage as ndi
from KDEpy import FFTKDE
from shapely.geometry import Point, LineString


# Default Parameters (see explanation above)
DEFAULT_PARAMS = {
    "CELL_SIZE": 10,  # meters, for grid
    "KERNEL_BANDWIDTH": 100,  # basically uncertainty of GPS in meters
    # originally, greatest possible number of cells per row/column, default 30000
    "MAX_RASTER_SIZE": 14000*14000,
    "INTERPOLATE_MAX_DELAY": 120,  # seconds
    # seconds, default: 3600 || high value ~ infinity
    "INTERPOLATE_MAX_DROP_TIME": 360000,
    # meters, default: 100 || high value ~ infinity
    "INTERPOLATE_MAX_DISTANCE": 100000,
    "MIN_VISIT_DURATION": 300,  # seconds
    "MIN_VISIT_TIMESPAN": 900,  # 3600,
    "MIN_LOOP_DURATION": 300,
    "DOWNSAMPLE_FREQUENCY": 0,  # default: 5 (0=off)
    "MIN_HOTSPOT_KEEP_DURATION": 150,  # seconds
    "MIN_PATH_KEEP_DURATION": 30,  # NEW parameter! seconds
    "KERNEL_HOTSPOT_THRESHOLD": 1,  # 0.1 NEW parameter! May need to be tuned
    "BUFFER_WEIGHT": np.array([0.1, 0.25, 0.3, 0.25, 0.1]),
    "BUFFER_SNAP": np.array([0.5, 1.0, 1.5, 1.0, 0.5]),
    # NEW parameter! Minimum length of a data gap to be considered a missing data period
    "SIGNIFICANT_GAP_LENGTH": 900
}

# Column headers.
CH_TRIP_INDEX = "Trip index"
CH_VISIT_INDEX = "Visit index"
CH_GAP = "Gap"

##################################
# Utility methods
##################################


def format_gps_data(df, columns, code=None):
    """
    :param df: pandas dataframe containing GPS data to format
    :param columns: a dictionary containing the names of lat, lon, utm_n and utm_e columns in the dataframe.
                    May differ between Ethica and Sensedoc data.
    :param code: the ESPG code of the data to be projected, as a string.
    :return: formatted pandas dataframe
    """
    # Sort rows by time stamp.
    df = df.sort_values(columns["utc_date"])

    # Add UTM projections to DF if it doesn't exist already
    if columns["utm_e"] not in list(df.columns) or columns["utm_n"] not in list(df.columns):
        utm_e, utm_n = get_projection(
            df[columns["lon"]].values, df[columns["lat"]].values, code=code)
        df[columns["utm_e"]] = utm_e
        df[columns["utm_n"]] = utm_n

    return df


def get_data_gaps(df, pid, columns,
                  gap_length=DEFAULT_PARAMS["SIGNIFICANT_GAP_LENGTH"], smooth_gaps=False):
    """
    Utility function for characterizing gaps existing in GPS data prior to trip detection.
    Creates a dataframe containing gaps of non-trivial length, marking their start and stop time, and saves as a temporary
    file to disk.
    :param df: pandas dataframe containing the GPS observations
    :param pid: the participant's ID, as an integer
    :param gap_length: minimum length of a data gap, in seconds, to be considered as a missing data period
    :param smooth_gaps: If True, gaps will be merged if separated by a small number of observations
    :return: None. Writes the gaps to a temporary file, to be loaded during later processing.
    """
    df = df.sort_values(columns["utc_date"])
    df.loc[:, "gap_time"] = ((df[columns["utc_date"]] - df.shift(1)
                             [columns["utc_date"]]) / np.timedelta64(1, 's')).fillna(0)
    gaps = df.loc[df["gap_time"] > 1, (columns["utc_date"], "gap_time")]
    sig_gaps = gaps.loc[gaps["gap_time"] > gap_length].copy()
    sig_gaps.loc[:, "end_time"] = sig_gaps[columns["utc_date"]] - \
        np.timedelta64(1, 's')
    sig_gaps.loc[:, "start_time"] = sig_gaps["end_time"] - \
        pd.to_timedelta(sig_gaps["gap_time"], unit="s")
    sig_gaps = sig_gaps.loc[:, ("start_time", "end_time")]

    if smooth_gaps:
        # TODO: If a very long gap is broken up by just a couple of observations, discard the observations and fold into the gap
        pass

    df = df.drop(["gap_time"], axis=1)
    return sig_gaps


def resize_cells(w, h, cell_size, kernel_bandwidth, max_raster_size):
    """
    Method for resizing the grid cells if the area covered is too
    large to compute with the default cell size. The main limiting
    factor is the availability of sufficient RAM.
    :param w: width of the extents, in metres
    :param h: height of the extents, in metres
    :param cell_size: current width/height of square grid cells, in meters
    :param kernel_bandwidth: width/height of kernel, in meters
    :return: integer value of the new grid cell size, in meters
    """
    raster_size = (w / cell_size) * (h / cell_size)
    """
    BT: another option would be to implement a multiscale/sparse raster
    processing approach and focus on the areas where GPS fixes exist
    """
    if raster_size > max_raster_size:
        # redefine cellsize to stay within limits
        cell_size = int(np.ceil(np.sqrt(w * h / max_raster_size)))

        # check that new cell size is still compatible with the kernel bandwidth
        if kernel_bandwidth / 2.0 >= cell_size:
            print(
                f"Changing raster resolution to {cell_size} m to keep raster size below threshold.")
        else:
            print(
                f"FATAL ERROR: New cell size {cell_size} m is incompatible with kernel bandwidth!")
            sys.exit()
    return cell_size


def downsample_trace(df, columns, frequency=5):
    """
    Takes the original GPS trace, and downsamples it to one reading every [frequency] seconds.
    :param df: GPS points as a pandas dataframe
    :param frequency: integer describing the number of seconds to elapse between readings in the downsampled data
    :return: a new dataframe containing the downsampled data
    """
    dates = df[columns["utc_date"]]
    # Calculate difference between two adjacent samples
    differences = ((dates - dates.shift(1)) / np.timedelta64(1, 's')).fillna(0)

    # compute step between resampled records
    step = int(np.ceil(frequency / np.median(differences)))

    # Extract those that have difference over frequency
    mask = differences > frequency
    keep = df[mask]
    lower_diff = df[~mask]
    lower_diff = lower_diff.iloc[::step]
    downsampled = pd.concat([keep, lower_diff], axis=0)
    downsampled = downsampled.sort_values(
        columns["utc_date"]).reset_index(drop=True)
    return downsampled


def interpolate_over_period(df, columns, frequency, noise=3.0):
    """
    Helper method for interpolate_over_dropped_periods() (see below), which performs actual interpolation
    :param df: pandas dataframe (group??) containing the range of points to iterpolate over
    :param frequency: how frequently to add interpolated points, in seconds
    :param noise: sigma parameter for the random noise distribution, in meters
    :param columns: a dictionary containing the names of lat, lon, utm_n and utm_e columns in the dataframe.
                    May differ between Ethica and Sensedoc data.
    :return: pandas dataframe containing only the new rows
                concatenate these new rows onto your original dataframe
    """

    f_lat, f_lon, f_n, f_e = tuple(
        df[[columns["lat"], columns["lon"], columns["utm_n"], columns["utm_e"]]].iloc[0])
    new_rows = df.copy().set_index(columns["utc_date"]).resample(
        f"{int(frequency)}S").asfreq().replace([0.0], np.nan)
    new_rows = new_rows.reset_index()
    new_rows.at[0, columns["lat"]] = f_lat
    new_rows.at[0, columns["lon"]] = f_lon
    new_rows.at[0, columns["utm_e"]] = f_e
    new_rows.at[0, columns["utm_n"]] = f_n
    new_rows = new_rows.interpolate()

    noise = pd.DataFrame(np.random.normal(
        0, noise, (len(new_rows), 2)), columns=["xnoise", "ynoise"])
    new_rows[columns["utm_n"]] = new_rows[columns["utm_n"]] + noise["ynoise"]
    new_rows[columns["utm_e"]] = new_rows[columns["utm_e"]] + noise["xnoise"]

    new_rows = new_rows[(new_rows[columns["utc_date"]] >= df.iloc[0][columns["utc_date"]]) & (
        new_rows[columns["utc_date"]] <= df.iloc[1][columns["utc_date"]])].copy()

    # TODO: if target dataframe holds both WGS and UTM, convert the columns UTM to WGS and add those (or vice versa)

    return new_rows


def interpolate_over_period_test1(df, columns, frequency, noise=3.0):
    """
    Helper method for interpolate_over_dropped_periods() (see below), which performs actual interpolation
    :param df: pandas dataframe (group??) containing the range of points to iterpolate over
    :param frequency: how frequently to add interpolated points, in seconds
    :param noise: sigma parameter for the random noise distribution, in meters
    :param columns: a dictionary containing the names of lat, lon, utm_n and utm_e columns in the dataframe.
                    May differ between Ethica and Sensedoc data.
    :return: pandas dataframe containing only the new rows
                concatenate these new rows onto your original dataframe
    --
    Interpolation done along the road network, shortest path chosen based on GPS track, 
    linear sampling along path.
    """
    from .interpolate import interpolate_graph
    from .interpolate import density_sample

    f_lat, f_lon, f_n, f_e = tuple(
        df[[columns["lat"], columns["lon"], columns["utm_n"], columns["utm_e"]]].iloc[0])
    new_rows = df.copy().set_index(columns["utc_date"]).resample(
        f"{int(frequency)}S").asfreq().replace([0.0], np.nan)
    new_rows = new_rows.reset_index()

    from_pt = Point((df.iloc[0][columns["utm_e"]],
                    df.iloc[0][columns["utm_n"]]))
    to_pt = Point((df.iloc[-1][columns["utm_e"]],
                  df.iloc[-1][columns["utm_n"]]))
    n_samples = len(new_rows.index)

    ipath = interpolate_graph(
        from_pt, to_pt, r'test_data/utm_roadnet.bin', impedance='weighted_length')
    if ipath.is_empty:
        ipath = LineString([from_pt, to_pt])
    igps_fix = density_sample(ipath, n_samples, noise=noise)
    igps_coords = map(lambda p: (p.x, p.y), igps_fix)
    igps_x, igps_y = zip(*igps_coords)
    new_rows.loc[:, columns["utm_e"]] = igps_x
    new_rows.loc[:, columns["utm_n"]] = igps_y

    noise = pd.DataFrame(np.random.normal(
        0, noise, (len(new_rows), 2)), columns=["xnoise", "ynoise"])
    new_rows[columns["utm_n"]] = new_rows[columns["utm_n"]] + noise["ynoise"]
    new_rows[columns["utm_e"]] = new_rows[columns["utm_e"]] + noise["xnoise"]

    new_rows = new_rows[(new_rows[columns["utc_date"]] >= df.iloc[0][columns["utc_date"]]) & (
        new_rows[columns["utc_date"]] <= df.iloc[1][columns["utc_date"]])].copy()

    return new_rows


def interpolate_over_period_test2(df, columns, frequency, noise=3.0):
    """
    Helper method for interpolate_over_dropped_periods() (see below), which performs actual interpolation
    :param df: pandas dataframe (group??) containing the range of points to iterpolate over
    :param frequency: how frequently to add interpolated points, in seconds
    :param noise: sigma parameter for the random noise distribution, in meters
    :param columns: a dictionary containing the names of lat, lon, utm_n and utm_e columns in the dataframe.
                    May differ between Ethica and Sensedoc data.
    :return: pandas dataframe containing only the new rows
                concatenate these new rows onto your original dataframe
    --
    Interpolation done along the road network, shortest path chosen based on GPS track, 
    density sampling along path using KDE50 raster.
    """
    from .interpolate import interpolate_graph
    from .interpolate import density_sample

    f_lat, f_lon, f_n, f_e = tuple(
        df[[columns["lat"], columns["lon"], columns["utm_n"], columns["utm_e"]]].iloc[0])
    new_rows = df.copy().set_index(columns["utc_date"]).resample(
        f"{int(frequency)}S").asfreq().replace([0.0], np.nan)
    new_rows = new_rows.reset_index()

    from_pt = Point((df.iloc[0][columns["utm_e"]],
                    df.iloc[0][columns["utm_n"]]))
    to_pt = Point((df.iloc[-1][columns["utm_e"]],
                  df.iloc[-1][columns["utm_n"]]))
    n_samples = len(new_rows.index)

    ipath = interpolate_graph(
        from_pt, to_pt, r'test_data/utm_roadnet.bin', impedance='weighted_length')
    if ipath.is_empty:
        logging.warning(
            'Interpolation returned with no valid path, defaulting to straight line')
        ipath = LineString([from_pt, to_pt])
    igps_fix = density_sample(
        ipath, n_samples, gps_dsty_rst=r'test_data/sd_gps_asap_KDE50c10.tif', noise=noise)
    igps_coords = map(lambda p: (p.x, p.y), igps_fix)
    igps_x, igps_y = zip(*igps_coords)
    new_rows.loc[:, columns["utm_e"]] = igps_x
    new_rows.loc[:, columns["utm_n"]] = igps_y

    noise = pd.DataFrame(np.random.normal(
        0, noise, (len(new_rows), 2)), columns=["xnoise", "ynoise"])
    new_rows[columns["utm_n"]] = new_rows[columns["utm_n"]] + noise["ynoise"]
    new_rows[columns["utm_e"]] = new_rows[columns["utm_e"]] + noise["xnoise"]

    new_rows = new_rows[(new_rows[columns["utc_date"]] >= df.iloc[0][columns["utc_date"]]) & (
        new_rows[columns["utc_date"]] <= df.iloc[1][columns["utc_date"]])].copy()

    return new_rows


def interpolate_over_period_test3(df, columns, frequency, noise=3.0, min_dist=50):
    """
    Helper method for interpolate_over_dropped_periods() (see below), which performs actual interpolation
    :param df: pandas dataframe (group??) containing the range of points to iterpolate over
    :param frequency: how frequently to add interpolated points, in seconds
    :param noise: sigma parameter for the random noise distribution, in meters
    :param columns: a dictionary containing the names of lat, lon, utm_n and utm_e columns in the dataframe.
                    May differ between Ethica and Sensedoc data.
    :return: pandas dataframe containing only the new rows
                concatenate these new rows onto your original dataframe
    --
    Interpolation done along the road network, shortest path chosen based on GPS track, 
    density sampling along path using KDE50 raster.
    """
    from .interpolate import interpolate_graph
    from .interpolate import density_sample

    f_lat, f_lon, f_n, f_e = tuple(
        df[[columns["lat"], columns["lon"], columns["utm_n"], columns["utm_e"]]].iloc[0])
    new_rows = df.copy().set_index(columns["utc_date"]).resample(
        f"{int(frequency)}S").asfreq().replace([0.0], np.nan)
    new_rows = new_rows.reset_index()

    from_pt = Point((df.iloc[0][columns["utm_e"]],
                    df.iloc[0][columns["utm_n"]]))
    to_pt = Point((df.iloc[-1][columns["utm_e"]],
                  df.iloc[-1][columns["utm_n"]]))
    n_samples = len(new_rows.index)

    ipath = interpolate_graph(
        from_pt, to_pt, r'test_data/utm_roadnet.bin', impedance='weighted_length')
    if ipath.is_empty:
        logging.warning(
            'Interpolation returned with no valid path, defaulting to straight line')
        ipath = LineString([from_pt, to_pt])
    igps_fix = density_sample(
        ipath, n_samples, gps_dsty_rst=r'test_data/sd_gps_asap_KDE50c10.tif', noise=noise, min_dist=min_dist)
    igps_coords = map(lambda p: (p.x, p.y), igps_fix)
    igps_x, igps_y = zip(*igps_coords)
    new_rows.loc[:, columns["utm_e"]] = igps_x
    new_rows.loc[:, columns["utm_n"]] = igps_y

    noise = pd.DataFrame(np.random.normal(
        0, noise, (len(new_rows), 2)), columns=["xnoise", "ynoise"])
    new_rows[columns["utm_n"]] = new_rows[columns["utm_n"]] + noise["ynoise"]
    new_rows[columns["utm_e"]] = new_rows[columns["utm_e"]] + noise["xnoise"]

    new_rows = new_rows[(new_rows[columns["utc_date"]] >= df.iloc[0][columns["utc_date"]]) & (
        new_rows[columns["utc_date"]] <= df.iloc[1][columns["utc_date"]])].copy()

    return new_rows


def interpolate_over_dropped_periods(df, columns, frequency, max_delay=120,
                                     max_drop_time=3600, max_distance=100,
                                     interpolate_helper_func=interpolate_over_period):
    """
    Interpolate GPS points for periods with large gaps
    Criteria:
        - if delay with previous point over max_delay: start interpolation
        - if dropped period is over max_drop_time: do not interpolate
            (unless the distance from last valid point is below max_distance)
    :param df: pandas dataframe of GPS points
    :param frequency: how frequently to add interpolated points, in seconds
    :param max_delay: time threshold for starting interpolation, in seconds
    :param max_drop_time: maximum time after which to not interpolate, in seconds
    :param max_distance: maximum distance for interpolation between points, in meters
    :param columns: a dictionary containing the names of lat, lon, utm_n and utm_e columns in the dataframe.
                May differ between Ethica and Sensedoc data.
    :param interpolate_helper_func:
    :return: new dataframe containing the interpolated points
    """
    # get time delta between all adjacent pairs of points (in seconds)
    df = df.sort_values(columns["utc_date"]).reset_index(drop=True)
    differences = ((df[columns["utc_date"]].shift(-1) - df[columns["utc_date"]]
                    ).shift(1) / np.timedelta64(1, 's')).fillna(0.0)

    # get Euclidean distance delta between all adjacent pairs of points
    df["prev_e"] = df.shift(1)[columns["utm_e"]]
    df["prev_n"] = df.shift(1)[columns["utm_n"]]
    distance_delta = df[[columns["utm_e"], columns["utm_n"],
                         "prev_e", "prev_n"]].apply(lambda x: get_euclidean_distance((x["prev_e"], x["prev_n"]),
                                                                                     (x[columns["utm_e"]], x[columns["utm_n"]])), axis=1)
    df = df.drop(["prev_e", "prev_n"], axis=1)

    # distance_delta = pd.Series([0.0]).append(distance_delta, ignore_index=True)
    distance_delta = pd.Series(distance_delta).fillna(0.0)

    # find all rows where time delta is greater than the max_delay parameter AND
    # (the delay is less than max_drop_time OR distance delta is less than max_distance)
    delay_mask = differences > max_delay
    drop_distance_mask = ((differences < max_drop_time) |
                          (distance_delta < max_distance))
    df['gap'] = delay_mask & drop_distance_mask

    # interpolate over those matching rows with _interpolate_over_period()
    print("Previous trace length: ", len(df))
    interpolated_df = df.copy()
    for i, row in df.loc[df['gap']].iterrows():
        new_points = interpolate_helper_func(df.loc[i-1:i], columns, frequency)
        interpolated_df = pd.concat([interpolated_df, new_points], axis=0)
    df = df.drop(['gap'], axis=1)
    interpolated_df = interpolated_df.drop(['gap'], axis=1)
    interpolated_df = interpolated_df.drop_duplicates([columns["utc_date"]])
    interpolated_df = interpolated_df.sort_values(columns["utc_date"])
    interpolated_df = interpolated_df.reset_index(drop=True)
    print("New trace length after interpolation: ", len(interpolated_df))
    return interpolated_df


def interpolate_over_dropped_periods_median(df, columns, frequency,
                                            max_delay=120, max_drop_time=3600, max_distance=100,
                                            interpolate_helper_func=interpolate_over_period):
    """
    Interpolate GPS points for periods with large gaps
    Criteria:
        - if delay with previous point over max_delay: start interpolation
        - if dropped period is over max_drop_time: do not interpolate
            (unless the distance from last valid point is below max_distance)
    :param df: pandas dataframe of GPS points
    :param frequency: how frequently to add interpolated points, in seconds
    :param max_delay: time threshold for starting interpolation, in seconds
    :param max_drop_time: maximum time after which to not interpolate, in seconds
    :param max_distance: maximum distance for interpolation between points, in meters
    :param columns: a dictionary containing the names of lat, lon, utm_n and utm_e columns in the dataframe.
                May differ between Ethica and Sensedoc data.
    :param interpolate_helper_func:
    :return: new dataframe containing the interpolated points
    """
    # get time delta between all adjacent pairs of points (in seconds)
    df = df.sort_values(columns["utc_date"]).reset_index(drop=True)
    differences = ((df[columns["utc_date"]].shift(-1) - df[columns["utc_date"]]
                    ).shift(1) / np.timedelta64(1, 's')).fillna(0.0)

    # compute median point over moving window and get Euclidian distance between those points
    bins = 5  # length of window over which median position is computed
    med_prev = df.rolling(bins).median().rename(
        columns={columns["utm_e"]: 'prev_e', columns["utm_n"]: 'prev_n'})
    med_next = df.shift(-bins + 1).rolling(bins).median()
    med_df = pd.concat([med_prev, med_next], axis=1, sort=False)
    med_distance_delta = med_df[[columns["utm_e"], columns["utm_n"],
                                 "prev_e", "prev_n"]].apply(lambda x: get_euclidean_distance((x["prev_e"], x["prev_n"]),
                                                                                             (x[columns["utm_e"]], x[columns["utm_n"]])), axis=1)
    med_distance_delta = pd.Series(med_distance_delta).fillna(0.0)

    # find all rows where time delta is greater than the max_delay parameter AND
    # (the delay is less than max_drop_time OR distance delta is less than max_distance)
    delay_mask = differences > max_delay
    drop_distance_mask = ((differences < max_drop_time) |
                          (med_distance_delta < max_distance))
    df['gap'] = delay_mask & drop_distance_mask

    # interpolate over those matching rows with _interpolate_over_period()
    print("Previous trace length: ", len(df))
    interpolated_df = df.copy()
    interpolated_df['interpolated'] = False
    for i, row in df.loc[df['gap']].iterrows():
        prev_median = df.loc[i-bins:i-1].median()
        next_median = df.loc[i:i+bins-1].median()
        bounds = df.loc[i-1:i].copy()
        for c in [columns["utm_e"], columns["utm_n"], columns["lat"], columns["lon"]]:
            bounds.loc[i-1, c] = prev_median.loc[c]
            bounds.loc[i, c] = next_median.loc[c]
        new_points = interpolate_helper_func(bounds, columns, frequency)
        new_points['interpolated'] = True
        interpolated_df = pd.concat([interpolated_df, new_points], axis=0)
    df = df.drop(['gap'], axis=1)
    interpolated_df = interpolated_df.drop(['gap'], axis=1)
    interpolated_df = interpolated_df.drop_duplicates([columns["utc_date"]])
    interpolated_df = interpolated_df.sort_values(columns["utc_date"])
    interpolated_df = interpolated_df.reset_index(drop=True)
    print("New trace length after interpolation: ", len(interpolated_df))
    return interpolated_df


def grid_loc_to_utm(coord, axis):
    """
    Utility method to convert a grid location to a UTM value
    :param coord: the index value along a given axis, as a floating-point number
    :param axis: numpy array containing the axis values for each index
    :return: a single value representing the UTM coordinate at that index
    """
    edge1 = int(np.floor(coord))
    edge2 = int(np.ceil(coord))
    if edge1 == edge2:
        return axis[edge1]

    difference = axis[edge2] - axis[edge1]
    return axis[edge1] + np.modf(coord)[0] * difference

##################################
# Algorithm stages
##################################


# Step 1: Extract hotspots from GPS data (may have been resampled, interpolated etc.)

def extract_hotspots(points, x_bounds, y_bounds, kernel_bandwidth, pid,
                     qualifiers="5", cell_size=10, save_kernel: bool = False, plot: bool = False):
    """
    Extract hotspots from GPS track by identifying the local peaks
        on a kernel density surface built from the GPS fixes.
    :param points: a numpy array containing the GPS coordinates
    :param x_bounds: tuple of the minimum/maximum x value
    :param y_bounds: tuple of the minimum/maximum y value
    :param kernel_bandwidth: the kernel uncertainty in meters
    :param pid: the participant's ID, as an integer
    :param qualifiers: a string containing parameter shorthand for the filenames;
                        "i" for interpolation or "" for none, followed by the downsampling frequency
    :param cell_size: square cell size (meters)
    :return: a numpy array containing the kernel values for each GPS point,
             a dataframe with kernel zonal statistics (mean & std)
             a numpy array containing the zone values for each GPS point,
             and a pandas dataframe containing the discovered hotspots.
    """

    print("Initializing the grid")
    # Step 0a: Initialize everything you need
    x_grid = np.arange(x_bounds[0] - cell_size,
                       x_bounds[1] + cell_size, cell_size)
    y_grid = np.arange(y_bounds[0] - cell_size,
                       y_bounds[1] + cell_size, cell_size)
    print(f"Making {len(x_grid)} x {len(y_grid)} grid...")
    x_mesh, y_mesh = np.meshgrid(x_grid, y_grid)
    grid = np.vstack((y_mesh.ravel(), x_mesh.ravel())).T

    # Step 1: Run Kernel Density Estimation
    # ArcPy assumes planar distance and quartic kernel; using biweight (~Quartic) kernel from KDEpy
    # KDEpy kernel bandwidth are expressed as standard-deviation,
    # see https://en.wikipedia.org/wiki/Kernel_(statistics)#Kernel_functions_in_common_use for the
    # characteristics of some common kernels
    kde = FFTKDE(kernel='biweight', bw=np.sqrt(1/7) * kernel_bandwidth)
    print("Fitting the kernel")
    c0 = perf_counter()
    result_kernel = kde.fit(points).evaluate(grid) * len(points)
    print(f'Done: {perf_counter()-c0:.2f}s')

    # Produce 2D view of KDE
    result_k_2d = result_kernel.reshape((len(y_grid), len(x_grid)))
    if save_kernel:
        print("Saving the kernel.")
        np.savetxt(f"{os.getcwd()}/temp/{pid}_temp_kde{str(kernel_bandwidth)}_ds{qualifiers}.asc",
                   np.flip(result_k_2d, 0), delimiter=" ", comments='', fmt='%.18g',
                   header=f'NCOLS {len(x_grid)}\nNROWS {len(y_grid)}\nXLLCENTER {x_grid[0]}\nYLLCENTER {y_grid[0]}\nCELLSIZE {cell_size}\nNODATA_VALUE -9999')
    if plot:
        # Make the plot
        levels = np.linspace(0, result_kernel.max(), 255)
        plt.contourf(x_mesh, y_mesh, result_k_2d,
                     levels=levels, cmap=plt.cm.Reds)
        # plt.show()
        plt.savefig(
            f"{os.getcwd()}/temp/{pid}_temp_kernel_k{str(kernel_bandwidth)}_ds{qualifiers}.png")

    # Locating kernel values for GPS points
    print("Snapping kernel values.")
    y_indices = np.searchsorted(y_grid, points.T[0])
    x_indices = np.searchsorted(x_grid, points.T[1])
    indexes = [y_indices[i] * len(x_grid) + x_indices[i]
               for i in range(len(y_indices))]
    kernel_snapped_values = [result_kernel[x] for x in indexes]

    print("Masking to area of interest")
    # redefining area of interest
    aoi = np.where(result_kernel == 0.0, np.nan, result_kernel)

    # get the mean of the non-zero values (from area of interest)
    mean = np.nanmean(aoi)

    # Step 3: Retain values above average or 1 std, set others to null
    aoi = np.where(result_kernel < mean, 0, result_kernel)
    k_mask = np.where(aoi != 0, 1.0, 0).reshape((len(y_grid), len(x_grid)))
    k_bool = k_mask.astype(bool)

    # Watershed algorithm using skimage:
    # See http://scikit-image.org/docs/dev/auto_examples/segmentation/plot_watershed.html
    # (removed the line about distance since the kernel function operates as distance)
    # Increasing the footprint tends to decrease the number of local maxima

    print("Running watershed.")
    # NB The last version of scikit-image to support the "indices" keyword
    # argument seems to be 0.19.3.
    # In order to migrate to a newer version, one would need to manually
    # convert the returned coordinates to a boolean array:
    # https://stackoverflow.com/questions/75825151/changes-to-peak-local-max-in-skimage-feature-how-do-i-get-the-boolean-array-sha
    # local_maxi = peak_local_max(result_k_2d, indices=False, footprint=np.ones((3, 3)),
    #                             labels=k_bool)

    # markers = ndi.label(local_maxi, structure=np.ones((3, 3)))[0]
    local_maxi_coords = peak_local_max(
        result_k_2d, footprint=np.ones((3, 3)), labels=k_bool)
    local_maxi = np.zeros_like(result_k_2d, dtype=bool)
    if local_maxi_coords.size > 0:
        local_maxi[tuple(local_maxi_coords.T)] = True

    markers = ndi.label(local_maxi, structure=np.ones((3, 3)))[0]

    sink_mask = np.where(markers > 0, 1, 0)
    temp_markers = ndi.label(sink_mask, structure=np.ones((3, 3)))[0]

    maximum = np.max(temp_markers)
    centroids = ndi.center_of_mass(
        sink_mask, temp_markers, index=range(1, maximum + 1))

    print("Calculating hotspots.")
    # np.set_printoptions(threshold=np.nan) #BT: wrong call to function, which does not allow NaN parameters)
    if len(np.unique(markers)) == 2:
        hotspots = pd.DataFrame([[1, grid_loc_to_utm(centroids[0][1], x_grid),
                                  grid_loc_to_utm(centroids[0][0], y_grid)]],
                                columns=["hotspot_id", "centroid_x", "centroid_y"])
    else:
        centroids_x = [grid_loc_to_utm(x[1], x_grid) for x in centroids]
        centroids_y = [grid_loc_to_utm(x[0], y_grid) for x in centroids]
        hotspots = pd.DataFrame(
            [], columns=["hotspot_id", "centroid_x", "centroid_y"])
        hotspots["hotspot_id"] = range(1, np.max(markers) + 1)
        hotspots["centroid_x"] = centroids_x
        hotspots["centroid_y"] = centroids_y

    # Perform watershed using sinks as markers
    labels = watershed(-result_k_2d, markers, mask=k_bool)
    zone_labels = [labels.reshape(
        (len(y_grid) * len(x_grid),))[x] for x in indexes]

    # Compute z-kernel for each zone
    labels_1d = labels.reshape((1, labels.size)).ravel()
    kdf = pd.DataFrame(data=np.vstack((labels_1d, result_kernel)).T,
                       columns=['zone', 'kernel']).astype({'zone': 'int32'})
    zone_groups = kdf.groupby("zone")
    aggregate_stats = zone_groups.agg({"kernel": ["mean", "std"]})
    aggregate_stats.columns = ["mean", "std"]

    pd.set_option('display.max_rows', len(hotspots))
    # print("Located hotspots:")
    # print(hotspots)

    return kernel_snapped_values, aggregate_stats, zone_labels, hotspots


# Step 2: Calculate normal and modified kernel

def get_norm_modified_kernel(df, aggregate_stats, buffer_weight, pid, kernel_bandwidth=100, qualifiers="5"):
    """
    Adds the time-convolved modified kernel values to the GPS data, as well as the normalized values for both.
    :param df: a pandas dataframe containing the GPS data (plus zone and kernel values from extract_hotspots())
    :param buffer_weight: a 1D numpy array containing the weight window for convolution
    :param pid: the participant's ID, as an integer
    :param kernel_bandwidth: the kernel uncertainty in meters
    :param qualifiers: a string containing parameter shorthand for the filenames
                        "i" for interpolation or "" for none, followed by the downsampling frequency
    :return: a pandas datafram containing the input data plus modified kernel and normalized kernel values.
    """
    # calculate convolution of kernel values with weight (=modified kernel) --uses weight matrix and buffer values above
    kernel = df["kernel"].values
    zones = df["zone"].values
    modified_kernel = np.convolve(kernel, buffer_weight, "same")

    # calculate normalized values of kernels based on the zone/basin
    zone_mean = np.array([-9999 * len(zones)], float)
    zone_std = np.array([-9999 * len(zones)], float)
    norm_kernel = np.array([0 * len(kernel)], float)
    for index, row in aggregate_stats.iterrows():
        mean = row["mean"]
        std = row["std"]
        zone_mean = np.where(zones == index, mean, zone_mean)
        zone_std = np.where(zones == index, std, zone_std)

    # calculate convolution of kernel values with weight (=modified kernel) --uses weight matrix and buffer values above
    kernel = df["kernel"].values
    zones = df["zone"].values
    modified_kernel = np.convolve(kernel, buffer_weight, "same")

    # calculate normalized values of kernels based on the zone/basin
    try:
        norm_kernel = np.where(zone_mean == -9999, -9999,
                               (kernel - zone_mean) / zone_std)
    except RuntimeWarning:
        print("Divide by zero error.")
        norm_kernel = np.where(zone_std == 0, zone_mean, norm_kernel)
    norm_modified_kernel = np.where(
        zone_mean == -9999, -9999, (modified_kernel - zone_mean) / zone_std)

    df["norm_kernel"] = norm_kernel
    df["modfied_kernel"] = modified_kernel
    df["norm_modified_kernel"] = norm_modified_kernel
    if len(df[df["norm_kernel"] == -9999]) > 0 or len(df[df["norm_modified_kernel"] == -9999]) > 0:
        print("WARNING: Some kernel values are still set to null values.")

    return df

# Step 3: Link GPS data to discovered hotspots


def link_gps_to_hotspots(df, buffer_snap, kernel_hotspot_threshold):
    """
    Method for finding GPS points closest to hotspots, to determine which should be "snapped" to those hotspots.
    :param df: a pandas dataframe containing the GPS data, zone, and normalized kernel values
    :param buffer_snap: a 1D numpy array containing the weight window for convolution
    :param kernel_hotspot_threshold: a positive float value, greater than 0, used to find significant kernel values
    :return: a pandas dataframe with the GPS data, plus a binary "snap" value indicating a hotspot match
    """
    # removing 0 ("no zone") from the list of valid zones)
    zones = np.setdiff1d(
        df["zone"].value_counts().index.values, np.zeros((1,)))

    snap_dict = dict.fromkeys(np.unique(zones))
    for z in zones:
        snap_dict[z] = np.zeros(len(df), dtype=int)

    # Find points with kernel values higher than the threshold and assume they belong to hotspots.
    for i, row in df.iterrows():
        if row["zone"] != 0:
            if ((row["norm_kernel"] >= kernel_hotspot_threshold) or
                    (row["norm_modified_kernel"] >= kernel_hotspot_threshold)):
                snap_dict[row["zone"]][i] = 1

    # Convolve the snap condition temporally for each zone
    for z in zones:
        snap_dict[z] = np.convolve(snap_dict[z], buffer_snap, "same")

    zone_array = df["zone"].values
    snap_val = np.zeros(len(df))
    for k, snap in snap_dict.items():
        snap_val = np.where(zone_array == k, snap, snap_val)

    # give a mask of the points where the smoothed kernel values are above 0
    snap_val = np.where(snap_val > 0, 1, 0)

    df["snap_to_hs"] = snap_val

    return df


# Step 4: Refine the detected hotspots by extracting dwell times

def refine_hotspots(df, columns, hotspots, data_gaps, min_keep_duration,
                    gap_length=DEFAULT_PARAMS["SIGNIFICANT_GAP_LENGTH"]):
    """
    Create a list of dwells to find potential visits, find the time spent at each hotspot, and refine list of hotspots
    to include only those meeting the minimum duration threshold.
    :param df: a pandas dataframe containing the GPS data with the "snap" flag from link_gps_to_hotspots()
    :param hotspots: a pandas dataframe containing the hotspots found in extract_hotspots()
                    Contains a hotspot ID and the centroid coordinates.
    :param data_gaps: a pandas dataframe containing the start and stop periods for missing data gaps.
                    Used to create "dummy" points for breaking up visits
    :param min_keep_duration: the number of seconds above which a hotspot must be visited to be considered valid
    :param gap_length: minimum length of a missing data gap, in seconds, to be considered significant
    :return: the filtered pandas dataframe containing hotspot data, with the total duration added, and
                a pandas dataframe containing dwells, with the hotspot ID, start and end times and duration in seconds
    """

    # filter df to only snapped values
    filtered = df[df["snap_to_hs"] > 0].copy()

    # Add fake GPS points to the filtered visits to signify data gaps.
    # These are used to break up visits with long gaps, and are removed later.
    fake_points = data_gaps.copy()
    if not fake_points.empty:
        fake_points.loc[:, columns["utc_date"]
                        ] = fake_points["start_time"] + np.timedelta64(1, 's')
        fake_points.loc[:, "zone"] = -999
        filtered = pd.concat([filtered, fake_points], axis=0, sort=True)

    filtered = filtered.sort_values(columns["utc_date"]).reset_index(drop=True)

    # Identify points as beginnings of new visits by their difference in zone
    # Fill appropriate values for first row to account for shifts
    filtered["diff_visit"] = (
        filtered["zone"] != filtered["zone"].shift(-1)).shift(1)
    if not filtered.empty:
        filtered.loc[0, "diff_visit"] = True

    # A visit must be broken into two if it has a period of missing data in the middle.
    filtered["sig_gap"] = (((filtered[columns["utc_date"]].shift(-1) - filtered[columns["utc_date"]]) /
                            np.timedelta64(1, 's')).fillna(0) > gap_length).shift(1)
    filtered.loc[0, "sig_gap"] = True
    filtered["diff_visit"] = (filtered["diff_visit"] | filtered["sig_gap"])

    # Propagate the start time "downwards" through the results; all points from the same visit will have the same start
    current_start_time = pd.NaT
    for i, row in filtered.iterrows():
        if row['diff_visit']:
            current_start_time = row[columns["utc_date"]]
        filtered.at[i, 'start_time'] = current_start_time

    # Remove the fake data points representing gaps
    filtered = filtered[filtered["zone"] != -999]

    # Get the end time for each dwell by finding the last timestamp for each dwell
    visit_agg = filtered.groupby("start_time")
    stop_times = []
    for a, b in visit_agg:
        b = b.sort_values(columns["utc_date"])
        stop_times.append(b.iloc[-1][columns["utc_date"]])

    # select rows where df["diff_visit"] = True to get a summary of the visit
    dwell_rows = filtered[filtered["diff_visit"]].copy()
    dwell_rows["stop_time"] = stop_times

    # Create dataframe containing dwells
    dwell_rows["duration"] = (
        dwell_rows["stop_time"] - dwell_rows["start_time"]) / np.timedelta64(1, 's')
    dwell_rows = dwell_rows.reset_index(drop=True)
    dwell_rows = dwell_rows[["zone", "start_time", "stop_time", "duration"]]

    # Add the total visit duration (in seconds) to a location in the hotspot data and filter by duration
    # Accounts for shift in hotspot ID and index
    duration_agg = dwell_rows[["zone", "duration"]].groupby("zone").agg("sum")
    last = duration_agg.iloc[-1]["duration"]
    hotspots["total_duration"] = duration_agg["duration"]
    hotspots["total_duration"] = hotspots["total_duration"].shift(-1)
    hotspots.loc[max(duration_agg.index) - 1, "total_duration"] = last
    hotspots = hotspots[~hotspots["total_duration"].isnull()]
    hotspots = hotspots[hotspots["total_duration"] > min_keep_duration]
    return hotspots, dwell_rows


# Step 5: Build visits from the refined hotspot data

def filter_visits(dwell_table, hotspots, min_visit_timespan, min_visit_duration):
    """
    Method for finding visits from the dwell data.
    Group visits to the same place that too close to the previous visit, remove too-short visits
    :param dwell_table: pandas dataframe containing dwell data (start, stop, locationID, duration)
    :param hotspots: a pandas dataframe containing the hotspots found in extract_hotspots()
                    Contains a hotspot ID and the centroid coordinates.
    :param min_visit_timespan: the minimum number of seconds that must elapse between visits to be considered different
    :param min_visit_duration: the minimum number of seconds a dwell must be to be considered a visit
    :return: dataframe containing the visits (columns: start, stop, locationID, duration)
    """
    df = dwell_table.copy().reset_index(drop=True)
    df = df.sort_values("start_time")

    # Columns used to make other calculations; add extra values on edges to account for shift
    df["diff_location"] = (df["zone"] != df["zone"].shift(-1)).shift(1)
    df.loc[0, "diff_location"] = True

    df["time_since_last_visit"] = (
        (df["start_time"].shift(-1) - df["stop_time"]).shift(1)) / np.timedelta64(1, 's')
    df.loc[0, "time_since_last_visit"] = 0.0

    # Do gnarly pandas calculations!
    # Propagate start times "downwards" for visits meeting criteria:
    #   Propagate if the location hasn't changed and if the time falls below the threshold
    last_result = pd.Series([0])
    while not df["start_time"].equals(last_result):
        last_result = df["start_time"].copy()
        df.loc[~((df["time_since_last_visit"] > min_visit_timespan) |
                 (df["diff_location"])), "start_time"] = df["start_time"].shift(1)

    # Aggregate result by start time to get records grouped by visit
    agg_dwells = df.groupby("start_time")

    # Build the list of visits from the aggregated results
    filtered_visits = pd.DataFrame()
    for start, frame in agg_dwells:
        stop = frame["stop_time"].max()
        dur = (stop - start) / np.timedelta64(1, 's')
        loc = frame.iloc[0]["zone"]
        row = pd.DataFrame([[start, stop, loc, dur]], columns=[
                           "start_time", "stop_time", "zone", "duration"])
        filtered_visits = pd.concat(
            [filtered_visits, row], axis=0, ignore_index=True)

    # Remove visits with a duration too short
    filtered_visits = filtered_visits[filtered_visits["duration"]
                                      > min_visit_duration]

    # Remove visits with a zone not corresponding to a hotspot
    filtered_visits = filtered_visits[filtered_visits["zone"].isin(
        hotspots["hotspot_id"].unique())]

    filtered_visits = filtered_visits.reset_index(drop=True)
    return filtered_visits


def build_visits(dwell_table, hotspots, min_visit_timespan, min_visit_duration):
    """
    Takes the identified dwells and turns them into visits.
    :param dwell_table: a pandas dataframe containing the dwells from refine_hotspots()
    :param hotspots: a pandas dataframe containing the hotspots found in extract_hotspots()
                    Contains a hotspot ID and the centroid coordinates.
    :param min_visit_timespan: the minimum number of seconds that must elapse between visits to be considered different
    :param min_visit_duration: the minimum number of seconds a dwell must be to be considered a visit
    :return: a dataframe of visits containing the columns (zone, start_time, end_time, duration)
    """
    # filter visits (only need one call unlike original code)
    visit_table = filter_visits(
        dwell_table, hotspots, min_visit_timespan, min_visit_duration)

    # TODO figure out timestamp conversion if needed
    # Convert local start and end times for each visit to UTC (or vice versa?)

    visit_table = visit_table.sort_values("start_time")
    return visit_table


# Step 6: Update coordinates for snapped points

def update_snapped_points(gps_data, columns, visits, hotspots):
    """
    Update the GPS points to have the same coordinates as the hotspot where they fall within a visit.
    For all filtered visits, finds the gps points within start and end times for that visit and sets that point's
    location to be the hotspot centroid coordinates instead of the point coordinates
    :param gps_data: a pandas dataframe containing the GPS data
    :param visits: a pandas dataframe containing the visits found in build_visits()
    :param hotspots: a pandas dataframe containing the hotspots refined from refine_hotspots()
    :param columns: a dictionary containing the names of lat, lon, utm_n and utm_e columns in the dataframe.
                May differ between Ethica and Sensedoc data.
    :return: the updated GPS dataframe with coordinates snapped to hotspot coordinates where applicable.
    """
    for i in range(len(visits)):
        gps_data.loc[((gps_data[columns["utc_date"]] >= visits.iloc[i]["start_time"]) &
                      (gps_data[columns["utc_date"]] <= visits.iloc[i]["stop_time"])), columns["utm_e"]] = \
            hotspots.loc[(hotspots["hotspot_id"] == visits.iloc[i]["zone"])].reset_index(
                drop=True).iloc[0]["centroid_x"]

        gps_data.loc[((gps_data[columns["utc_date"]] >= visits.iloc[i]["start_time"]) &
                      (gps_data[columns["utc_date"]] <= visits.iloc[i]["stop_time"])), columns["utm_n"]] = \
            hotspots.loc[(hotspots["hotspot_id"] == visits.iloc[i]["zone"])].reset_index(
                drop=True).iloc[0]["centroid_y"]

    return gps_data


# Step 7: Build path bouts from GPS, hotspot and visit data

def detect_path(gps_data, columns, from_time, to_time, max_drop_time,
                min_keep_time, path_df, utm=True):
    """
    Helper method for build_paths() below. Given a set of boundary times, detect GPS points which fall into paths and
    build a path from their coordinates and times.
    :param gps_data: pandas dataframe containing GPS data
    :param from_time: lower filter bounds for GPS data, as a timestamp
    :param to_time: upper filter bounds for GPS data, as a timestamp
    :param max_drop_time: the maximum time in seconds between points, after which to not consider them consecutive
    :param min_keep_time: the minimum duration of a path, in seconds, for it to be valid
    :param path_df: dataframe containing the path data so far
    :param columns: a dictionary containing the names of lat, lon, utm_n and utm_e columns in the dataframe.
                May differ between Ethica and Sensedoc data.
    :param utm: True if the GPS readings are in UTM format, false otherwise.
    :return: the modified path dataframe with any newly discovered paths added
            The path contains its start and end times, duration, its start and end point coordinates, its start and end
            zones, the number of line segments in the path, and the points in the path.
    """

    if utm:
        x_col = columns["utm_e"]
        y_col = columns["utm_n"]
    else:
        x_col = columns["lon"]
        y_col = columns["lat"]

    selected = gps_data.copy()[((gps_data[columns["utc_date"]] >= from_time) &
                                (gps_data[columns["utc_date"]] < to_time))]

    if len(selected) == 0:
        # Don't give warning for very short intervals, because obviously nothing was selected.
        if (to_time - from_time) / np.timedelta64(1, 's') > 10:
            print("No points selected within interval:",
                  from_time, " to ", to_time)
        return path_df

    selected = selected.sort_values(columns["utc_date"])

    # Split the path as needed by temporal gap
    selected["between_point_time"] = ((selected.shift(-1)[columns["utc_date"]] - selected[columns["utc_date"]]).shift(1)) \
        / np.timedelta64(1, 's')
    selected = selected.reset_index(drop=True)
    selected.loc[0, "between_point_time"] = 0

    # Split into multiple dataframes based on long temporal gaps
    selected["path_id"] = (selected["between_point_time"]
                           > max_drop_time).cumsum()
    split_groups = selected.groupby("path_id")

    # For each collection of points, build a unique sequence of points for the path
    for _, g in split_groups:
        points = selected[[x_col, y_col]]
        unique_sequence = points[~((points[x_col] == points[x_col].shift(-1)) &
                                   (points[y_col] == points[y_col].shift(-1)))]
        selected = selected[~((points[x_col] == points[x_col].shift(-1)) &
                              (points[y_col] == points[y_col].shift(-1)))].reset_index(drop=True)
        segments = [tuple(x) for x in unique_sequence.values]
        if len(segments) > 1:
            # TODO: Format the dates for the found paths (local -> UTM or vice versa)
            duration = (selected[columns["utc_date"]].max(
            ) - selected[columns["utc_date"]].min()) / np.timedelta64(1, 's')
            if duration > min_keep_time:
                new_path = pd.DataFrame([[selected[columns["utc_date"]].min(), selected[columns["utc_date"]].max(),
                                          duration, segments[0][0], segments[0][1], selected.iloc[0]["zone"],
                                          segments[-1][0], segments[-1][1], selected.iloc[-1]["zone"],
                                          len(segments) - 1, segments]],
                                        columns=list(path_df.columns))
                path_df = pd.concat([path_df, new_path], axis=0)

    return path_df


def build_paths(gps_data, columns, visits, data_gaps, max_drop_time, min_keep_time):
    """
    Build paths from the detected visits and the GPS data.
    :param gps_data: pandas dataframe containing the GPS data
    :param visits: pandas dataframe containing the visit data
    :param data_gaps: pandas dataframe containing the start and stop times of each missing data gap
    :param max_drop_time: the maximum time in seconds between points, after which to not consider them consecutive
    :param min_keep_time: the minimum duration of a path, in seconds, for it to be valid
    :return: a dataframe containing paths with the columns
                ["start_time", "end_time", "duration", "path_start_x", "path_start_y",
                 "path_end_x", "path_end_y", "num_segments", "segments"]
                The segments are a list of (x, y) coordinate tuples as a string, which can be further parsed when the
                dataframe is returned
    """
    # Initialize empty path structure
    path_cols = ["start_time", "stop_time", "duration", "start_x", "start_y", "start_location_id", "stop_x",
                 "stop_y", "stop_location_id", "num_segments", "segments"]

    paths = pd.DataFrame([], columns=path_cols)

    visits.loc[:, "type"] = "visit"

    # Consider significant gaps in GPS data to break trips, as well as visits
    data_gaps.columns = ["start_time", "stop_time"]
    if not data_gaps.empty:
        data_gaps.loc[:, "type"] = "gap"
        data_gaps.loc[:, "contained"] = False

        # Fold gaps into visits if they are fully contained within a visit
        # Visits were previously merged if they were less than MIN_VISIT_TIMESPAN apart, so gaps detected as significant may
        #  be contained withing these merged visits, if the SIGNIFICANT_GAP_LENGTH is less than MIN_VISIT_TIMESPAN
        for vindex, visit in visits.iterrows():
            data_gaps["contained"] = (((data_gaps["start_time"] > visit["start_time"]) &
                                       (data_gaps["stop_time"] < visit["stop_time"])) |
                                      (data_gaps["contained"]))

        data_gaps = data_gaps[~data_gaps["contained"]]
        data_gaps = data_gaps[["start_time", "stop_time"]]

    stop_events = pd.concat([data_gaps[["start_time", "stop_time"]], visits[[
                            "start_time", "stop_time"]]], axis=0)
    stop_events = stop_events.sort_values("start_time")

    # Loop through stops and find the points in-between stops to be candidate paths.
    from_time = gps_data.iloc[0][columns["utc_date"]]
    for i in range(len(stop_events)):
        to_time = stop_events.iloc[i]["start_time"]
        paths = detect_path(gps_data, columns, from_time,
                            to_time, max_drop_time, min_keep_time, paths)
        # Update reference time
        from_time = stop_events.iloc[i]["stop_time"]

    # After iterating through stops, add the last piece of track which was not previously processed.
    to_time = gps_data.iloc[-1][columns["utc_date"]] + \
        pd.Timedelta(1, unit="s")
    paths = detect_path(gps_data, columns, from_time, to_time,
                        max_drop_time, min_keep_time, paths)

    # Drop duplicated paths (if any).
    paths = paths.drop_duplicates(
        ["start_time", "stop_time"]).reset_index(drop=True)
    return paths

#############################################
# Main method for putting it all together:
#############################################


def make_index(iter, time_series: pd.Series, binary: bool) -> list:
    index_list = []
    first_time = time_series.iat[0]
    last_time = time_series.iat[-1]
    stop_time = first_time - np.timedelta64(1, 's')
    for _, t in time_series.items():
        while t > stop_time:
            try:
                index, row = next(iter)
                index += 1
                start_time = row["start_time"]
                stop_time = row["stop_time"]
            except StopIteration:
                index = 0
                start_time = first_time
                stop_time = last_time
        index_list.append(0 if t < start_time else (
            int(index > 0) if binary else index))
    return index_list


def detect_trips(gps_data, pid, columns, params=DEFAULT_PARAMS,
                 interpolate_helper_func=interpolate_over_period, code=None):
    """
    Method for detecting trips from GPS data.
    Intended for use with only a single participant's data at a time.
    :param gps_data: data frame containing the GPS data to detect trips on.
    :param pid: a string containing the participant's INTERACT ID (or other unique identifier);
                Used when generating temporary files.
    :param params: dictionary containing the parameters for the trip detection, each as described in the documentation.
    :param interpolate_helper_func: function to be used to interpolate GPS track. Function signature as follow:
                interpolate_helper_func(df, columns, frequency, noise). No interpolation if None
    :param columns: a dictionary containing the names of utc_date, lat, lon, utm_n and utm_e columns in the dataframe.
                May differ between Ethica and Sensedoc data.
    :param code: the ESPG code of the data to be projected, as a string. Leave as None if no conversion is needed.
    :return: a pandas dataframe containing the final detected trips,
             a pandas dataframe containing the detected hotspots, and
             a pandas dataframe containing the detected visits.
    """

    # Make temporary scratch folder to store intermediate values and prevent thrashing.
    os.makedirs(os.getcwd() + "/temp", exist_ok=True)

    # Load parameters and initialize needed values
    cell_size = params["CELL_SIZE"]  # meters
    kernel_bandwidth = params["KERNEL_BANDWIDTH"]  # meters
    max_raster_size = params["MAX_RASTER_SIZE"]
    downsample_frequency = params["DOWNSAMPLE_FREQUENCY"]  # seconds
    max_delay = params["INTERPOLATE_MAX_DELAY"]  # seconds
    max_drop_time = params["INTERPOLATE_MAX_DROP_TIME"]  # seconds
    max_distance = params["INTERPOLATE_MAX_DISTANCE"]  # meters
    frequency = downsample_frequency if downsample_frequency else 1
    i_string = ""
    if interpolate_helper_func is not None:
        i_string = "i"
    i_string += str(downsample_frequency)
    min_visit_duration = params["MIN_VISIT_DURATION"]
    min_visit_timespan = params["MIN_VISIT_TIMESPAN"]
    buffer_weight = params["BUFFER_WEIGHT"]
    buffer_snap = params["BUFFER_SNAP"]
    min_hotspot_keep = params["MIN_HOTSPOT_KEEP_DURATION"]
    min_path_keep = params["MIN_PATH_KEEP_DURATION"]
    min_loop_duration = params["MIN_LOOP_DURATION"]
    kernel_hotspot_threshold = params["KERNEL_HOTSPOT_THRESHOLD"]
    utm = True

    if utm:
        x_col = columns["utm_e"]
        y_col = columns["utm_n"]
    else:
        x_col = columns["lon"]
        y_col = columns["lat"]

    # Currently assumes dataframe contains the following columns: (interact_id, utc_date, lat, lon, utm_e, utm_n)
    gps_data = format_gps_data(gps_data, columns, code=code)

    # get grid width/height
    width = np.ceil(gps_data[x_col].max() - gps_data[x_col].min())
    height = np.ceil(gps_data[y_col].max() - gps_data[y_col].min())
    print("Width: ", width)
    print("Height: ", height)
    print(f"Cell size: {cell_size} m")
    print("Kernel_bandwidth", kernel_bandwidth)

    cell_size = resize_cells(width, height, cell_size, kernel_bandwidth,
                             max_raster_size)

    if downsample_frequency != 0:
        print(f"[{strftime('%H:%M:%S')}] Downsampling gps data.")
        gps_data = downsample_trace(gps_data, columns, downsample_frequency)

    if interpolate_helper_func is not None:
        print(f"[{strftime('%H:%M:%S')}] Interpolating over GPS data")
        gps_data = interpolate_over_dropped_periods_median(gps_data, columns,
                                                           frequency, max_delay, max_drop_time, max_distance,
                                                           interpolate_helper_func=interpolate_helper_func)
        gps_data.to_csv(r'temp\gps_interpolated.csv')

    points = gps_data[[y_col, x_col]].ffill().values
    x_bounds = (gps_data[x_col].min(), gps_data[x_col].max())
    y_bounds = (gps_data[y_col].min(), gps_data[y_col].max())

    print(f"[{strftime('%H:%M:%S')}] Extracting hotspots.")
    kernel, zone_stats, zones, hotspots = extract_hotspots(points, x_bounds, y_bounds, kernel_bandwidth=kernel_bandwidth, pid=pid,
                                                           qualifiers=i_string, cell_size=cell_size)

    gps_data["kernel"] = kernel
    gps_data["zone"] = zones

    print(f"[{strftime('%H:%M:%S')}] Normalizing the kernel.")
    gps_data = get_norm_modified_kernel(
        gps_data, zone_stats, buffer_weight, pid, kernel_bandwidth, i_string)
    print(f"[{strftime('%H:%M:%S')}] Linking the GPS points to hotspots.")
    gps_data = link_gps_to_hotspots(
        gps_data, buffer_snap, kernel_hotspot_threshold)
    print(f"[{strftime('%H:%M:%S')}] Characterizing existing gaps in GPS data.")
    gaps = get_data_gaps(gps_data, pid, columns)
    print(f"[{strftime('%H:%M:%S')}] Refining hotspots and finding dwells.")
    hotspots, dwells = refine_hotspots(
        gps_data, columns, hotspots, gaps, min_hotspot_keep)
    print(f"[{strftime('%H:%M:%S')}] Building visits from dwells.")
    visit_table = build_visits(
        dwells, hotspots, min_visit_timespan, min_visit_duration)
    print(f"[{strftime('%H:%M:%S')}] Updating GPS values for snapped points.")
    gps_data = update_snapped_points(gps_data, columns, visit_table, hotspots)
    print(f"[{strftime('%H:%M:%S')}] Detecting the final bouts.")
    final_bouts = build_paths(
        gps_data, columns, visit_table, gaps, max_drop_time, min_path_keep)
    print(f"[{strftime('%H:%M:%S')}] Building incidents table.")
    incidents_table = build_incident_table(
        final_bouts, hotspots, visit_table, gaps, pid)

    print(f"[{strftime('%H:%M:%S')}] Building trip, visit, and gap look-ups...")
    gps_data[CH_TRIP_INDEX] = make_index(
        final_bouts.iterrows(), gps_data["utc_date"], False)
    gps_data[CH_VISIT_INDEX] = make_index(
        visit_table.iterrows(), gps_data["utc_date"], False)
    gps_data[CH_GAP] = make_index(gaps.iterrows(), gps_data["utc_date"], True)

    return gps_data, incidents_table, width, height, cell_size


def build_incident_table(paths, hotspots, visits, gaps, pid):
    incident_columns = ["start_time", "stop_time", "start_location_id", "stop_location_id",
                        "start_x", "start_y", "stop_x", "stop_y", "type"]

    paths["type"] = "trip"
    visits["type"] = "visit"

    visits["start_location_id"] = visits["zone"]
    visits["stop_location_id"] = visits["zone"]
    visits["start_x"] = visits["start_location_id"].map(lambda x: hotspots.loc[hotspots["hotspot_id"] == x,
                                                                               "centroid_x"].iloc[0])
    visits["start_y"] = visits["start_location_id"].map(lambda y: hotspots.loc[hotspots["hotspot_id"] == y,
                                                                               "centroid_y"].iloc[0])
    visits["stop_x"] = visits["start_x"]
    visits["stop_y"] = visits["start_y"]

    # Format gaps to fit in the table
    if not gaps.empty:
        gaps = gaps[~gaps["contained"]]
        gaps = gaps[["start_time", "stop_time"]]
        gaps["type"] = "gap"
        gaps["start_location_id"] = np.nan
        gaps["stop_location_id"] = np.nan
        gaps["start_x"] = np.nan
        gaps["start_y"] = np.nan
        gaps["stop_x"] = np.nan
        gaps["stop_y"] = np.nan
        incidents = pd.concat(
            [paths[incident_columns], visits[incident_columns], gaps[incident_columns]], axis=0)
    else:
        incidents = pd.concat(
            [paths[incident_columns], visits[incident_columns]], axis=0)

    incidents.loc[:, "interact_id"] = pid  # TODO: change interact_id
    incidents = incidents[["interact_id"] + incident_columns]
    incidents = incidents.sort_values("start_time")
    return incidents
