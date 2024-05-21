
import os
import re
import sys

import Dans_Diffraction as dif
import matplotlib.pyplot as plt

def filter_angle(peaks, fr, to):
    output = []
    for peak in peaks:
        if peak[3] >= fr and peak[3] <= to:
            output.append(peak)
    return output

def filter_intensity(peaks, fr, to):
    output = []
    for peak in peaks:
        if peak[4] >= fr and peak[4] <= to:
            output.append(peak)
    return output

def intensity_rescale(peaks):
    output = []
    values = [x[4] for x in peaks]
    max_val = max(values)

    for peak in peaks:
        peak[4] = peak[4] * 100./max_val
        output.append(peak)

    return output

def get_powder_xrd(filename, twotheta_min, twotheta_max, inten_min):
    xtl = dif.Crystal(filename)
    #print(xtl) # print Crystal structure parameters

    twotheta, inten, reflections = xtl.Scatter.powder(units='twotheta')

    peaks = []
    for line in reflections:
        peak = [int(line[0]), int(line[1]), int(line[2]), line[3], line[4]]
        peaks.append(peak)

    peaks = filter_angle(peaks, twotheta_min, twotheta_max)
    peaks = intensity_rescale(peaks)
    peaks = filter_intensity(peaks, inten_min, 100)
    #print("Number of peaks:", len(peaks))
    return peaks

def save_xrd(path, peaks):
    directory = os.path.dirname(path)
    try:
        os.makedirs(directory)
        #print(f"Created missing folders: {directory}")
    except FileExistsError:
        #print(f"Folders already exist: {directory}")
        pass

    with open(path, "w", encoding="utf-8") as fp:
        for peak in peaks:
            if len(peak) < 5:
                print("Invalid line", peak, "in", path)
                continue
            fp.write( "\t".join([str(peak[0]), str(peak[1]),
                                 str(peak[2]), "d", "M",
                                 str(round(peak[3],4)),
                                 str(round(peak[4],4))]) + "\n")

def get_file_list(parentfolder, path, ext):
    file_list = []
    fullpath = os.path.join(parentfolder, path)
    ext_low = ext.lower()
    if os.path.isdir(fullpath):
        items = os.listdir(fullpath)

        for item in items:
            newpath = os.path.join(path, item)
            file_list += get_file_list(parentfolder, newpath, ext)

    elif os.path.isfile(fullpath):
        if fullpath.lower().endswith(ext_low):
            file_list.append(path)

    else:
        print("Unknown item (file/dir)", fullpath)

    return file_list


def make_jobs(folder_in, folder_out, job_size=1000):
    #job_size = 10

    cifs = []
    for s in ["1", "2", "3", "4", "5", "6", "7", "8", "9"]:
    #for s in ["1"]:
        cifs += get_file_list(os.path.join(folder_in), os.path.join(s), ".cif")

    todo = []
    for cif in cifs:
        path_out = os.path.join(folder_out, cif)
        if not os.path.isfile(path_out):
            todo.append(cif)

    #print("Not completed:", len(todo))
    #for i in range(10):
    #    print("     ", todo[i])

    job_id = 0
    chunks = []
    for i_cif in range(0, len(todo), job_size):
        i_max = min(len(todo), i_cif + job_size)
        chunks.append((i_cif, i_max))
    #print(chunks)

    for ic, chunk in enumerate(chunks):
        job_name = os.path.join("jobs", str(ic) + ".txt")
        with open(job_name, "w", encoding="utf-8") as fp:
            for i in range(chunk[0], chunk[1]):
                fp.write(folder_in + "\t" + todo[i] + "\n")

    print("Created jobs:", len(chunks), "contain", len(todo), "cifs")

def load_next_job():
    output = []
    jobs = os.listdir("jobs")
    #print(jobs)
    job = jobs[0]
    print("Found job:", job)
    if job.endswith(".txt"):
        path = os.path.join("jobs", job)
        with open(path, encoding="utf-8") as fp:
            lines = fp.readlines()
        os.remove(path)

        for line in lines:
            words = line.split()
            output.append(words[1])
            #print(words[1])
    else:
        print("Expect extension txt, got file", job)

    return output


if __name__ == "__main__":

    if False:
        cifs = get_file_list(os.path.join(".."), os.path.join("cod"), ".cif")
        cifs = get_file_list(os.path.join("..", "cod"), os.path.join(""), ".cif")
        print("Found cif files:", cifs)
        print("Found cif files:", len(cifs))

    if False:
        filename = os.path.join("..", "cod", "9012685.cif")
        peaks = get_powder_xrd(filename, 5, 70, 10)
        for p in peaks:
            print(p)
        print("Total number of peaks:", len(peaks))

        # Plot Powder pattern:
        #xtl = dif.Crystal(filename)
        #xtl.Plot.simulate_powder(energy_kev=8)
        #plt.show()

    cifs = []

    FOLDER_IN = os.path.join("C:\\", "Users", "PRUT01", "COD_DATA", "cif")
    FOLDER_OUT = "peaks"

    if len(sys.argv) > 1:
        if sys.argv[1].strip().lower() == "--make-jobs":
            make_jobs(FOLDER_IN, FOLDER_OUT)
        else:
            print(f"Unknown argument '{sys.argv[1]}', expect '--make-jobs'.")
            1/0

    while True:

        cifs = load_next_job()
        if len(cifs) == 0:
            print("No jobs found. Exiting now!")
            break

        log_file = "log_x.txt"

        #print("Jobs starts with:", cifs[:20])

        """
    subfold = None
    if len(sys.argv) > 1:
        subfold = sys.argv[1]

    if subfold == "1":
        cifs += get_file_list(os.path.join(FOLDER_IN), os.path.join("1"), ".cif")
        log_file = "log_1.txt"

    if subfold == "2":
        cifs += get_file_list(os.path.join(FOLDER_IN), os.path.join("2"), ".cif")
        log_file = "log_2.txt"

    if subfold == "3":
        cifs += get_file_list(os.path.join(FOLDER_IN), os.path.join("3"), ".cif")
        log_file = "log_3.txt"

    if subfold == "4":
        cifs += get_file_list(os.path.join(FOLDER_IN), os.path.join("4"), ".cif")
        log_file = "log_4.txt"

    if subfold == "5":
        cifs += get_file_list(os.path.join(FOLDER_IN), os.path.join("5"), ".cif")
        log_file = "log_5.txt"

    if subfold == "6":
        cifs += get_file_list(os.path.join(FOLDER_IN), os.path.join("6"), ".cif")
        log_file = "log_6.txt"

    if subfold == "7":
        log_file = "log_7.txt"
        cifs += get_file_list(os.path.join(FOLDER_IN), os.path.join("7"), ".cif")

    if subfold == "8":
        log_file = "log_8.txt"
        cifs += get_file_list(os.path.join(FOLDER_IN), os.path.join("8"), ".cif")

    if subfold == "9":
        cifs += get_file_list(os.path.join(FOLDER_IN), os.path.join("9"), ".cif")
        log_file = "log_9.txt"

    if False:
        FOLDER_IN = os.path.join("..", "code-release", "ontozeolite", "crystal", "data")
        cifs += get_file_list(os.path.join(FOLDER_IN), os.path.join("cifdir"), ".cif")
        log_file = "log.txt"
    """

        #print("Found cif files:", cifs[:20])
        print("Found cif files:", len(cifs))

        for i_cif, cif in enumerate(cifs):
            if i_cif % 1 == 0:
                print("Starting cif file:", i_cif+1, "of", len(cifs), ":", cif)
            file_in = os.path.join(FOLDER_IN, cif)
            file_out = os.path.join(FOLDER_OUT, cif)
            try:
                if not os.path.isfile(file_out):
                    peaks = get_powder_xrd(file_in, 5, 70, 5)
                    save_xrd(file_out, peaks)
            except:
                print("Failed to read file:", cif)
                with open(log_file, "a", encoding="utf-8") as fp:
                    fp.write("Failed to read " + cif + "\n")

