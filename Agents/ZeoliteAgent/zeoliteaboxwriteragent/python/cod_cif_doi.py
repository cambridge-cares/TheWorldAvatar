
import os
import logging
import json

# logging.basicConfig(level=logging.WARNING)
logging.basicConfig(level=logging.ERROR)


def get_cif_files(dir_name, ext=".cif"):
    """
    Get a list of files with given extension in a nested directory structure.

    """
    logging.info(" Start get_cif_files()...")
    if not os.path.isdir(dir_name):
        logging.error(" '%s' is not a directory in addDir()", dir_name)
        return None

    files = []
    for f in os.listdir(dir_name):
        path = os.path.join(dir_name, f)
        # print( f, path )
        if os.path.isdir(path):
            logging.info(" Checking a sub-dir '%s'.", path)
            files += get_cif_files(path)

        elif os.path.isfile(path):
            _, extension = os.path.splitext(path)
            if ext == extension.lower():
                files.append(path)
            else:
                logging.warning(" File '%s' is not .cif extension," +
                                " I skip it.", path)
        else:
            logging.warning(" Not a file, not a dir: '%s'. ", path)

    # logging.info("Detected the following files: ")
    # logging.info(files)
    # for f in files:
    #     pass

    return files


def get_cif_doi(file_path):
    """ Find and return the DOI value from a .cif file (input argument).

    """
    doi = None

    with open(file_path, "r", encoding="utf-8") as f:
        lines = f.readlines()
        for i_line, line in enumerate(lines):
            if line.find("_journal_paper_doi") >= 0:
                words = line.split()
                if len(words) != 2:
                    doi = lines[i_line + 1]
                    if not doi.strip().strip("'").startswith("10."):
                        logging.error(" Expecting a value after" +
                                      " _journal_paper_doi line %s\n" +
                                      " trying to use next line: %s",
                                      file_path, doi.strip())
                elif words[0] != "_journal_paper_doi":
                    logging.error(" Something may be wrong, expecting" +
                                  " _journal_paper_doi %s", file_path)
                else:
                    doi = words[1]
                break

    if doi is None:
        logging.warning(" File at '%s' does not have a doi entry.", file_path)
    else:
        doi = doi.strip().strip("'")

    return doi


def read_all_doi(files):
    """ Read doi from all the files from the input list.
    Return two items: a dictionary with detectec doi,
                      a list with cifs without doi.
    """
    doi_dict = {}
    no_doi = []

    count_missing_doi = 0
    for f in files:
        if not os.path.isfile(f):
            no_doi.append(f)
            continue

        doi = get_cif_doi(f)
        if doi is None:
            count_missing_doi += 1
            no_doi.append(f)
        else:
            if doi in doi_dict:
                # logging.error(" Conflicting doi value: doi = %s in" +
                #              " %s and %s.", k, f, doi_dict[k])
                doi_dict[doi].append(f)
            else:
                doi_dict[doi] = [f]

    print("doi is not specified in", count_missing_doi, "cifs (out of",
          len(files), "total).",
          round(count_missing_doi * 100./len(files), 1), "%.")

    return doi_dict, no_doi


def merge_list_together(file_list, path_out=""):
    """ Similar to standard linux 'cat' function, plus remove empty strings.
    Combile lists from several files.
    The output file also does not contain empty lines.
    If 'path_out' is not specified, no file is written.
    Return a list containing the combination of the content of inputs.
    """
    full_list = []
    for file in file_list:
        if not os.path.isfile(file):
            logging.error(" Missing file '%s'.", file)
            continue
        with open(file, "r", encoding="utf-8") as fp_in:
            for line in fp_in:
                short = line.strip()
                if len(short) > 0:
                    full_list.append(short)

    if "" != path_out:
        with open(path_out, "w", encoding="utf-8") as fp_out:
            for line in full_list:
                if len(full_list) > 0:
                    fp_out.write("\n")
                fp_out.write(line)

    return full_list

def merge_db_together(db_files, path_out=""):
    """ Function combines several databases into a single file.
    The total COD database is big, requires around 5 hours to scan,
    so it is loaded by main directories and saved in individual jsons.
    This function can merge them and save to a new database file.
    """
    db_dict = {}
    for file in db_files:
        with open(file, encoding="utf-8") as fp_in:
            tmp = json.load(fp_in)
            for k in tmp:
                if k in db_dict:
                    # logging.error(" Conflicting doi value: doi = %s in" +
                    #               " %s and %s.", k, tmp[k], db_dict[k])
                    #db_dict[k].append(tmp[k])
                    db_dict[k] += tmp[k]
                else:
                    db_dict[k] = tmp[k]

    if "" != path_out:
        with open(path_out, "w", encoding="utf-8") as fp_out:
            json.dump(db_dict, fp_out)

    # print("total number of DOIs in DB =", len(db_dict))
    return db_dict

if __name__ == "__main__":
    cod_path = os.path.join("C:\\", "users", "prut01", "cod_data")

    dir_arr = [os.path.join(cod_path, "cif", "1"),
               os.path.join(cod_path, "cif", "2"),
               os.path.join(cod_path, "cif", "3"),
               os.path.join(cod_path, "cif", "4"),
               os.path.join(cod_path, "cif", "5"),
               os.path.join(cod_path, "cif", "6"),
               os.path.join(cod_path, "cif", "7"),
               os.path.join(cod_path, "cif", "8"),
               os.path.join(cod_path, "cif", "9")]
    # files = get_cif_files( os.path.join(cod_path, "cif", "1") )
    # files = get_cif_files( os.path.join(cod_path, "cif", "1" ) )

    files_out = []
    files_no_doi = []
    for i_files, dir_in in enumerate(dir_arr):

        cif_files = get_cif_files(dir_in, ext=".cif")

        print("Total", len(cif_files), ".cif files in dir", dir_in)

        doi_db, doi_no = read_all_doi(cif_files)

        file_out = "cod_doi_" + str(i_files+1) + ".json"
        files_out.append(file_out)

        with open(file_out, "w", encoding="utf-8") as fp_output:
            json.dump(doi_db, fp_output)

        file_out = "no_doi_" + str(i_files+1) + ".txt"
        files_no_doi.append(file_out)
        with open(file_out, "w", encoding="utf-8") as fp_output:
            for no_doi_path in doi_no:
                fp_output.write(no_doi_path + "\n")

    merge_db_together(files_out, "cod_doi_cif.json")
    merge_list_together(files_no_doi, "cod_no_doi.txt")
