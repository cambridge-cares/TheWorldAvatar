from concurrent.futures import ThreadPoolExecutor, as_completed
from pathlib import Path
import argparse
import time
import sys
import os

HELP_FILE = Path.cwd() / ".first_run_done"

def display_help():
    print(
        f"""
        NOTE: Please run this script in the same directory as the molovol executable.

        Args:
        -r: Small probe radius (default is 1.2)
        -g: Grid resolution (default is 0.5)
        -r2: Large probe radius (default is 3)
        -fs: XYZ structure file or folder (default is ./inputfile)
        -do: Directory for outputs (default is ./outputs_<current_epoch_time>, will be created if it doesn't exist)
        -n: Number of concurrent processes (default is 2)

        Press Enter to continue, or any other key to exit...
        """
    )

def get_xyz_files(path):
    xyz_files = []
    path = Path(path)
    if path.is_file():
        if path.suffix == '.xyz':
            xyz_files.append(path)
    elif path.is_dir():
        for file in path.rglob('*.xyz'):
            xyz_files.append(file)    
    return xyz_files

def run_subprocess(args):
    os.system(f"molovol -r {args.r} -g {args.grid} -r2 {args.r2} -fs {args.fs} -xr -do {args.do} -q -o none")
    # -o inputfile,radius_small,radius_large,resolution,formula,vol_core_s,vol_shell_s

def main():
    if not HELP_FILE.exists():
        display_help()
        user_input = input()
        if user_input != "":
            print("Exiting the script...")
            sys.exit(0)
        else:
            print("Continuing with the script...")
        HELP_FILE.touch()

    parser = argparse.ArgumentParser(description="Parallel Execution Script")
    parser.add_argument('-r', type=float, default=1.2, help="Small probe radius")
    parser.add_argument('-g', '--grid', type=float, default=0.5, help="Grid resolution")
    parser.add_argument('-r2', type=float, default=3, help="Large probe radius")
    parser.add_argument('-fs', type=str, default='./inputfile', help="XYZ structure file or folder")
    parser.add_argument('-do', type=str, default=f'outputs_{int(time.time())}', help="Directory for outputs")
    parser.add_argument('-n', '--num-processes', type=int, default=2, help="Number of concurrent processes (default is 2)")
    args = parser.parse_args()

    if not os.path.exists(args.do):
        os.makedirs(args.do)
    else:
        if not os.path.isdir(args.do):
            raise Exception(f'Please provide a valid directory for output files, received: {args.do}')
    xyzs = get_xyz_files(args.fs)

    tasks = []
    for xyz in xyzs:
        task = argparse.Namespace(r=args.r, grid=args.grid, r2=args.r2, fs=xyz, do=args.do)
        tasks.append(task)

    with ThreadPoolExecutor(max_workers=args.num_processes) as executor:
        futures = [executor.submit(run_subprocess, task) for task in tasks]

        # Wait for all processes to complete
        for future in as_completed(futures):
            output = future.result()
            print(output)

if __name__ == '__main__':
    main()
