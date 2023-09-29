import argparse
from subprocess import PIPE
import time

import psutil


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("cmd", type=str)
    args = parser.parse_args()

    mems = [] # in MiB
    cpus = []

    proc = psutil.Popen(args.cmd.split(), stdout=PIPE)

    while proc.is_running() and proc.status() != psutil.STATUS_ZOMBIE:
        mem = proc.memory_info().rss / (2**20)
        cpu = proc.cpu_percent()

        mems.append(mem)
        cpus.append(cpu)

        time.sleep(0.1)
        
    
    print(f"Peak memory consumption: {max(mems)} MiB")
    print(f"Peak CPU utilization: {max(cpus)} %")

    with open("memory_consumption.txt", "w") as f:
        f.writelines([f"{x}\n" for x in mems])

    with open("cpu_utilization.txt", "w") as f:
        f.writelines([f"{x}\n" for x in cpus])


if __name__ == "__main__":
    main()
