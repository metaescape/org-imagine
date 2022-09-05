#!/usr/bin/env python3
import argparse
import os
import time
import subprocess
import hashlib
import pandas as pd
import re
from collections import Counter
import matplotlib.pyplot as plt
import calmap
import warnings
warnings.filterwarnings("ignore")

def parse():
    parser = argparse.ArgumentParser()
    parser.add_argument('-l', '--link', type=str, default='')
    parser.add_argument('-d', '--directory', type=str, default='/tmp')
    parser.add_argument('-s', '--scale', type=int, default=1)
    args = parser.parse_args()
    return args

def get_modified_time(path):
    localtime = time.localtime(os.path.getmtime(path))
    return time.strftime('%Y%m%d%H%M%S', localtime)

def get_args_hash(args):
    return hashlib.md5(str(args).encode(encoding='UTF-8')).hexdigest()

def get_file_name(path):
    name = os.path.splitext(os.path.basename(path))[0]
    return name

def get_target_image_name(args, path):
    filename = get_file_name(path)
    timestamp = get_modified_time(path)
    hashcode = get_args_hash(args)
    return f"{filename}-{timestamp}-{hashcode[-5:]}"

def extract_timestamp_to_img(path, scale, imgpath=''):
    time_reg = r"(?<=\[)(\d{4}-\d{2}-\d{2} [A-Z][a-z]{2})( \d{2}:\d{2})?(?=\])"
    times = []
    with open(path, "r") as f:
        for line in f.readlines():
            timestamp = re.findall(time_reg, line)
            for t in timestamp:
                times.append(pd.Timestamp(t[0]))

    ctimes = Counter(times)
    plt.figure(figsize=(8*scale, 2*scale))
    days = list(zip(*dict(ctimes).items()))
    events = pd.Series(days[1], index=days[0])
    a = calmap.yearplot(events)
    fig = a.get_figure()
    fig.savefig(imgpath)
    return imgpath

def get_path(link):
    """
    pdf:~/manual/DL_en.pdf::790++0.00
    return /home/user/manual/DL_en.pdf
    """
    fullpath = link
    if link[:5] == "file:" or link[:4] == "pdf:":
        # get ~/manual/DL_en.pdf::790++0.00
        fullpath = link.split(":", 1)[1] 

    # get (~/manual/DL_en.pdf, 790)
    if "::" in fullpath:
        filepath, loc = fullpath.split("::")
    else:
        filepath = fullpath
    path = os.path.expanduser(filepath)
    return path


if __name__ == "__main__":
    args = parse()
    link = args.link.strip()
    path = get_path(link)
    image_name = get_target_image_name(args, path)
    folder = os.path.expanduser(args.directory)
    image_path = os.path.join(folder, image_name+".png")
    scale = float(args.scale)
    print(extract_timestamp_to_img(path, scale, image_path), end="")
