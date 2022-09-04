#!/usr/bin/env python3
import argparse
import os
import time
import subprocess
import hashlib
import shutil

def parse():
    parser = argparse.ArgumentParser()
    parser.add_argument('-l', '--link', type=str, default='')
    parser.add_argument('-p', '--page', type=str, default='-1')
    parser.add_argument('-d', '--directory', type=str, default='/tmp')
    parser.add_argument('-s', '--scale', type=int, default=0)
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

def convert_pdf_page_to_img(path, page, scale, imgpath=''):
    if shutil.which("pdftoppm"):
        imgpath_sans = os.path.splitext(image_path)[0]
        command = f"pdftoppm -png -f {page} -scale-to {scale} -singlefile {path} {imgpath_sans}".split()
        process = subprocess.Popen(command, stdin = subprocess.PIPE,
                                   universal_newlines=True, text=True,
                                   stdout = subprocess.PIPE)
        process.wait()

        if process.returncode != 0 or not os.path.exists(imgpath):
            raise Exception(process.returncode)
    else:
        import fitz 
        page = page - 1
        doc = fitz.open(path)
        pix = doc[page].get_pixmap()  # render page to an image
        pix.save(imgpath) 
    return imgpath

def get_path_and_page(link, page):
    """
    pdf:~/manual/DL_en.pdf::790++0.00
    return (/home/user/manual/DL_en.pdf, 790)
    """
    assert link[:5] == "file:" or link[:4] == "pdf:"

    # get ~/manual/DL_en.pdf::790++0.00
    fullpath = link.split(":", 1)[1] 

    # get (~/manual/DL_en.pdf, 790)
    if "::" in fullpath:
        filepath, loc = fullpath.split("::")
        if page == "-1":
            page = loc.split("++")[0]
    else:
        filepath = fullpath
    path = os.path.expanduser(filepath)
    return path, int(page)


if __name__ == "__main__":
    args = parse()
    link = args.link.strip()
    path, page = get_path_and_page(link, args.page)
    image_name = get_target_image_name(args, path)
    folder = os.path.expanduser(args.directory)
    image_path = os.path.join(folder, image_name+".png")
    scale = args.scale
    print(convert_pdf_page_to_img(path, page, scale, image_path), end="")
