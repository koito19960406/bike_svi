# encoding: utf-8
# author: zhaotianhong
# contact: zhaoteanhong@gmail.com
# file: my_task.py
# time: 2021/10/16 9:13

import os
import random
import time
from src.data.get_img.utils.imtool import ImageTool
import datetime
import pandas as pd



def read_pids(path_pid):
    pid_df = pd.read_csv(path_pid)
    # get unique pids as a list
    pids = pid_df.iloc[:,0].unique().tolist()
    return pids


def save_mata(meta_path, meta_data):
    with open(meta_path, 'a+') as fw:
        for m in meta_data:
            pid = m["panoid"]
            lat = m["lat"]
            lon = m["lon"]
            try:
                year = m["year"]
                month = m["month"]
            except:
                year = "None"
                month = "None"
            fw.write('%s,%s,%s,%s,%s\n' % (pid, lat, lon, year, month))


def check_already(img_path, all_panoids):
    name_r, all_panoids_f = set(), []
    for name in os.listdir(img_path):
        name_r.add(name.split(".")[0])

    for pid in all_panoids:
        if pid not in name_r:
            all_panoids_f.append(pid)
    return all_panoids_f


def get_ua(path=r'UserAgent.csv'):
    UA = []
    with open(path, 'r') as f:
        for line in f:
            ua = {"user_agent": line.strip()}
            UA.append(ua)
    return UA


def get_nthreads_pid(panoids, nthreads):
    # Output path for the images

    all_pid, panos = [], []
    for i in range(len(panoids)):
        if i % nthreads != 0 or i == 0:
            panos.append(panoids[i])
        else:
            all_pid.append(panos)
            panos = []
    return all_pid


def log_write(log_path, pids):
    with open(log_path, 'a+') as fw:
        for pid in pids:
            fw.write(pid+'\n')


def main(UA, path_pid, dir_save, log_path, nthreads):
    # Import tool
    tool = ImageTool()
    # Horizontal Google Street View tiles
    # zoom 3: (8, 4); zoom 5: (26, 13) zoom 2: (4, 2) zoom 1: (2, 1);4:(8,16)
    zoom = 2
    h_tiles = 4  # 26
    v_tiles = 2  # 13
    cropped = False
    full = True

    panoids = read_pids(path_pid)
    panoids_rest = check_already(dir_save, panoids)

    # random.shuffle(panoids_rest)
    task_pids, errors, img_num = [], 0, 0

    for i in range(len(panoids_rest)):
        if i%nthreads != 0 or i == 0:
            task_pids.append(panoids_rest[i])
        else:
            UAs = random.sample(UA, nthreads)
            try:
                tool.dwl_multiple(task_pids, task_pids, nthreads, zoom, v_tiles, h_tiles, dir_save, UAs, cropped, full)
                img_num += nthreads
                print(datetime.datetime.now(), "Task:", i, "/ ", len(panoids_rest),"got:",img_num, "errors:", errors, dir_save)

            except Exception as e:
                print(e)
                time.sleep(random.randint(1, 5)*0.1)
                errors += nthreads
                log_write(log_path, task_pids)
            task_pids = []


if __name__ == '__main__':

    dir_save = r'src/data/get_img/img'
    dir_pid = r'src/data/get_img/pids'
    ua_path = r'src/data/get_img/utils/UserAgent.csv'
    log_dir = r'src/data/get_img/logging'
    # Number of threads
    nthreads = 5

    for name in os.listdir(dir_pid):
    
        path_pid = os.path.join(dir_pid, name)
        log_path = os.path.join(log_dir,name.replace('.csv','_error.csv'))
        dir_save_c = os.path.join(dir_save, name[:-4])
        if not os.path.exists(dir_save_c):
            os.mkdir(dir_save_c)
        UA = get_ua(path=ua_path)
        main(UA, path_pid, dir_save_c,log_path, nthreads)
