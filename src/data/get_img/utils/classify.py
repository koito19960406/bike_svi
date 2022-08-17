# encoding: utf-8
# author: zhaotianhong
# contact: zhaoteanhong@gmail.com
# file: classify.py
# time: 2021/11/6 14:03

import os

path_in = r'E:\GSVI_sequentially\pids.csv'
dir_out = r'year'


data_dict = {}
with open(path_in, 'r') as f:
    for line in f:
        line_arr = line.split(',')
        if len(line_arr) != 6:
            continue
        year = line_arr[4]
        if year not in data_dict.keys():
            data_dict[year] = [line]
        else:
            data_dict[year].append(line)
        # break

for year, lines in data_dict.items():
    path_out = os.path.join(dir_out, year+'.csv')
    with open(path_out, 'w') as fw:
        for line in lines:
            fw.write(line)
