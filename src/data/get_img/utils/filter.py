# encoding: utf-8
import os
import h3
import streetview

def h3_filter(path_in, path_out, level):
    '''
    根据h3筛选数据
    :param path_in: 输入的panorama id数据路径
    :param path_out: 输出路径
    :param level:
    :return:
    '''
    with open(path_in, 'r') as f:
        h3_xy = {}
        for line in f:
            line_arr = line.split(',')
            lat = float(line_arr[1])
            lon = float(line_arr[2])
            h3_id = h3.geo_to_h3(lat, lon, level)

            if h3_id in h3_xy.keys():
                if line_arr[-2] != 'None':
                    h3_xy[h3_id] = line
                else:
                    continue
            else:
                h3_xy[h3_id] = line

    with open(path_out, 'w') as fw:
        for k, v in h3_xy.items():
            fw.write(k+","+v)



if __name__ == '__main__':
    dir_in = 'PID_RAW'
    dir_out = 'Pid_f'
    level = 12

    for name in os.listdir(dir_in):
        path_in = os.path.join(dir_in, name)
        path_out = os.path.join(dir_out, name)

        h3_filter(path_in, path_out, level)

        # break