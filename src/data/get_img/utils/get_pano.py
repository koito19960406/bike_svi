# encoding: utf-8
# author: zhaotianhong
# contact: zhaoteanhong@gmail.com
# file: get_pano.py
# time: 2021/10/16 21:11


import os
import h3
import streetview
import threading
from threading import Thread
import time


def read_h3_xy(path):
    '''
    h3中心点，最好是10
    :param path: xy 是二和第三列
    :return:
    '''
    xys = []
    with open(path, 'r') as f:
        f.__next__()
        for line in f:
            line_arr = line[:-1].split(',')
            xy = [line_arr[1], line_arr[0]]
            xys.append(xy)
    return xys


def point_around(xy):
    '''
    获取一个点周边pano_id
    :param xy:!! 特别注意，经度纬度的顺序
    :return:
    '''

    info = streetview.panoids(xy[0], xy[1])

    return info




def save_mata(path, meta_data):
    '''
    追加保存mata数据信息
    :param meta_path:
    :param meta_data:
    :return:
    '''
    with open(path, 'a+') as fw:
        for ms in meta_data:
            for m in ms:
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
            h3_xy[h3_id] = line
    with open(path_out, 'w') as fw:
        for k, v in h3_xy.items():
            fw.write(v)



def get_all_pano(xy, ALL_DATA):
    '''
    :param xy_list: 所有需要索引的xy
    :param meta_path: 保存mata数据的路径，追加写入方式
    :return:
    '''
    try:
        meta_data = point_around(xy)
        # out_meta = []
        # for panoid in meta_data:
        #     out_meta.append(panoid)
        ALL_DATA.append(meta_data)

    except Exception as e:
        print(e)

        # get_all_pano(xy_list, panoids_got, meta_path)

        # break


def run(xy_list, pid_raw_path):

    index = 0
    num_thread = 10
    get = 0


    ALL_DATA = []
    threads = []
    for xy in xy_list:
        index += 1
        if index == -20:
            break

        if index % num_thread == 0:
            t = threading.Thread(target=get_all_pano, args=(xy, ALL_DATA,))
            threads.append(t)
            for t in threads:
                t.setDaemon(True)
                t.start()
            time.sleep(0.2)
            t.join()

            save_mata(pid_raw_path, ALL_DATA)
            get += len(ALL_DATA)
            threads = []
            ALL_DATA = []

            print('Done:',index, '/', len(xy_list),'got:', get)

        else:
            t = threading.Thread(target=get_all_pano, args=(xy, ALL_DATA,))
            threads.append(t)




if __name__ == '__main__':
    dir_xy = r'city_xy'  # 城市坐标文件夹，注意命名"city name"+"_"+xy" 注意经纬度前后顺序
    # dir_xy = r'temp'  # 城市坐标文件夹，注意命名"city name"+"_"+xy" 注意经纬度前后顺序
    dir_pid = r'city_pid'  # 保存pid的文件夹


    # xy = [ '37.82911236137448','-122.3698446387125']
    # get_all_pano(xy, [])


    for name in os.listdir(dir_xy):
        print(name)
        # if 'Amsterdam' in name:
        #     continue
        name_city = name.split('_')[0]
        xy_path = os.path.join(dir_xy, name)
        pid_raw_path = os.path.join(dir_pid, name_city + "_pid_raw.csv")
        pid_L12_path = os.path.join(dir_pid, name_city + "_pid_L12.csv")

        xy_list = read_h3_xy(xy_path)

        run(xy_list, pid_raw_path)

        # break

        # h3_filter(pid_raw_path, pid_L12_path, level=12) # 11: Average Hexagon Edge Length 0.0249 (km)
