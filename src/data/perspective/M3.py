# encoding: utf-8
# author: zhaotianhong
# contact: zhaoteanhong@gmail.com
# file: main.py
# time: 2022/1/22 10:15


import os
import numpy as np
import pandas as pd
import tool as E2P
import threading
import time
import cv2



def cityscape_colormap():
    """Get CityScapes colormap"""
    colormap = np.zeros((256, 3), dtype=np.uint8)
    colormap[0] = [128, 64, 128]
    colormap[1] = [244, 35, 232]
    colormap[2] = [70, 70, 70]
    colormap[3] = [102, 102, 156]
    colormap[4] = [190, 153, 153]
    colormap[5] = [153, 153, 153]
    colormap[6] = [250, 170, 30]
    colormap[7] = [220, 220, 0]
    colormap[8] = [107, 142, 35]
    colormap[9] = [152, 251, 152]
    colormap[10] = [70, 130, 180]
    colormap[11] = [220, 20, 60]
    colormap[12] = [255, 0, 0]
    colormap[13] = [0, 0, 142]
    colormap[14] = [0, 0, 70]
    colormap[15] = [0, 60, 100]
    colormap[16] = [0, 80, 100]
    colormap[17] = [0, 0, 230]
    colormap[18] = [119, 11, 32]
    colormap = colormap[:, ::-1]
    return colormap


def visualize_semantic(semantic, save_path, colormap, image=None, weight=0.5):
    """
    Save semantic segmentation results.

    Args:
        semantic(np.ndarray): The result semantic segmenation results, shape is (h, w).
        save_path(str): The save path.
        colormap(np.ndarray): A color map for visualization.
        image(np.ndarray, optional): Origin image to prediction, merge semantic with
            image if provided. Default: None.
        weight(float, optional): The image weight when merge semantic with image. Default: 0.5.
    """
    semantic = semantic.astype('uint8')
    colored_semantic = colormap[semantic]
    if image is not None:
        colored_semantic = cv2.addWeighted(image, weight, colored_semantic,
                                           1 - weight, 0)
    cv2.imwrite(save_path, colored_semantic)


def statistics_class(new_img):
    total = new_img.size
    low_dim = new_img.ravel()
    mask = np.unique(low_dim)
    tmp = {}
    for v in mask:
        tmp[v] = np.sum(low_dim == v) / total

    result = np.zeros(19)
    for i in range(19):
        if i in tmp.keys():
            result[i] = tmp[i]
        else:
            continue
    result = list(result)
    return result


def load_xy_info(path_img_xy):
    pid_info = {}
    with open(path_img_xy, 'r') as f:
        for line in f:
            line_arr = line.strip().split(',')
            pid = line_arr[1]
            pid_info[pid] = line_arr

    return pid_info


def run(path_input_ss, path_input_raw, path_output_c, img_info):

    thetas = [0, 45, 90, 135, 180, 225, 270, 315]
    FOVs = [120, 90, 80, 70]

    aspects_v = [(3, 4),(2.25,4),(4,4)]
    aspects = [(3, 4), (9, 16), (1,1)]


    img_ss = cv2.imread(path_input_ss, cv2.IMREAD_COLOR)
    # img_raw = cv2.imread(path_input_raw, cv2.IMREAD_COLOR)

    equ_ss = E2P.Equirectangular(img_ss)
    # equ_raw = E2P.Equirectangular(img_raw)

    colored_ss = colormap[img_ss[:,:,0]]
    equ_color = E2P.Equirectangular(colored_ss)

    for theta in thetas:
        for FOV in FOVs:
            for ii in range(len(aspects)):
                line = [city]
                line.extend(img_info[:])
                height = int(aspects_v[ii][0] * show_size)
                width = int(aspects_v[ii][1] * show_size)
                aspect_name = '%s--%s'%(aspects[ii][0], aspects[ii][1])
                img = equ_ss.GetPerspective(FOV, theta, 0, height, width)
                result = statistics_class(img[:, :, 0])
                line.append(theta)
                line.append(FOV)
                line.append(aspect_name)
                line.extend(result)
                result_all.append(line)

                if output_v == 1:
                    # if save the images
                    img_color = equ_color.GetPerspective(FOV, theta, 0, height, width)
                    img_raw = equ_raw.GetPerspective(FOV, theta, -0, height, width)

                    path_output = path_output_c[:]
                    # path_output_ss =path_output.replace('.png', '_Direction_%s_FOV_%s_aspect_%s_ss.png'%(theta, FOV, aspect_name))
                    # path_output_raw = path_output.replace('.png', '_Direction_%s_FOV_%s_aspect_%s_raw.png'%(theta, FOV, aspect_name))
                    path_output_add = path_output.replace('.png','_Direction_%s_FOV_%s_aspect_%s_add.png'%(theta, FOV, aspect_name))

                    # visualize_semantic(img[:, :, 0], path_output_ss, colormap)
                    # cv2.imwrite(path_output_ss, img_color)
                    # cv2.imwrite(path_output_raw, img_raw)

                    visualize_semantic(img[:, :, 0], path_output_add, colormap, image=img_raw)





if __name__ == '__main__':
    header = ['city','H3_L12', 'pid', 'lat', 'lng', 'year', 'month', 'direction', 'FOV', 'aspect', 'road', 'sidewalk', 'building',
              'wall', 'fence', 'pole', 'traffic light', 'traffic sign', 'vegetation', 'terrain', 'sky', 'person',
              'rider',  'car', 'truck', 'bus', 'train', 'motorcycle', 'bicycle']

    dir_input = r'E:\SS'
    dir_input_raw = r'D:\MyData\Code\python\GDX\Filip\raw'
    dir_out_show = r'D:\MyData\Code\python\GDX\Filip\perspective'
    path_img_xy = 'Singapore_img_xy.csv'
    path_result = 'D:\MyData\Code\python\GDX\Filip\sample_csv\Perspective_result_sample.csv'

    city = 'Singapore'
    output_v = 0
    show_size = 50  # 像素大小 * 4 或者 3
    threads = []
    num_thread = 50

    colormap = cityscape_colormap()


    for city in os.listdir(dir_input):

        if city in ["HongKong", 'Amsterdam']:
            continue

        #
        index = 0
        global result_all
        result_all = []

        dir_input_city = os.path.join(dir_input, city)
        path_img_xy = 'D:\Tianhong\Statistics\PID\%s_pid_raw.csv' % (city)
        path_result = r'E:\Result\Perspective_%s.csv'%(city)
        pids_info = load_xy_info(path_img_xy)

        for name in os.listdir(dir_input_city):
            try:
                img_info = pids_info[name[:-4]][:]
            except:
                continue

            index += 1
            if index == -200:
                break

            path_input_ss = os.path.join(dir_input_city, name)
            path_input_raw = os.path.join(dir_input_raw, name)
            path_output = os.path.join(dir_out_show, name.replace('jpg','png'))

            if index % num_thread == 0:
                print('Now:', index, len(result_all))
                t = threading.Thread(target=run, args=(path_input_ss, path_input_raw, path_output, img_info,))
                threads.append(t)
                for t in threads:
                    t.setDaemon(True)
                    t.start()
                t.join()
                threads = []
            else:
                t = threading.Thread(target=run, args=(path_input_ss, path_input_raw, path_output, img_info,))
                threads.append(t)

        time.sleep(20)
        datadf = pd.DataFrame(result_all, columns=header)
        datadf.to_csv(path_result, float_format='%.6f')

        # break
