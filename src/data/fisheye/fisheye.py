# encoding: utf-8
# author: zhaotianhong
# contact: zhaoteanhong@gmail.com
# file: fisheye.py
# time: 2022/6/9 9:01




import cv2
import numpy as np
import math
import pandas as pd
import os
import threading
import time
import copy

header = ["city",'H3_L12', 'pid','lat','lng','year','month',' road', 'sidewalk',  'building',  'wall',  'fence',  'pole',
           'traffic light', 'traffic sign', 'vegetation',  'terrain',  'sky', 'person',  'rider',
            'car',  'truck',  'bus',  'train',  'motorcycle',  'bicycle']

col_map = {0: 'road', 1: 'sidewalk', 2: 'building', 3: 'wall', 4: 'fence', 5: 'pole', 6: 'traffic light',
           7: 'traffic sign', 8: 'vegetation', 9: 'terrain', 10: 'sky', 11: 'person', 12: 'rider',
           13: 'car', 14: 'truck', 15: 'bus', 16: 'train', 17: 'motorcycle', 18: 'bicycle'}



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
    colormap[255] = [255,255,255]
    colormap = colormap[:, ::-1]
    return colormap


def cityscape_colormap_green():
    """Get CityScapes colormap"""
    colormap = np.zeros((256, 3), dtype=np.uint8)

    colormap[8] = [107, 142, 35]
    colormap[9] = [107, 142, 35]
    colormap[255] = [255,255,255]
    colormap = colormap[:, ::-1]
    return colormap


def cityscape_colormap_sky():
    """Get CityScapes colormap"""
    colormap = np.zeros((256, 3), dtype=np.uint8)
    colormap[10] = [70, 130, 180]
    colormap[255] = [255,255,255]
    colormap = colormap[:, ::-1]
    return colormap


def cityscape_colormap_building():
    """Get CityScapes colormap"""
    colormap = np.zeros((256, 3), dtype=np.uint8)
    colormap[2] = [70, 70, 70]
    colormap[255] =[255,255,255]
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



def SVFcalculationOnFisheye(skyImg):
    '''
    This script is used to calculate the Sky View Factor from the binary classification
    result of re-projected Sphere view Google Street View panorama

    https://www.researchgate.net/profile/Silvana_Di_Sabatino/publications/2
    https://www.unibo.it/sitoweb/silvana.disabatino/en
    Recommended by Rex

    Input:
        skyImg: the input sky extraction result in numpy array format
        skyPixlValue: the pixel value of the sky pixels
    Output:
        SVFres: the value of the SVF
    '''

    F_dict = {i:0 for i in range(19)}

    rows = skyImg.shape[0]
    cols = skyImg.shape[1]

    # split the sphere view images into numbers of strips based on spikes
    ringNum = 36
    ringRad = rows / (2.0 * ringNum)

    # the center of the fisheye image
    Cx = int(0.5 * cols)
    Cy = int(0.5 * rows)

    # the SVF value
    SVF = 0.0

    for i in range(1,ringNum):
        # calculate the SVFi for each ring
        ring_dict =  {i:0 for i in range(19)}
        ang = np.pi * (i - 0.5) / (2 * ringNum)
        SVFmaxi = 2 * np.pi / (4 * ringNum) * math.sin(np.pi * (2 * i - 1) / (2 * ringNum))

        # the radii of inner limit and the outer limit
        radiusI = i * ringRad
        radiusO = (i + 1) * ringRad

        # Total areas (pixels) of sky in the ring region, search in a squre box
        totalSkyPxl = 0
        totalPxl = 0

        for x in range(int(Cx - (i + 1) * ringRad), int(Cx + (i + 1) * ringRad)):
            for y in range(int(Cy - (i + 1) * ringRad), int(Cy + (i + 1) * ringRad)):
                # the distance to the center of the fisheye image
                dist2Center = math.sqrt((x - Cx) ** 2 + (y - Cy) ** 2)

                # # if the pixel in the search box in the ring region
                # if dist2Center > radiusI and dist2Center <= radiusO and skyImg[x, y] == skyPixlValue:
                #     totalSkyPxl = totalSkyPxl + 1
                if dist2Center > radiusI and dist2Center <= radiusO:
                    index = skyImg[x, y]
                    try:
                        ring_dict[index] += 1
                    except:
                        continue

        totalPxl = sum(ring_dict.values())
        for i in range(19):
            i_pxl = ring_dict[i]
            i_alphai = i_pxl * 1.0 / totalPxl

            i_SVFi = SVFmaxi * i_alphai
            F_dict[i] += i_SVFi


        # # alphai = totalSkyPxl/(np.pi*(radiusO**2 - radiusI**2))
        # alphai = totalSkyPxl * 1.0 / totalPxl
        #
        # SVFi = SVFmaxi * alphai
        # SVF = SVF + SVFi

    sta = list(F_dict.values())

    return sta



def transform(img):
    rows,cols,c = img.shape
    R = np.int(cols/2/math.pi)
    D = R*2
    cx = R
    cy = R
    new_img = np.zeros((D,D,c),dtype = np.uint8)
    new_img[:,:,:] = 255

    for i in range(D):
        for j in range(D):
            r = math.sqrt((i-cx)**2+(j-cy)**2)
            if r > R:
                continue
            tan_inv = np.arctan((j-cy)/(i-cx+1e-10))
            if(i<cx):
                theta = math.pi/2+tan_inv
            else:
                theta = math.pi*3/2+tan_inv
            xp = np.int(np.floor(theta/2/math.pi*cols))
            yp = np.int(np.floor(r/R*rows)-1)
            new_img[j,i] = img[yp,xp]
    return new_img


def statistics_class(new_img):
    total = new_img.size*(1-0.215)
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
    with open(path_img_xy,'r') as f:
        for line in f:
            line_arr = line.strip().split(',')
            pid = line_arr[1]
            pid_info[pid] = line_arr

    return pid_info



def trans_round(img_raw, reduce):
    # Compress image
    rows, cols, c = img_raw.shape
    new_w, new_h = int(rows * reduce), int(cols * reduce)
    img_reduce = cv2.resize(img_raw, (new_w, new_h))

    # Crop above the horizontal
    rows, cols, c = img_reduce.shape
    new_rows = int(rows / 2)
    img = img_reduce[:new_rows, :, :]

    img = transform(img)

    return img


def run(path_ss_input, path_raw_input, path_show_output, img_info):

    line = [city]
    line.extend(img_info[:])

    img_ss = cv2.imread(path_ss_input)
    img_ss_round = trans_round(img_ss, reduce)

    # # 平面计算
    # result_p = statistics_class(img_ss_round[:, :, 0])
    # 考虑弧面
    result = SVFcalculationOnFisheye(img_ss_round[:, :, 0])

    line.extend(result)
    result_all.append(line)


    if output_v == 1:

        visualize_semantic(img_ss_round[:,:,0], path_show_output, colormap)
        visualize_semantic(img_ss_round[:, :, 0], path_show_output.replace(".jpg", '_green.jpg'), colormap_green)
        visualize_semantic(img_ss_round[:, :, 0], path_show_output.replace(".jpg", '_sky.jpg'), colormap_sky)
        visualize_semantic(img_ss_round[:, :, 0], path_show_output.replace(".jpg", '_building.jpg'), colormap_building)

        # Save the raw fisheye
        img_raw = cv2.imread(path_raw_input)
        img_raw_round = trans_round(img_raw, reduce)

        cv2.imwrite(path_show_output.replace(".jpg", '_raw.jpg'), img_raw_round)
        visualize_semantic(img_ss_round[:, :, 0], path_show_output.replace(".jpg", '_add.jpg'), colormap, image= img_raw_round)


if __name__ == '__main__':

    dir_input = r'F:\SS'

    dir_input_raw = r'D:\MyData\Code\python\GDX\Filip\raw'
    dir_out_show = r'D:\MyData\Code\python\GDX\Filip\fisheye'

    reduce = 0.2
    output_v = 0 # 0 不输出图片，1 输出图片

    index = 0
    num_thread = 50


    colormap = cityscape_colormap()
    colormap_green = cityscape_colormap_green()
    colormap_sky = cityscape_colormap_sky()
    colormap_building = cityscape_colormap_building()

    threads = []

    for city in os.listdir(dir_input):


        index = 0
        global result_all
        result_all = []

        dir_input_city = os.path.join(dir_input, city)
        path_img_xy = 'D:\Tianhong\Statistics\PID\%s_pid_raw.csv' % (city)
        path_result = r'F:\Result\Fisheye_%s.csv'%(city)
        pids_info = load_xy_info(path_img_xy)

        for name in os.listdir(dir_input_city):
            try:
                img_info = copy.deepcopy(pids_info[name[:-4]])
            except:
                continue

            index += 1
            if index == -200:
                break

            path_ss_input = os.path.join(dir_input_city, name)
            path_raw_input = os.path.join(dir_input_raw, name)
            path_show_output = os.path.join(dir_out_show, name.replace('.jpg','.jpg'))

            if index % num_thread == 0:
                print(city, 'Now:', index, len(result_all))
                t = threading.Thread(target=run, args=(path_ss_input, path_raw_input,
                                                       path_show_output,img_info,))
                threads.append(t)
                for t in threads:
                    t.setDaemon(True)
                    t.start()
                t.join()
                threads = []
            else:
                t = threading.Thread(target=run, args=(path_ss_input, path_raw_input,
                                                       path_show_output,img_info,))
                threads.append(t)

        time.sleep(20)

        datadf = pd.DataFrame(result_all, columns=header)
        datadf.to_csv(path_result, float_format='%.6f')

        # break


