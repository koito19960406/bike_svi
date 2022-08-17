# encoding: utf-8
# author: zhaotianhong
# contact: zhaotianhong2016@email.szu.edu.cn


import cv2
import numpy as np
from matplotlib import pyplot as plt
# import pymeanshift as pms
import ndicom_meanshift as pms
import math

def calSvf(img):
    # cal theta parameter in equation
    def theta(xf, yf):
        if xf < cx:
            return np.pi / 2 + np.arctan((yf - cy) / (xf - cx))
        else:
            return 3 * np.pi / 2 + np.arctan((yf - cy) / (xf - cx))

    # image circle subset
    # return white pixel count divide whole pixel count
    def pt(i):
        circle_img = np.zeros((h, w), np.uint8)
        cv2.circle(
            circle_img, (int(h / 2), int(w / 2)),
            int(r0 - i * wr),
            1,
            thickness=-1)
        masked_data1 = cv2.bitwise_and(brightness, brightness, mask=circle_img)
        circle_img2 = np.zeros((h, w), np.uint8)
        cv2.circle(
            circle_img2, (int(h / 2), int(w / 2)),
            int(r0 - (i - 1) * wr),
            1,
            thickness=-1)
        masked_data2 = cv2.bitwise_and(brightness, brightness, mask=circle_img2)

        masked_data = cv2.bitwise_not(
            masked_data1, masked_data1, mask=masked_data2)

        unique, count = np.unique(masked_data, return_counts=True)
        t = np.pi * np.power(int(r0 - (i - 1) * wr), 2) - np.pi * np.power(int(r0 - i * wr), 2)
        return count[1] / int(t) if len(count) > 1 else 0

    # read image
    img1 = img[:,:,0]
    wc = img1.shape[1]
    hc = img1.shape[0]
    r0 = wc / (2 * np.pi)
    cx = cy = r0

    # histogram equalization
    # img_yuv = cv2.cvtColor(img1, cv2.COLOR_BGR2YUV)
    # img_yuv[:,:,0] = cv2.equalizeHist(img_yuv[:,:,0])
    # img1 = cv2.cvtColor(img_yuv, cv2.COLOR_YUV2BGR)

    rows, cols, c = img.shape
    R = np.int(cols / 2 / math.pi)
    D = R * 2
    cx = R
    cy = R
    new_img = np.zeros((D, D, c), dtype=np.uint8)
    new_img[:, :, :] = 100

    for i in range(D):
        for j in range(D):
            r = math.sqrt((i - cx) ** 2 + (j - cy) ** 2)
            if r > R:
                continue
            tan_inv = np.arctan((j - cy) / (i - cx + 1e-10))
            if (i < cx):
                theta = math.pi / 2 + tan_inv
            else:
                theta = math.pi * 3 / 2 + tan_inv
            xp = np.int(np.floor(theta / 2 / math.pi * cols))
            yp = np.int(np.floor(r / R * rows) - 1)
            new_img[j, i] = img[yp, xp]

    #
    # img2 = np.empty([int(2 * r0) + 1, int(2 * r0) + 1, 3])
    #
    # # project to fisheye image
    # for xf in range(img2.shape[1]):
    #     for yf in range(img2.shape[0]):
    #         xc = theta(xf, yf) / (2 * np.pi) * wc
    #         r = np.sqrt(np.power(xf - cx, 2) + np.power(yf - cy, 2))
    #         yc = r / r0 * hc
    #         if xc <= img1.shape[1] and yc <= img1.shape[0]:
    #             img2[yf][xf] = img1[int(yc)][int(xc)]
    #
    # img2 = np.uint8(img2)

    # # meanshift segment
    # (segmented_image, labels_image, number_regions) = pms.segment(
    #     img2, spatial_radius=6, range_radius=4.5, min_density=50)
    #
    # # threshold segment
    # b, g, r = cv2.split(segmented_image)
    # brightness = (0.5 * r + g + 1.5 * b) / 3
    # brightness = np.uint8(brightness)
    # ret, th = cv2.threshold(brightness, 0, 255,
    #                         cv2.THRESH_BINARY + cv2.THRESH_OTSU)
    # h = brightness.shape[0]
    # w = brightness.shape[1]
    # exg = 2 * g - b - r
    #
    # for y in range(0, h):
    #     for x in range(0, w):
    #         if brightness[y, x] >= ret:
    #             brightness[y, x] = 255
    #         elif brightness[y, x] >= exg[y, x]:
    #             brightness[y, x] = 127
    #         else:
    #             brightness[y, x] = 0

    # cal sky view factor
    n = 37
    wr = r0 / n
    svf = 0
    c = 1 / (2 * np.pi) * np.sin(
        np.pi / (2 * n))  # constant in front of svf calculation equation
    fn = 0

    for i in range(1, n):
        fn = fn + np.sin(np.pi * (2 * i - 1) / (2 * n)) * pt(i)
        svf = np.pi / (2 * n) * fn
    return (img2, segmented_image, brightness, svf)


reduce =  0.1
path = r'D:\MyData\Code\python\GDX\Filip\result_ss\semantic\_3Z6wEj8A-5mauhdAlSUiQ.jpg'
img_raw = cv2.imread(path)

# Compress image
rows, cols, c = img_raw.shape
new_w, new_h = int(rows * reduce), int(cols * reduce)
img_reduce = cv2.resize(img_raw, (new_w, new_h))

# Crop above the horizontal
rows, cols, c = img_reduce.shape
new_rows = int(rows / 2)
img = img_reduce[:new_rows, :, :]


aa = calSvf(img)

