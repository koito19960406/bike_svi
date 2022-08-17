# encoding: utf-8
# author: zhaotianhong
# contact: zhaoteanhong@gmail.com
# file: PLOT.py
# time: 2021/11/8 17:13



import matplotlib.pyplot as plt
import os

dir = r'Year_SG_pid'

num_list = []
year = []
for name in os.listdir(dir):
    path = os.path.join(dir,name)
    num = 0
    # with open(path, 'r') as f:
    #     for line in f:
    #         num += 1

    num_list.append(num)
    year.append(name[:-4])

num_list = [236809, 267183, 3260, 236435, 99407, 78957, 42407, 366656, 178392, 140464, 589855, 549596, 250648, 408953]
print(num_list)
x = [i for i in range(len(num_list))]

fig,ax = plt.subplots()
plt.bar(x, num_list)

font = {"size":15}
plt.title("Number of Historic SVI Updates in Singapore")

plt.ylabel("Number of SVI", font)
plt.xlabel("Year", font)
plt.xticks(x,year)
fig.autofmt_xdate(rotation=45)
plt.tight_layout()
plt.show()