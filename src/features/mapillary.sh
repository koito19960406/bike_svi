#!/bin/sh
# set inplace script
git clone https://github.com/mapillary/inplace_abn.git
cd inplace_abn
python setup.py install
cd scripts
pip install -r requirements.txt

# run the model for inference
python test_vistas.py "/Volumes/Extreme SSD/bike_svi/data/external/wide_resnet38_deeplab_vistas.pth.tar" "/Volumes/Extreme SSD/bike_svi/data/raw/cities/London/gsv/image/perspective" "/Volumes/Extreme SSD/bike_svi/data/interim/cities/London/segmented"