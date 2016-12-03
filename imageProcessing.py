import numpy as np
import sys
import argparse
import os

from skimage import data
from skimage import io

parser = argparse.ArgumentParser(description='Process Image.')
parser.add_argument('image', metavar='N', type=str, nargs=1,
                   help='')

args = parser.parse_args()
img = args.image[0]
image = os.path.abspath(img)
img_gray = io.imread(image, True)
print "(",
for intensity in np.nditer(img_gray):
    print str(intensity) + " ",
print ")"
