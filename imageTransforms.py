import numpy as np
import sys
import argparse
import os

from skimage import data, color
from skimage import io
from skimage.morphology import disk, square, erosion, dilation
from skimage.filters.rank import gradient, median
from skimage.filters import gaussian, rank, rank_order, sobel, laplace
from skimage import exposure, img_as_float
from skimage.transform import hough_circle, integral_image, hough_line
from skimage.feature import peak_local_max, canny
from skimage.draw import circle_perimeter
from skimage.util import img_as_ubyte
from skimage import feature
from skimage.draw import line

parser = argparse.ArgumentParser(description='Process Image.')
parser.add_argument('image', metavar='I', type=str, nargs=1,
                   help='')

parser.add_argument('transforms', metavar='T', type=str, nargs=1,
                   help='')

args = parser.parse_args()

img = args.image[0]
image = os.path.abspath(img)
img = io.imread(image, True)

trans = args.transforms[0]
transf = trans.replace('[','').split('],')
transforms = [map(str, t.replace(']','').split(',')) for t in transf]


try:
    for i, transform in enumerate(transforms):
        if i == 0:
            x1 = int(transform[0])
            x2 = int(transform[1])
            y1 = int(transform[2])
            y2 = int(transform[3])
            img = image[x1, x2 : y1, y2]
        else :
            if transform[0].lower() == "gradient":
                img = gradient(img, disk(int(transform[1])))
            elif transform[0].lower() == "gaussian":
                img = gaussian(img)
            elif transform[0].lower() == "histogram":
                img = img_as_ubyte(img)
                img = rank.equalize(img, disk(int(transform[1])))
            elif transform[0].lower() == "hough-circle":
                img = img_as_ubyte(img)
                edges = canny(img, sigma=3, low_threshold=10, high_threshold=50)

                # Detect two radii
                hough_radii = np.arange(15, 30, 2)
                hough_res = hough_circle(edges, float(transform[1]), transform[2] == True)
                
                centers = []
                accums = []
                radii = []

                for radius, h in zip(hough_radii, hough_res):
                    # For each radius, extract two circles
                    num_peaks = 2
                    peaks = peak_local_max(h, num_peaks=num_peaks)
                    centers.extend(peaks)
                    accums.extend(h[peaks[:, 0], peaks[:, 1]])
                    radii.extend([radius] * num_peaks)

                for idx in np.argsort(accums)[::-1][:5]:
                    center_x, center_y = centers[idx]
                    radius = radii[idx]
                    cx, cy = circle_perimeter(center_y, center_x, radius)
                    img[cy, cx] = (220, 20, 20)
            elif transform[0].lower() == "median-blur":
                img = median(img, disk(int(transform[1])))
            elif transform[0].lower() == "integral-blur":
                img = integral_image(img)
            elif transform[0].lower() == "canny-edge":
                img = feature.canny(img, float(transform[1]), float(transform[2]), float(transform[3]))
            elif transform[0].lower() == "rank-order":
                img = rank_order(img)
            #elif transform[0].lower() == "resize":
            #    img = resize(img, <tuple>)
            elif transform[0].lower() == "sobel":
                img = sobel(img)
            elif transform[0].lower() == "erosion":
                img = erosion(img, square(int(transform[1])))
            elif transform[0].lower() == "threshold-adaptive":
                img = threshold_adaptive(img, int(transform[1], transform[2]))
            elif transform[0].lower() == "hough-line":
                img, angles, d = hough_line(img)
            elif transform[0].lower() == "equalize-hist":
                img = exposure.equalize_hist(img, int(transform[1]))
            elif transform[0].lower() == "laplace-edge":
                img = laplace(img, int(transform[1]))
            elif transform[0].lower() == "dilation":
                img = dilate(img, square(int(transform[1])))
            #elif transform[0].lower() == "corner-harris":
            elif transform[0].lower() == "gabor":
                img = gabor(img, float(transform[1]))
    print "(",
    for intensity in np.nditer(img):
        print str(intensity) + " ",
    print ")"
except:
    print "nil"
