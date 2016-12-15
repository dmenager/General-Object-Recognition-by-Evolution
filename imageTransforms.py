import numpy as np
import sys
import argparse
import os
import ctypes

from skimage import data, color
from skimage import io
from skimage.morphology import disk, square, erosion, dilation
from skimage.filters.rank import gradient, median
from skimage.filters import gaussian, rank, rank_order, sobel, laplace, gabor, threshold_adaptive
from skimage import exposure, img_as_float
from skimage.transform import hough_circle, integral_image, hough_line, hough_line_peaks
from skimage.feature import peak_local_max, canny
from skimage.draw import circle_perimeter, line
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

def is_number(s):
    try:
        float(s)
        return True
    except ValueError:
        return False

exitStr = ""
#try:
for i, transform in enumerate(transforms):
    if i == 0 and is_number(transform[0]):
        try:
            x1 = int(transform[0])
            x2 = int(transform[1])
            y1 = int(transform[2])
            y2 = int(transform[3])
            img = image[x1, x2 : y1, y2]
        except:
            print "(ROI Fault)"
    else :
        if transform[0].lower() == "gradient":
            exitStr = "(applying gradient)"
            img = gradient(img, disk(int(transform[1])))
        elif transform[0].lower() == "gaussian":
            exitStr = "(applying gaussian)"
            img = gaussian(img, int(transform[1]))
        elif transform[0].lower() == "histogram":
            exitStr = "(applying rank.equalize)"
            img = img_as_ubyte(img)
            img = rank.equalize(img, disk(int(transform[1])))
        elif transform[0].lower() == "hough-circle":
            exitStr = "(applying hough-cirlce)"
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
                img[cy, cx] = 255
        elif transform[0].lower() == "median-blur":
            exitStr = "(applying median)"
            img = median(img, disk(int(transform[1])))
        elif transform[0].lower() == "integral-blur":
            exitStr = "(applying integral_image)"
            img = integral_image(img)
        elif transform[0].lower() == "canny-edge":
            exitStr = "(applying canny-edge)"
            img = canny(img, float(transform[1]), float(transform[2]), float(transform[3]))
            img = img.astype(float)
        elif transform[0].lower() == "rank-order":
            exitStr = "(applying rank_order)"
            img,_ = rank_order(img)
            #elif transform[0].lower() == "resize":
            #    img = resize(img, <tuple>)
        elif transform[0].lower() == "sobel":
            exitStr = "(applying sobel)"
            img = sobel(img)
        elif transform[0].lower() == "erosion":
            exitStr = "(applying erosion)"
            img = erosion(img, square(int(transform[1])))
        elif transform[0].lower() == "threshold-adaptive":
            exitStr = "(applying threshold_adaptive)"
            img = img_as_ubyte(img)
            img = threshold_adaptive(img, int(transform[1]), transform[2])
            for i,pix in enumerate(img):
                if False == img[i].all():
                    img[i] = 0
                else:
                    img[i] = 255            
        elif transform[0].lower() == "hough-line":
            exitStr = "(applying hough_line)"
            h, angles, d = hough_line(img)
            rows, cols = img.shape
            for _, angle, dist in zip(*hough_line_peaks(h, angles, d)):
                y0 = (dist - 0 * np.cos(angle)) / np.sin(angle)
                y1 = (dist - cols * np.cos(angle)) / np.sin(angle)
                rr, cc =line(0, y0, cols, y1)
                img[int(rr), int(cc)] = 255
        elif transform[0].lower() == "equalize-hist":
            exitStr = "(applying exposure.equalize_hist)"
            img = exposure.equalize_hist(img, ctypes.c_uint64(int(transform[1])))
        elif transform[0].lower() == "laplace-edge":
            exitStr = "(applying laplace)"
            img = laplace(img, int(transform[1]))
        elif transform[0].lower() == "dilation":
            exitStr = "(applying dilation)"
            img = dilation(img, square(int(transform[1])))
            #elif transform[0].lower() == "corner-harris":
        elif transform[0].lower() == "gabor":
            exitStr = "(applying gabor)"
            img, _ = gabor(img, float(transform[1]))
print "(",
for intensity in np.nditer(img):
    print str(intensity) + " ",
print ")"
print
#except:
#    print exitStr
