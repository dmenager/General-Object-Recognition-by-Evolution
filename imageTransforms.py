import numpy as np
import sys
import argparse
import os
import ctypes
import matplotlib.pyplot as plt


from skimage import data, color
from skimage import io
from skimage.morphology import disk, square, erosion, dilation
from skimage.filters.rank import gradient, median
from skimage.filters import gaussian, rank, rank_order, sobel, laplace, gabor, threshold_adaptive
from skimage import exposure, img_as_float
from skimage.exposure import adjust_log
from skimage.transform import hough_circle, integral_image, hough_line, hough_line_peaks, radon
from skimage.feature import peak_local_max, canny, corner_harris, corner_peaks
from skimage.draw import circle_perimeter, line
from skimage.util import img_as_ubyte, img_as_int
from skimage import feature
from skimage.draw import line
from skimage.color import rgb2gray

parser = argparse.ArgumentParser(description='Process Image.')
parser.add_argument('image', metavar='I', type=str, nargs=1,
                   help='')

parser.add_argument('transforms', metavar='T', type=str, nargs=1,
                   help='')

args = parser.parse_args()

img = args.image[0]
image = os.path.abspath(img)
img = io.imread(image)
img = rgb2gray(img)
img = img_as_ubyte(img)

#print img
#io.imshow(img)
#io.show()

# Standardize image
#print img.mean()
#img = (img - img.mean()) / img.std()
#print img.shape
#io.imshow(img)
#io.show()
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
            #io.imshow(img)
            #io.show()
        elif transform[0].lower() == "gaussian":
            exitStr = "(applying gaussian)"
            img = img_as_ubyte(gaussian(img, int(transform[1])))
            #io.imshow(img)
            #io.show()
        elif transform[0].lower() == "histogram":
            exitStr = "(applying rank.equalize)"
            img = img_as_ubyte(rank.equalize(img, disk(int(transform[1]))))
            #io.imshow(img)
            #io.show()
        elif transform[0].lower() == "hough-circle":
            exitStr = "(applying hough-cirlce)"
            #img = img_as_ubyte(img)
            edges = canny(img, sigma=3, low_threshold=10, high_threshold=50)
            
            # Detect two radii
            hough_radii = np.arange(15, 30, 2)
            hough_res = hough_circle(edges, hough_radii)
            
            accums, cx, cy, radii = hough_circle_peaks (hough_res, hough_radii, total_num_peaks=3)

            for center_y, center_x, radius in zip(cy, cx, radii):
                circy, circx = circle_perimeter(center_y, center_x, radius)
                img[circy, circx] = 255
        elif transform[0].lower() == "median-blur":
            exitStr = "(applying median)"
            img = img_as_ubyte(median(img, disk(int(transform[1]))))
            #io.imshow(img)
            #io.show()
        elif transform[0].lower() == "log":
            img = img_as_ubyte(adjust_log(img, float(transform[1])))
            #io.imshow(img)
            #io.show()
        elif transform[0].lower() == "integral-blur":
            exitStr = "(applying integral_image)"
            img = img_as_ubyte(integral_image(img))
            #io.imshow(img)
            #io.show()
        elif transform[0].lower() == "canny-edge":
            exitStr = "(applying canny-edge)"
            img = img_as_ubyte(canny(img, float(transform[1]), float(transform[2]), float(transform[3])))
            #io.imshow(img)
            #io.show()
        elif transform[0].lower() == "rank-order":
            exitStr = "(applying rank_order)"
            img,_ = rank_order(img)
            #io.imshow(img)
            #io.show()
            #elif transform[0].lower() == "resize":
            #    img = resize(img, <tuple>)
        elif transform[0].lower() == "sobel":
            exitStr = "(applying sobel)"
            img = img_as_ubyte(sobel(img))
            #io.imshow(img)
            #io.show()
        elif transform[0].lower() == "erosion":
            exitStr = "(applying erosion)"
            img = img_as_ubyte(erosion(img, square(int(transform[1]))))
            #io.imshow(img)
            #io.show()
        elif transform[0].lower() == "threshold-adaptive":
            exitStr = "(applying threshold_adaptive)"
            img = img_as_ubyte(threshold_adaptive(img, int(transform[1]), transform[2]))
            #io.imshow(img)
            #io.show()
        elif transform[0].lower() == "radon":
            theta = np.linspace(0., 180., max(img.shape), endpoint=False)
            img = radon(img, theta=theta, circle=False)
            #io.imshow(img)
            #io.show()
        elif transform[0].lower() == "hough-line":
            exitStr = "(applying hough_line)"
            h, angles, d = hough_line(img)
            rows, cols = img.shape
            largest = [0]
            m, n = h.shape
            '''
            for i in np.arange(0, m):
                for j in np.arange(0, n):
                    if h[i][j] > np.array(largest).any() and h[i][j] not in largest:
                        largest.append(h[i][j])
                        if len(largest) > 100:
                            largest.remove(min(largest))
            for i in np.arange(0, m):
                for j in np.arange(0, n):
                    if h[i][j] not in largest:
                        h[i][j] = 0
            '''
            img = h
            #io.imshow(img)
            #io.show()
            #for _, angle, dist in zip(*hough_line_peaks(h, angles, d)):
            #    y0 = (dist - 0 * np.cos(angle)) / np.sin(angle)
            #    y1 = (dist - cols * np.cos(angle)) / np.sin(angle)
            #    rr, cc =line(0, y0, cols, y1)
            #    img[int(rr), int(cc)] = 255
        elif transform[0].lower() == "equalize-hist":
            exitStr = "(applying exposure.equalize_hist)"
            img = img_as_ubyte(exposure.equalize_hist(img, int(transform[1])))
            #io.imshow(img)
            #io.show()
        elif transform[0].lower() == "laplace-edge":
            exitStr = "(applying laplace)"
            img = laplace(img, int(transform[1]))
            #io.imshow(img)
            #io.show()
        elif transform[0].lower() == "dilation":
            exitStr = "(applying dilation)"
            img = img_as_ubyte(dilation(img, square(int(transform[1]))))
            #io.imshow(img)
            #io.show()
        elif transform[0].lower() == "corner-harris":
            img = corner_peaks(corner_harris(img, transform[1], float(transform[2]), float(transform[3]), float(transform[4])))
            #io.imshow(img)
            #io.show()
        elif transform[0].lower() == "gabor":
            exitStr = "(applying gabor)"
            img, _ = gabor(img, float(transform[1]))
            #io.imshow(img)
            #io.show()
print "(",
for intensity in np.nditer(img):
    print str(intensity) + " ",
print ")"
print
exit()
#except:
#    print exitStr
