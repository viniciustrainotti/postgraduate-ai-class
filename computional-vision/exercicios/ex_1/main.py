import sys
import cv2
import numpy as np

filename  = str(sys.argv[1])
technique = str(sys.argv[2])

print(filename)
print(technique)

img = cv2.imread(filename)

