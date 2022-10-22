import sys
import cv2
import numpy as np

filename  = str(sys.argv[1])
technique = str(sys.argv[2])

print(filename)
print(technique)

img = cv2.imread(filename)

# verificar o shape original
# verificar o shape resultante (50%)
# fazer a verificação pela imagem original para a imagem resultante

def resize(img, technique):
    print(img.shape[0])
    print(int(img.shape[0]/2))
    new_image = np.zeros([int(img.shape[0]/2), int(img.shape[1]/2)], img.dtype)
    print(new_image)
    if(technique == 'media'):
        for y in range(0, new_image.shape[0]):
            for x in range(0, new_image.shape[1]):
                print(img[y, x])
                print((img[y, x]+img[y+1, x]+img[y, x+1]+img[y+1, x+1])/4)
                new_image[y, x] = np.clip(( (img[y, x]+img[y+1, x]+img[y, x+1]+img[y+1, x+1])/4 ), 0 , 255)
                print(new_image)

    cv2.imwrite('new_image.jpg', new_image)

#EndDef    

resize(img, technique)