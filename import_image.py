import os
os.chdir('C:/Users/user/Desktop/Another_h')
from PIL import Image
im=Image.open('first_microservice.jpg')
pix_val = list(im.getdata())

pix_val_flat = [x for sets in pix_val for x in sets]
f = open('file.txt', 'wb')
f.write("pix_val_flat \n")
f.close()
format % pix_val_flat

for i in range(10):
x=random.random()
print x