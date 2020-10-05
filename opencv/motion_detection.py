import numpy as np
import cv2
import matplotlib.pyplot as plt

video = cv2.VideoCapture("movies/Touch.Me.Not.2018.PL.HDTV.x264-B89 [EXSite24.pl].mkv")
video = cv2.VideoCapture(0)

kernel = np.ones((5,5),np.uint8)

def wielokrotnosc(liczba, dzielnik):
	reszta = np.array(liczba) % dzielnik
	zaokr = liczba - reszta
	return(zaokr / dzielnik)

total = 0
# loop over the frames of the video
while True:
	(grabbed, frame_org) = video.read()

	if not grabbed:
		break

	cv2.imshow('frame_org', frame_org)

	if total != 0:
		frame_prev = frame
	else:
		frame_prev = cv2.cvtColor(frame_org, cv2.COLOR_BGR2GRAY) 

	total = total + 1

	# convert tot grayscale and blur
	frame = cv2.cvtColor(frame_org, cv2.COLOR_BGR2GRAY)
	frame = cv2.GaussianBlur(frame, (21, 21), cv2.BORDER_DEFAULT)
	cv2.imshow('frame', frame)

	# find difference between last and current frame
	frame_diff = cv2.absdiff(frame_prev, frame)
	frame_d = cv2.threshold(frame_diff, 25, 255, cv2.THRESH_BINARY)[1]
	frame_d = cv2.dilate(frame_d, None, iterations=2)

	cnts = cv2.findContours(frame_d.copy(), cv2.RETR_EXTERNAL, cv2.CHAIN_APPROX_SIMPLE)
	print(cnts)
 
	# loop over the contours
	for c in cnts:
		(x, y, w, h) = cv2.boundingRect(c)
		cv2.rectangle(frame, (x, y), (x + w, y + h), (0, 255, 0), 2)
	cv2.imshow('frame_d', frame_d)	



	'''
	pole = 100.0*np.count_nonzero(wielokrotnosc(frame_converted, 16))/(np.shape(frame_converted)[0] * np.shape(frame_converted)[1])
	poles.append(pole)

	if pole > 2.5:
		zlapana = zlapana + 1
		cv2.imshow('frame_converted', wielokrotnosc(frame_converted, 16)*16)
		cv2.imshow('frame_still', frame_org)
		# cv2.imwrite("frames/frame_%05d.jpg" % zlapana, frame_org)
	'''


	k = cv2.waitKey(10) & 0XFF
	if k == 27:
		break


cv2.destroyAllWindows()

# release the video pointer
video.release()

'''
print("Liczba złapanych klatek: %d (ze wszystkich %d)" % (zlapana, total))
print("Średni procent zmieniających się pixeli: %.3f%%" % np.mean(poles))


plt.hist(poles, bins = 100) 
plt.title("Rozkład liczby różniących się pixeli") 
plt.show()
'''