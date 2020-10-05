import pandas as pd
import numpy as np
import cv2
import matplotlib.pyplot as plt
import seaborn as sns

def get_frame_at(src, location):
    cap = cv2.VideoCapture()
    cap.open(src)
    cap.set(cv2.CAP_PROP_POS_FRAMES, location)
    rst, frame = cap.read()
    return rst, frame

i = 0
df = pd.DataFrame()
last_median_red = 1

while True:
    rst, frame = get_frame_at("movie/Wildlife.wmv", i)

    if not rst:
        break

#    print(rst, np.shape(frame)) # Height x Width X Channels

    frame = cv2.resize(frame, (640,360))
    org_frame = frame
    if i == 0:
        last_frame = frame

    frame = cv2.cvtColor(frame, cv2.COLOR_BGR2HSV)

    cv2.imshow("org", frame)

    # cv2.imshow("blue", frame[:,:,0])
    # cv2.imshow("green", frame[:,:,1])
    # cv2.imshow("red", frame[:,:,2])

    mean_red = np.mean(frame[:,:,2])
    mean_green = np.mean(frame[:,:,1])
    mean_blue = np.mean(frame[:,:,0])

    median_red = np.median(frame[:,:,2])
    median_green = np.median(frame[:,:,1])
    median_blue = np.median(frame[:,:,0])

    sum_red = np.sum(frame[:,:,2])
    sum_green = np.sum(frame[:,:,1])
    sum_blue = np.sum(frame[:,:,0])

    df = df.append(pd.DataFrame({"i": i,
                  "rm": mean_red, "rd": median_red, "rs": sum_red,
                  "gm": mean_green, "gd": median_green, "gs": sum_green,
                  "bm": mean_blue, "bd": median_blue, "bs": sum_blue},
                       index=[0]))

    '''
    print("F: {} R: mean {} median {} sum {} | G: mean {} median {} sum {} | B: mean {} median {} sum {}".format(i,
                                                                                                round(mean_red, 2),
                                                                                                round(median_red, 2),
                                                                                                sum_red,
                                                                                                round(mean_green, 2),
                                                                                                round(median_green, 2),
                                                                                                sum_green,
                                                                                                round(mean_blue, 2),
                                                                                                round(median_blue, 2),
                                                                                                sum_blue))
    '''


    i += 15 # krok co 15 klatek

    if abs((median_red - last_median_red)/last_median_red) > 0.1:
        print("Cięcie na klatce {}".format(i))
        cv2.imshow("zmieniona_klatka", org_frame)
        cv2.imshow("poprzednia", last_frame)


    last_median_red = median_red
    last_frame = org_frame

    k = cv2.waitKey(10) & 0XFF
    if k == 27:
        break

cv2.destroyAllWindows()

# min tutaj zawsze ==0, więc można to odpuścić
# df["rm"] = (df["rm"]-df["rm"].min()) / (df["rm"].max()-df["rm"].min())
# df["rd"] = (df["rd"]-df["rd"].min()) / (df["rd"].max()-df["rd"].min())
# df["rs"] = (df["rs"]-df["rs"].min()) / (df["rs"].max()-df["rs"].min())

print(df[['i', 'rm', 'rd', 'rs']].head(20))

# wykres wartości
fig = plt.figure(figsize=(15,8))
# sns.lineplot(data=df, x='i', y='rm', color='red')
sns.lineplot(data=df, x='i', y='rd', color='green') # najlepiej widać różnice
sns.lineplot(data=df, x='i', y='rs', color='blue')
plt.show()

# oblicznie zmian między wierszami
df_changes = df.pct_change().reset_index(drop=True)
df_changes['i'] = df_changes.index

# przemnożenie zmiany przez 100
# df_changes['rm'] = 100*df_changes['rm']
df_changes['rd'] = 100*df_changes['rd'] # najlepiej widać różnice
df_changes['rs'] = 100*df_changes['rs']


print(df_changes[['i', 'rm', 'rd', 'rs']])


# wykres zmian
fig = plt.figure(figsize=(15,8))
# sns.lineplot(data=df_changes, x='i', y='rm', color='red')
sns.lineplot(data=df_changes, x='i', y='rd', color='green')
sns.lineplot(data=df_changes, x='i', y='rs', color='blue')
plt.show()


# Histogram po kolorach
channels = ('b', 'g', 'r')
# we now separate the colors and plot each in the Histogram
for i, color in enumerate(channels):
    # oblicz histogram dla i-tego kanału
    histogram = cv2.calcHist([frame], [i], None, [256], [0, 256])
    # narysuj histogram (linia) w matplotlibie odpowiednim kolorem
    plt.plot(histogram, color=color)
    plt.xlim([0, 256])

plt.show()
