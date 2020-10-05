import cv2
import numpy as np
import pandas as pd
import datetime
import sys

if __name__ == "__main__":
    # pobieramy dane z parametrów
    
    # ścieżka do filmu
    MOVIE_PATH = sys.argv[1]
    
    # plik wyjściowy
    OUTPUT_FILE = sys.argv[2]
    
    # czy pokazywać kolejne klatki?
    SHOW_FRAMES = True if sys.argv[3][0].upper() == "T" else False

    # strumień wideo z filmu
    cap = cv2.VideoCapture(MOVIE_PATH)

    # licznik klatek i miejsce na dane wynikowe
    i = 0
    df = pd.DataFrame()

    mega_start = datetime.datetime.now()

    while True:
        start = datetime.datetime.now()

        i += 1

        # wczytujemy kolejną klatkę
        rst, frame = cap.read()

        # jeśli się nie udało - kończymy pętlę
        if not rst:
            break

        # jeśli jest szersza niż 300 pikseli to zmniejszamy jej wymiary o połowę
        # w sumie tylko po to, żeby szybciej się to przeliczało i ewentualnie mieściło na ekranie
        if np.shape(frame)[0] > 300:
            frame = cv2.resize(frame, (int(np.shape(frame)[1] / 2), int(np.shape(frame)[0] / 2)))

        # pokzaujemy ramkę RGB jeśli był ustawiony odpowiedni parametr
        if SHOW_FRAMES:
            cv2.imshow("rgb", frame)

        # składowe RGB
        mean_red = np.mean(frame[:, :, 2])
        mean_green = np.mean(frame[:, :, 1])
        mean_blue = np.mean(frame[:, :, 0])

        median_red = np.median(frame[:, :, 2])
        median_green = np.median(frame[:, :, 1])
        median_blue = np.median(frame[:, :, 0])

        sum_red = np.sum(frame[:, :, 2])
        sum_green = np.sum(frame[:, :, 1])
        sum_blue = np.sum(frame[:, :, 0])

        # konwersja do przestrzeni HSV
        frame = cv2.cvtColor(frame, cv2.COLOR_BGR2HSV)

        # pokazujemy ramkę HSV
        if SHOW_FRAMES:
            cv2.imshow("hsv", frame)

        # składowe HSV
        mean_value = np.mean(frame[:, :, 2])
        mean_saturation = np.mean(frame[:, :, 1])
        mean_hue = np.mean(frame[:, :, 0])

        median_value = np.median(frame[:, :, 2])
        median_saturation = np.median(frame[:, :, 1])
        median_hue = np.median(frame[:, :, 0])

        sum_value = np.sum(frame[:, :, 2])
        sum_saturation = np.sum(frame[:, :, 1])
        sum_hue = np.sum(frame[:, :, 0])

        # dodajemy zebrane informacje do tabelki
        df = df.append(pd.DataFrame({"frame": i,
                                     "mean_red": mean_red,
                                     "mean_green": mean_green,
                                     "mean_blue": mean_blue,
                                     "median_red": median_red,
                                     "median_green": median_green,
                                     "median_blue": median_blue,
                                     "sum_red": sum_red,
                                     "sum_green": sum_green,
                                     "sum_blue": sum_blue,
                                     "mean_value": mean_value,
                                     "mean_saturation": mean_saturation,
                                     "mean_hue": mean_hue,
                                     "median_value": median_value,
                                     "median_saturation": median_saturation,
                                     "median_hue": median_hue,
                                     "sum_value": sum_value,
                                     "sum_saturation": sum_saturation,
                                     "sum_hue": sum_hue},
                                    index=[0]))

        
        # naciśnięcie ESC powoduje wyjście z pętli
        k = cv2.waitKey(10) & 0XFF
        if k == 27:
            break

        # some code
        elapsed = datetime.datetime.now() - start
        mega_elapsed = datetime.datetime.now() - mega_start

        print("\rFrame: %06d | %.2f FPS | Total time %.1f s -> mean %.1f FPS" %
              (i, elapsed.microseconds/1000, mega_elapsed.total_seconds(), i/mega_elapsed.total_seconds()),
              end="    ")

    # zamykamy otwarte okienka z podglądem klatek
    if SHOW_FRAMES:
        cv2.destroyAllWindows()
        
    # zapisujemy dane do pliku CSV
    df.to_csv(OUTPUT_FILE, sep=";", index=False, decimal=',')
