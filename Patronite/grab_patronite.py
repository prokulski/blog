import pandas as pd
import time
import random

import urllib.request
import urllib.error

from bs4 import BeautifulSoup

from tqdm.notebook import trange, tqdm


############
# jesli sa zapisane plik wpisz odpowiednie id i added (nazwa pliku: added_id.pkl)
# w przeciwnym przypadku id = 0 
id = 2900
added = 34
############


def grab_page(patronite_id):
    page_url = f"https://patronite.pl/profil/{patronite_id}/patron"

    # zapytanie razem z UserAgent
    req = urllib.request.Request(page_url,
                                    headers={
                                        'User-Agent': 'Mozilla/5.0 (Macintosh; Intel Mac OS X 10_9_3) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/35.0.1916.47 Safari/537.36'
                                    })

    try:
        page = urllib.request.urlopen(req)
    except Exception as e:
        print(f"\n{patronite_id}: {e}")
        return None

    # wczytanie strony i przerobienie na string
    page_doc = page.read().decode('utf-8')

    # parser HTMLa z BS
    soup = BeautifulSoup(page_doc, 'html.parser')

    # nazwa usera
    user_name = soup.find_all("h1")[0].getText().strip()
    if user_name == 'Patron':
        return None

    # od kiedy w Partonite
    el = soup.find_all("div", attrs={"class": "author__bio--joinDate"})[0].getText().strip()
    el = el.replace('W Patronite od ', "")
    el = time.strptime(el, "%d.%m.%Y")
    user_date = (el.tm_year, el.tm_mon, el.tm_mday)

    # ilu darczyńców, ile miesięcznie i ile łącznie?
    if soup.find("div", attrs={"class": "author__stats--wrapper"}) is None:
        return None
          
    user_numbers = []
    for i,elem in enumerate(soup.find("div", attrs={"class": "author__stats--wrapper"}).children):
        if len(elem) > 1:
            el = elem.find("span", attrs={"class": "author__stats--number"})
            l = el.getText()
            l = l.replace("zł", "")
            l = l.replace(" ", "")
            l = int(l)
            user_numbers.append(l)

    dct = {
        "id": patronite_id,
        "name": user_name,
        "registration_year": user_date[0],
        "registration_month": user_date[1],
        "registration_day": user_date[2]
    }

    for e in zip(["patrons", "dotations_per_month", "total_dotations"], user_numbers):
        dct[e[0]] = e[1]
        
    return pd.DataFrame(dct, index=[0])


today = time.mktime((time.gmtime().tm_year, time.gmtime().tm_mon, time.gmtime().tm_mday, 0,0,0, 0,0,0))

if id == 0:
    full_df = pd.DataFrame()
    id = 0
    added = 0
    print("Zaczynamy od zera")
else:
    print(f'Wczytałem plik f"data/{added}_{id}.pkl"')
    full_df = pd.read_pickle(f"data/{added}_{id}.pkl").drop_duplicates()

stop = False
saved = False

while not stop:
    id += 1
    print(f"\r{id} ", end="\r")
    temp_df = grab_page(id) 
    if temp_df is not None:
        full_df = full_df.append(temp_df)
        added += 1
        saved = False
        print(f"\nAdded {added} (after {id} iterations) user: {temp_df.name[0]}", end="\n")
        if today == time.mktime((temp_df.registration_year[0],
                                 temp_df.registration_month[0],
                                 temp_df.registration_day[0],
                                 0,0,0,
                                 0,0,0)):
            stop = True
    
    if ((id % 100 == 0) or (added % 10 == 0)) and not saved:
        full_df.to_pickle(f"data/{added}_{id}.pkl")
        print(f'\nSaved to file "data/{added}_{id}.pkl"', end='\n')
        saved = True

    time.sleep(random.choice([0.25, 0.5, 0.75, 1]))


print("\n\nFINISHED!")
full_df.drop_duplicates().to_pickle(f"data/{added}_{id}.pkl")
print(f'Saved to file "data/{added}_{id}.pkl"')
