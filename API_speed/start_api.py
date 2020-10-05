from flask import Flask
from flask import request

import pickle

# nazwy klas irysów
target_names = ['setosa', 'versicolor', 'virginica']

# wczytanie modelu z pliku
with open("model.pickle", "rb") as f:
    model_clas = pickle.load(f)

# appka flaskowa
app = Flask(__name__)


@app.route('/alive', methods=['GET'])
def get_test():
    return 'I\'m alive', 200


@app.route('/iris', methods=['POST'])
def post_predict_iris_genre():
    if request.is_json:
        content = request.get_json()
        my_data = [(content['sepal_length'],
                    content['sepal_width'],
                    content['petal_length'],
                    content['petal_width'])]

        # predykcja
        my_pred = model_clas.predict(my_data)

        return target_names[int(my_pred)], 200

    return 'Error: Ask me by JSON', 503


@app.route('/add/<a>/<b>', methods=['GET'])
def get_add(a, b):
    return str(float(a)+float(b)), 200


# uruchomienie aplikacji
app.run(host='0.0.0.0', port=8090)
