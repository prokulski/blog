from sklearn.datasets import load_iris
from sklearn.ensemble import RandomForestClassifier
from sklearn.model_selection import train_test_split
from sklearn.metrics import classification_report, confusion_matrix
import pickle

iris = load_iris()

X = iris.data
y = iris.target

X_train, X_test, y_train, y_test = train_test_split(X, y, test_size=0.3)

# uczenie modelu
model_clas = RandomForestClassifier(n_estimators=100)
model_clas.fit(X_train, y_train)

# predykcja na danych testowych
y_pred = model_clas.predict(X_test)

# wyniki treningu
print(classification_report(y_test, y_pred))
print(confusion_matrix(y_test, y_pred))

# zapisanie modelu do pliku
with open("model.pickle", "wb") as f:
    pickle.dump(model_clas, f)

print("Model trained and saved.")
