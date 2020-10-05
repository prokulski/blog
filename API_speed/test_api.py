import random
import requests
import time
import json


N_ITER = [10, 50, 100, 250, 500, 750, 1000, 1500, 2000, 2500, 5000, 7500, 10000]
N_TIMES = 10

# API w Python/Flask
api_urls = {"Plumber": "http://localhost:8091",
			"Flask":"http://localhost:8090",
			"RestRserve":"http://localhost:8092"}



def test_alive(n_iter, api_url):
	start = time.time()
	for t in range(n_iter):
		r = requests.get(f"{api_url}/alive")
		# print(time.time(), r.status_code, r.content)


	elapsed_time = time.time() - start
	print(f"/alive, {n_iter} iteracji, {api_url}: {round(1000*elapsed_time)} ms, {round(1000*elapsed_time/n_iter, 2)} ms/iter")

	return(1000*elapsed_time)


def test_iris(n_iter, api_url):
	start = time.time()
	for t in range(n_iter):
		my_data = {"sepal_length": random.randrange(10, 50)/10,
				   "sepal_width": random.randrange(10, 50)/10,
				   "petal_length": random.randrange(10, 50)/10,
				   "petal_width": random.randrange(10, 50)/10}

		r = requests.post(f"{api_url}/iris", json=my_data)
		# print(json.dumps(my_data), r.status_code, r.content)


	elapsed_time = time.time() - start
	print(f"/iris, {n_iter} iteracji, {api_url}: {round(1000*elapsed_time)} ms, {round(1000*elapsed_time/n_iter, 2)} ms/iter")

	return(1000*elapsed_time)


def test_add(n_iter, api_url):
	start = time.time()
	for t in range(n_iter):
		a, b = 50*random.random(), 100*random.random()
		r = requests.get(f"{api_url}/add/{a}/{b}")
		# print(r.status_code, a, b, r.content)


	elapsed_time = time.time() - start
	print(f"/add, {n_iter} iteracji, {api_url}: {round(1000*elapsed_time)} ms, {round(1000*elapsed_time/n_iter, 2)} ms/iter")

	return(1000*elapsed_time)


if __name__ == "__main__":
	
	wyniki = open("wyniki.csv", "w")
	wyniki.writelines(f"method,api,iterations,elapsed_ms\n")
	
	for t in range(N_TIMES):
		print(f"\n\n{t} próba")
		
		for i in N_ITER:
			print(f"\n{i} iteracji")
			
			for k, v in api_urls.items():
				elasped = test_alive(i, v)
				wyniki.writelines(f"alive,{k},{i},{elasped}\n")
				
				if k is not "RestRserve":
					elasped = test_add(i, v)
					wyniki.writelines(f"add,{k},{i},{elasped}\n")
				
				elasped = test_iris(i, v)
				wyniki.writelines(f"iris,{k},{i},{elasped}\n")
			
			wyniki.flush()
	
	wyniki.writelines(f"KONIEC,0,0,0\n")
	wyniki.close()