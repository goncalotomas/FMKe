#!/usr/bin/env python3
import requests
import pprint
import json
import time

api = "http://admin:admin@localhost:8080"


print("Hello world")

def read_file(file):
	with open(file) as f: 
		return f.read()

# Adds a loader, e.g. nodeName = 'loader@127.0.0.1'
def add_node(nodeName, cookie):
	response = requests.post(
			url=api + '/api/topo/action/add-node', 
			json={'name': nodeName, 'cookie':cookie})
	if response.status_code == 200:
		data = response.json()
		for n in data:
			pprint.pprint(n)
		print("Added node " + nodeName)
	elif response.status_code == 400:
		print("Could not add node " + nodeName + ". Maybe it is already added?")
	else:
		raise Exception("Could not add node; " + str(response))



def get_family_uuid():
	r = requests.get(api + '/api/topo/node-family')
	data = r.json()
	# just take the id of the first node
	return data[0]['id']

def upload_config_file(familyUUID, filename):
	# do a check with pythons json decoder first, to get better error messages:
	# throws JSONDecodeError
	json.loads(read_file(filename))

	with open(filename, 'rb') as file:
		response = requests.post(
			url=api + '/api/megaload/upload_config_file/' + familyUUID, 
			files={filename: file})
		if response.status_code == 200:
			print("File " + filename + " uploaded.")
		else:
			responseData = response.json()
			error = responseData['error'] 
			if error:
				error = error.replace("\\n","\n").replace("\\t","   ")
				raise Exception(error)


def upload_data_file(familyUUID, filename):
	with open(filename, 'rb') as file:
		response = requests.post(
			url=api + '/api/megaload/upload_data_file/' + familyUUID, 
			files={filename: file})
		if response.status_code == 200:
			print("File " + filename + " uploaded.")
		else:
			responseData = response.json()
			error = responseData['error'] 
			if error:
				error = error.replace("\\n","\n").replace("\\t","   ")
				raise Exception(error)

def start_load(familyUUID, testId):
	response = requests.post(
			url=api + '/api/megaload/start_load/' + familyUUID, 
			json={'id': testId})
	if response.status_code == 200:
		print("Started test " + testId)
	else:
		raise Exception("Could not start test, returned " + str(response))

# Returns current state (e.g: {"state" : "running", "id" : "test-1"})
# where state = running | stopping | stopped | finishing | finished | standby
def get_test_state(familyUUID):
	r = requests.get(api + '/api/megaload/is_running/' + familyUUID)
	return r.json()

def wait_for_test(familyUUID):
	while True:
		state = get_test_state(familyUUID)
		if state['state'] == 'finished':
			break
		print("Executing test " + state['id'] + " (" + state['state'] + ')')
		time.sleep(1)

add_node(nodeName='loader@127.0.0.1', cookie='loader')
uid = get_family_uuid()
print("Uid = " + uid)
upload_config_file(uid, 'megaload_test.json')
upload_data_file(uid, 'patient_list.csv')
start_load(uid, 'fmk-test')
wait_for_test(uid)


# TODO add metrics: 
# First get nodes: http://localhost:8080/api/topo/node-family/db0312da-76e3-4c3a-97c7-07ae119c416d/node
# Then read data: http://localhost:8080/api/metric/histogram/node/3cb3d4af-2f06-4870-9e97-2263d12a4a2f/global_histogram_requestsPerSecond?from=2016-11-22T22:59:58.078Z&to=2016-11-22T23:14:58.078Z
# 