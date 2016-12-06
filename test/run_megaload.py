#!/usr/bin/env python3
import requests
import pprint
import json
import time
import datetime

api = "http://admin:admin@localhost:8080"


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
			print("Error when uploading " + filename + ": " + str(response))
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

def stop_load(familyUUID):
	response = requests.post(
			url=api + '/api/megaload/stop_load/' + familyUUID,
			json={})
	if response.status_code == 200:
		print("Stopped test")
	else:
		println("Could not stop test, returned " + str(response))

# Returns current state (e.g: {"state" : "running", "id" : "test-1"})
# where state = running | stopping | stopped | finishing | finished | standby
def get_test_state(familyUUID):
	r = requests.get(api + '/api/megaload/is_running/' + familyUUID)
	return r.json()

def wait_for_test(familyUUID, nodes):
	try:
		while True:
			state = get_test_state(familyUUID)
			if state['state'] == 'finished' or state['state'] == 'stopped':
				break
			print("Executing test " + state['id'] + " (" + state['state'] + ')')
			successStatsList = get_statistics(nodes, 'global_counter_successfulRequests')
			failedStatsList = get_statistics(nodes, 'global_counter_failedRequests ')
			for node, successStats, failedStats in zip(nodes, successStatsList, failedStatsList):
				if len(successStats['data']) > 0:
					successCount = successStats['data'][-1]['value']
				else:
					successCount = 0
				if len(failedStats['data']) > 0:
					failCount = failedStats['data'][-1]['value']
				else:
					failCount = 0
				print("node " + node['name'] + " requests = " + str(successCount) + ", failures = " + str(failCount))

			time.sleep(10)
	except (KeyboardInterrupt, SystemExit):
		print("Cancelled benchmark")
		stop_load(familyUUID)

def get_nodes(familyUUID):
	r = requests.get(api + '/api/topo/node-family/' + familyUUID + '/node')
	return r.json()

def print_nodes(familyUUID):
	print("familyUUID = " + uid)
	nodes = get_nodes(familyUUID)
	for node in nodes:
		print("Node name = " + node['name'] + ", id = " + node['id'])

def time_to_string(t):
	return t.strftime("%Y-%m-%dT%H:%M:%S.000Z")

def get_statistics(nodes, statistic, start_time=0, end_time=0):
	# TODO api uses different time zone?
	if end_time == 0:
		# default end_time = now
		end_time = datetime.datetime.now()
	if start_time == 0:
		# default start time = 15 minutes before end
		start_time = end_time - datetime.timedelta(minutes=65)
	# Example call:
	# http://localhost:8080/api/metric/history/node/230c12e8-6bc9-46d4-93b3-81bd15c4b236/global_counter_successfulRequests/1?from=2016-11-25T14:34:46.000Z&to=2016-11-25T14:49:46.000Z
	# http://localhost:8080/api/metric/history/node/230c12e8-6bc9-46d4-93b3-81bd15c4b236/global_counter_successfulRequests/1?from=2016-11-25T13:38:34.792Z&to=2016-11-25T13:53:34.792Z
	# http://localhost:8080/api/metric/history/node/230c12e8-6bc9-46d4-93b3-81bd15c4b236/global_counter_successfulRequests/1?from=2016-11-25T12:55:18.735Z&to=2016-11-25T13:10:18.735Z
	result = []
	for node in nodes:
		url = (api + '/api/metric/history/node/' + node['id']
			 + '/' + statistic
			 + '/1?from=' + time_to_string(start_time)
			 + '&to=' + time_to_string(end_time))
		r = requests.get(url)
		result.append(r.json())
	return result


loaders = [
	'loader1@127.0.0.1',
	'loader2@127.0.0.1'
]
for loader in loaders:
	add_node(nodeName=loader, cookie='loader')

uid = get_family_uuid()
print_nodes(uid)

nodes = get_nodes(uid)


upload_config_file(uid, 'megaload_test.json')
upload_data_file(uid, 'patient_list.csv')
start_time = datetime.datetime.utcnow()
start_load(uid, 'fmk-test')
wait_for_test(uid, nodes)
end_time = datetime.datetime.utcnow()

statsList = get_statistics(nodes, 'global_counter_successfulRequests')
print("stats = " + str(statsList))

# TODO add metrics:
# First get nodes: http://localhost:8080/api/topo/node-family/db0312da-76e3-4c3a-97c7-07ae119c416d/node
# Then read data: http://localhost:8080/api/metric/histogram/node/3cb3d4af-2f06-4870-9e97-2263d12a4a2f/global_histogram_requestsPerSecond?from=2016-11-22T22:59:58.078Z&to=2016-11-22T23:14:58.078Z
#
